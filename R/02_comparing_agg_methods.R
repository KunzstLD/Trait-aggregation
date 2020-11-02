# Script that compares two trait aggregation methods
# 1) Species - Genus - Family (using Mode)
# 2) Species - Family
# TODO:
# Provide Details on aggregation methods
# Updata Github

# Methods used:
# Aggregation over genus and family (using median and mode)
# Aggregation directly to family level
# Weighted Aggregation

# _____________________________________________________________________________
#### Aggreagation ####
# 1) Species - Genus - Family (using Mode)
# - Aggregation procedure:
# Median to Genus/
# Mode for Family if exists, otherwise mean/median
# -> differing amount of values to calculate value for each modality
# sum(table(AUS_subset[grepl("Chirono.*", family)]$temp_eurytherm))

# Poff et al. 2006: most common genus-level modality assigned (genus level
# trait data with family-level monitoring data)
# which(table(AUS_subset$genus) == max(table(AUS_subset$genus), na.rm = TRUE))
# _____________________________________________________________________________

# leave the fuzzy coded NOA dataset out and investigate in separate script
agg_data <- preproc_dat[c(
  "Trait_AUS_harmonized",
  "Trait_EU_pp_harmonized",
  "Trait_NZ_pp_harmonized",
  "Traits_US_LauraT_pp_harmonized"
)]

# save
# for(i in names(agg_data)) {
#   write.csv(
#     x = agg_data[[i]],
#     file = file.path(data_out, paste0(names(agg_data[i]), ".csv")),
#     row.names = FALSE
#   )
# } 

#### Complex Aggregation ####
# aggregation over genus level (Median, Median)
data_complex_agg_median <- lapply(agg_data, function(y) {
  spec_genus_agg_alt(
    trait_data = y,
    non_trait_cols = c(
      "order",
      "family",
      "genus",
      "species"
    ),
    method = median
  ) %>%
  data.table::melt(., id.vars = c("family", "order")) %>%
  setnames(.,
    old = c("value"),
    new = c("value_genus_fam_agg_median")
  )
})

# aggregation over genus level (Median, Mean)
data_complex_agg_mean <- lapply(agg_data, function(y) {
  spec_genus_agg_alt(
    trait_data = y,
    non_trait_cols = c(
      "order",
      "family",
      "genus",
      "species"
    ),
    method = mean
  ) %>%
  data.table::melt(., id.vars = c("family", "order")) %>%
  setnames(.,
   old = c("value"),
   new = c("value_genus_fam_agg_mean")
 )
})

#### Direct aggregation ####
# Aggregation directly to family level median
data_direct_agg_median <- lapply(agg_data, function(y) {
  direct_agg(
    trait_data = y,
    non_trait_cols = c(
      "order",
      "family",
      "genus",
      "species"
    ),
    method = median
  ) %>%
  data.table::melt(., id.vars = c("family", "order")) %>%
  setnames(.,
    old = c("value"),
    new = c("value_direct_agg_median")
  )
})

# Aggregation directly to family level mean
data_direct_agg_mean <- lapply(agg_data, function(y) {
  direct_agg(
    trait_data = y,
    non_trait_cols = c(
      "order",
      "family",
      "genus",
      "species"
    ),
    method = mean
  ) %>%
  data.table::melt(., id.vars = c("family", "order")) %>%
  setnames(.,
   old = c("value"),
   new = c("value_direct_agg_mean")
 )
})

#### weighted Aggregation ####
# calculate weightes per DB
data_weighted <- lapply(agg_data, function(y) {
  weighted_agg(
    trait_data = y,
    non_trait_cols = c("order",
     "family",
     "genus",
     "species",
     "coeff")
  ) %>%
  data.table::melt(., id.vars = c("family", "order")) %>%
  setnames(.,
   old = c("value"),
   new = c("value_weighted_agg"))
})

# merge results
results_agg <-
  list(
    rbindlist(data_complex_agg_median, idcol = "database"),
    rbindlist(data_complex_agg_mean, idcol = "database"),
    rbindlist(data_direct_agg_median, idcol = "database"),
    rbindlist(data_direct_agg_mean, idcol = "database"),
    rbindlist(data_weighted, idcol = "database")
  ) %>%
  Reduce(function(x, y)
    merge(x, y, by = c("database", "family", "variable", "order")), .)

# calc differences (not all possible combinations are considered)
results_agg[, `:=`(diff_stepw_median_mean = value_genus_fam_agg_median - value_genus_fam_agg_mean, 
                   diff_direct_median_mean = value_direct_agg_median - value_direct_agg_mean, 
                   diff_stepw_median_direct_median = value_genus_fam_agg_median - value_direct_agg_median,
                   diff_stepw_median_weighted = value_genus_fam_agg_median - value_weighted_agg, 
                   diff_direct_median_weighted = value_direct_agg_median - value_weighted_agg)]

# convert to long-format
results_agg_lf <- melt(results_agg, 
                       measure.vars = c("diff_stepw_median_mean",
                                        "diff_direct_median_mean", 
                                        "diff_stepw_median_weighted", 
                                        "diff_direct_median_weighted",
                                        "diff_stepw_median_direct_median"), 
                       variable.name = "difference_vars",
                       value.name = "differences")

# remove NA differences
results_agg_lf <- results_agg_lf[!is.na(differences), ]

# create grouping feature col
results_agg_lf[, grouping_feature := sub("(\\_)(.+)", "", variable)] 


#### SDs & Mean for differing cases ####
# abs differences
results_agg_lf[, abs_differences := abs(differences)]
results_agg_lf[abs_differences != 0,
               .(abs_differences, 
                 variable,
                 family),
               by = .(database, 
                      difference_vars)] %>% 
  .[database == "Trait_AUS_harmonized" & difference_vars == "diff_stepw_median_mean" & family == "Aeshnidae", ] 


# mean & SD for each database and method comparison
diff_region_method <- results_agg_lf[abs_differences != 0,
                                     .(
                                       mean_abs_diff = mean(abs_differences),
                                       sd_abs_diff = sd(abs_differences),
                                       variable,
                                       grouping_feature
                                     ),
                                     by = .(database,
                                            difference_vars)]

diff_region_method[, difference_vars := factor(
  difference_vars,
  levels = c(
    "diff_stepw_median_weighted",
    "diff_stepw_median_direct_median",
    "diff_stepw_median_mean",
    "diff_direct_median_weighted",
    "diff_direct_median_mean"
  )
)]


region_names <- c("Trait_AUS_harmonized" = "Australia",
                  "Trait_EU_pp_harmonized" = "Europe",
                  "Trait_NZ_pp_harmonized" = "New Zealand", 
                  "Traits_US_LauraT_pp_harmonized" = "North America")

# create annotations
annotations <-
  cbind(diff_region_method[, .N, by = .(database, 
                                        difference_vars)],
        data.frame(x = rep(c(3, 5, 1, 4, 2), each = 4), y = rep(0.65, 20)))
annotations[, label := paste("N =", N)]

# plot
ggplot(diff_region_method, aes(x = difference_vars, y = mean_abs_diff))+
  geom_point(size = 2,
             color = "black") +
  geom_errorbar(
    aes(
      x = as.factor(difference_vars),
      ymin = ifelse(mean_abs_diff - sd_abs_diff >= 0, 
                    mean_abs_diff - sd_abs_diff, 
                    0),
      ymax = mean_abs_diff + sd_abs_diff
    ),
    width = 0.15,
    color = "black"
  ) +
  geom_text(
    data = annotations,
    aes(x = x,
        y = y,
        label = label),
    colour = "black",
    inherit.aes = FALSE,
    parse = FALSE
  )+
  coord_flip() +
  facet_wrap( ~ database, labeller = as_labeller(region_names))+
  labs(x = NULL, y = "Mean of absolute differences")+
  scale_x_discrete(
    labels = c(
      "Difference stepwise_agg (median) \n and weighted_agg",
      "Difference stepwise_agg (median) \n and direct_agg (median)",
      "Difference stepwise_agg (median) \n and stepwise_agg (mean)",
      "Difference direct_agg (median) \n and weighted_agg",
      "Difference direct_agg (median) \n and direct_agg (mean)"
    )
  )+
  expand_limits(y = c(0, 0.7))+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 9),
    axis.text.y = element_text(family = "Roboto Mono", size = 9),
  )
for(link in c(data_out, data_paper)) {
  ggplot2::ggsave(
    filename = file.path(link, "Comparison_trait_agg_methods.png"),
    width = 22,
    height = 12,
    units = "cm"
  )
}

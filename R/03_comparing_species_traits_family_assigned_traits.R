# _____________________________________________________________________________
# Here two trait aggregation methods are compared
# to traits assigned on family level for the Australian trait databbase.
# Reference traits on family level are from Chessman
# TODO Source for Chessman traits 
# ?For later, include respiration information:
# functional spiracles -> resp_spi
# air respiration   -> resp_spi
# gills -> resp_gil
# TODO check if there are rows with just zeros
# _____________________________________________________________________________

#### Data processing ####
chessman_raw <- read_excel(
  path = file.path(
    ".",
    "Data",
    "Chessman part 2.xlsx"
  ),
  skip = 1
)
setDT(chessman_raw)

# subset to feeding information, taxonomical information & size
chessman_raw <- chessman_raw[, .SD,
  .SDcols = names(chessman_raw) %like% "Order|Family|.*feeding|.*length"]

# Classify continuous variable length as size
chessman_raw[, `:=`(
  size_small = ifelse(`Maximum length (mm)` < 9,
    1, 0
  ),
  size_medium = ifelse(
    `Maximum length (mm)` >= 9 &
      `Maximum length (mm)` <= 16,
    1,
    0
  ),
  size_large = ifelse(`Maximum length (mm)` > 16,
    1, 0
  )
)]

# change col names so that data can be merged
# with results from other aggregations
setnames(chessman_raw,
  old = c(
    "Order",
    "Family",
    "Shredder (proportion of feeding)",
    "Scraper (proportion of feeding)",
    "Predator (proportion of feeding)",
    "Gatherer (proportion of feeding)",
    "Filterer (proportion of feeding)"
  ),
  new = c(
    "order",
    "family",
    "feed_shredder",
    "feed_herbivore",
    "feed_predator",
    "feed_gatherer",
    "feed_filter"
  )
)

# del maximum length variable
chessman_raw[, `Maximum length (mm)` := NULL]

# rm missing entries
chessman_raw <- na.omit(chessman_raw)

# transform to lf
chessman_raw <- melt(chessman_raw, id.vars = c("family", "order"))

# correct taxnomoy: "Veneroida" to "Venerida"
chessman_raw[order %in% "Veneroida", order := "Venerida"]

#### Calculate trait aggregation ####

# aggregate again using only the two grouping features feeding mode and size
preproc_AUS <- trait_dat$Trait_AUS_harmonized.rds[, .SD,
                                                  .SDcols = names(trait_dat$Trait_AUS_harmonized.rds) %like% "species|genus|family|order|feed.+|size.+"] %>%
  normalize_by_rowSum(.,
                      non_trait_cols = c("species",
                                         "genus",
                                         "family",
                                         "order")) %>%
  .[!is.na(family), ]
# save
# write.csv(
#   x = preproc_AUS,
#   file = file.path(
#     data_out,
#     "Grouping_feature_DB_harmonized_size_fmode_AUS.csv"
#   ),
#   row.names = FALSE
# )

# all families in chessman are present in preproc_AUS
unique(chessman_raw$family) %in% unique(preproc_AUS$family) %>% all

# save for analysis of taxonomic hierarchy
saveRDS(object = unique(chessman_raw$family), 
        file = file.path(data_cache, "aus_unique_families.rds"))

# compare orders -> some families have been classified differently by chessman
target_orders <- chessman_raw[!order %in% preproc_AUS$order, order] %>% unique
target_families <- chessman_raw[order %in% target_orders, family] %>% unique

# take classification preproc_AUS via merge
chessman_raw[preproc_AUS[family %in% target_families, .(family, order)],
             order := i.order,
             on = "family"]

# post analysis revealed few more families that were differently classified
chessman_raw[preproc_AUS[family %in% c(
  "Bdellidae",
  "Bithyniidae",
  "Cyzicidae",
  "Enchytraeidae",
  "Erythraeidae",
  "Hydrococcidae",
  "Pisidiidae",
  "Pomatiopsidae"
), .(family, order)],
order := i.order,
on = "family"]

# stepwise agg median ----
preproc_AUS_stepwise_median <- spec_genus_agg_alt(
  trait_data = preproc_AUS,
  non_trait_cols = c("order",
                     "family",
                     "genus",
                     "species"),
  method = median
) %>%
  data.table::melt(., id.vars = c("family", "order")) %>%
  setnames(.,
           old = c("value"),
           new = c("value.stepwise.agg.median"))
# preproc_AUS_stepwise_median[variable == "size_small" & !is.na(value.stepwise.agg.median), ]

# stepwise agg mean ----
preproc_AUS_stepwise_mean <- spec_genus_agg_alt(
  trait_data = preproc_AUS,
  non_trait_cols = c("order",
                     "family",
                     "genus",
                     "species"),
  method = mean
) %>%
  data.table::melt(., id.vars = c("family", "order")) %>%
  setnames(.,
           old = c("value"),
           new = c("value.stepwise.agg.mean"))

# direct agg median ----
preproc_AUS_median <- direct_agg(
  trait_data = preproc_AUS,
  non_trait_cols = c("order",
                     "family",
                     "genus",
                     "species"),
  method = median
) %>%
  data.table::melt(., id.vars = c("family", "order")) %>%
  setnames(.,
           old = c("value"),
           new = c("value.direct.agg.median"))

# direct agg mean 
preproc_AUS_mean <- direct_agg(
    trait_data = preproc_AUS,
    non_trait_cols = c("order",
                       "family",
                       "genus",
                       "species"),
    method = mean
  ) %>%
  data.table::melt(., id.vars = c("family", "order")) %>%
  setnames(.,
           old = c("value"),
           new = c("value.direct.agg.mean"))

# weighted agg ----
preproc_AUS_weighted <- weighted_agg(
  trait_dat = preproc_AUS,
  non_trait_cols = c("order",
                     "family",
                     "genus",
                     "species")
) %>%
  data.table::melt(., id.vars = c("family", "order")) %>%
  setnames(.,
           old = c("value"),
           new = c("value.weighted.agg"))

# merge results with trait values from Chessman together
traitval_aus <- list(chessman_raw,
                     preproc_AUS_median,
                     preproc_AUS_mean,
                     preproc_AUS_stepwise_median,
                     preproc_AUS_stepwise_mean,
                     preproc_AUS_weighted
                     ) %>%
  Reduce(function(x, y)
    merge(x, y, by = c("family", "variable", "order")), .)

setnames(traitval_aus,
         old = "value",
         new = "value.famlvl")

# create grouping feature column
traitval_aus[, grouping.feature := sub("(.+)(\\_)(.+)", "\\1", variable)]

# _____________________________________________________________________________
#### Analysis
# _____________________________________________________________________________

# calculate deviance
traitval_aus[, `:=`(
  deviance.direct.median.fam = value.direct.agg.median - value.famlvl,
  deviance.direct.mean.fam = value.direct.agg.mean - value.famlvl,
  deviance.stepwise.median.fam = value.stepwise.agg.median - value.famlvl,
  deviance.stepwise.mean.fam = value.stepwise.agg.mean - value.famlvl, 
  deviance.weighted.fam = value.weighted.agg - value.famlvl
)]

# overview over deviations
traitval_aus_lf <- melt(
  traitval_aus,
  measure.vars = c(
    "deviance.direct.median.fam",
    "deviance.direct.mean.fam",
    "deviance.stepwise.median.fam",
    "deviance.stepwise.mean.fam",
    "deviance.weighted.fam"
  ),
  variable.name = "deviance.vars"
)

# create subset only with cases with different affinity scores
traitval_aus_lf_diff <- traitval_aus_lf[value != 0, ]
traitval_aus_lf_diff[, `:=`(
  abs.value = abs(value),
  mean.abs.value = mean(abs(value)),
  sd.abs.value = sd(abs(value))
),
by = c("deviance.vars", "grouping.feature")]
traitval_aus_lf_diff[, deviance.vars := factor(deviance.vars, 
                              levels = c("deviance.weighted.fam", 
                                         "deviance.stepwise.mean.fam",
                                         "deviance.stepwise.median.fam",
                                         "deviance.direct.mean.fam",
                                         "deviance.direct.median.fam"))]

# prepare annotations & panel names
grouping.feature_names_aus <- c("feed" = "Feeding mode",
                               "size" = "Body size")

annotations_aus <-
  cbind(traitval_aus_lf_diff[, .N, by = c("deviance.vars", "grouping.feature")],
        data.frame(x = rep(5:1, each = 2), y = rep(1.2, 10)))
annotations_aus[, label := paste("n =", N)]

# Overview plot differences ----
plot_aggr_assig_aus <- traitval_aus_lf_diff %>%
  ggplot(., aes(x = as.factor(deviance.vars), y = abs.value)) +
  geom_violin(color = "gray45", 
              draw_quantiles = c(0.25, 0.5, 0.75),
              linetype = "solid") +
  geom_jitter(
    size = 2,
    width = 0.05,
    alpha = 0.2,
  ) +
  stat_summary(fun = mean, 
               geom = "point",
               size = 5,
               color = "dodgerblue4")+
  geom_errorbar(
    aes(
      x = as.factor(deviance.vars),
      ymin = mean.abs.value - sd.abs.value,
      ymax = mean.abs.value + sd.abs.value
    ),
    width = 0.3,
    color = "dodgerblue4",
  ) +
  scale_color_uchicago() +
  coord_flip() +
  geom_text(
    data = annotations_aus,
    aes(x = x,
        y = y,
        label = label),
    colour = "black",
    inherit.aes = FALSE,
    parse = FALSE
  ) +
  facet_wrap( ~ grouping.feature, labeller = as_labeller(grouping.feature_names_aus)) +
  labs(x = "Aggregation method",
       y = "Absolute difference") +
  ggtitle("AUS") +
  scale_x_discrete(
    labels = c(
      "Weighted_agg",
      "Stepwise_agg \n (mean)",
      "Stepwise_agg \n (median)",
      "Direct_agg \n (mean)",
      "Direct_agg \n (median)" 
    )
  ) +
  expand_limits(y = c(0, 1.3))+
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 11),
    axis.text.y = element_text(family = "Roboto Mono", size = 11),
    panel.grid = element_blank(),
    strip.text = element_text(family = "Roboto Mono", size = 11)
  ) 

#### Maximum differences ####

# Max differences AUS
# summary variables
traitval_aus_lf_diff[abs.value == 1, summary(variable)]

# How often deviations?  
traitval_aus_lf_diff[abs.value > 0.1, .N, by = variable] %>% 
  .[order(-N), ] 

# How many cases with maximum difference 
traitval_aus_lf_diff[abs.value == 1, .N]/traitval_aus_lf_diff[, .N] * 100

# How many for size_large and size_medium
traitval_aus_lf_diff[abs.value == 1 & variable == "size_medium", .N]/traitval_aus_lf_diff[abs.value == 1, .N]
traitval_aus_lf_diff[abs.value == 1 & variable == "size_large", .N]/traitval_aus_lf_diff[abs.value == 1, .N]

traitval_aus_lf_diff[abs.value == 1 & variable == "size_medium",]

# Mean and SD for all investigated traits
traitval_aus_lf_diff[, .(mean_abs_by_var = mean(abs.value), 
                         sd_abs_by_var = sd(abs.value)), 
                     by = variable] %>% 
  .[order(mean_abs_by_var), ]

# feeding mode
traitval_aus_lf_diff[abs.value == 1 &
                       variable %in% c("feed_gatherer",
                                       "feed_shredder"),]

#### Traits where no differences occurred - most consistent traits ####
traitval_aus_lf[value == 0, .N, by = variable] %>% 
  .[order(-N)]

#### Overall stats ####

# Overall mean for differing cases per aggregation methods 
traitval_aus_lf_diff[, .(
  mean.abs.value.overall = mean(abs.value),
  sd.abs.value.overall = sd(abs.value)
),
by = "deviance.vars"]

# How many cases overall have been evaluated differently by Chessman?
# range of deviations, mean, sd, min, max
aus_result_tbl <- traitval_aus_lf_diff[, .(
  dev_cases = .N / traitval_aus[, .N] * 100,
  min_diff_affinity = min(abs.value),
  max_diff_affinity = max(abs.value),
  mean_diff_affinities = mean(abs.value),
  sd_diff_affinities = sd(abs.value)
),
by = "deviance.vars"] 

xtable_wo_rownames(aus_result_tbl,
                   caption = "",
                   digits = 2)

# summary per order 
# is this interesting?
traitval_gt <- tbl_summary(
  data = traitval_aus_lf_diff[, .(order,
                                  grouping.feature,
                                  deviance.vars,
                                  value, 
                                  abs.value)],
  statistic = list(all_continuous() ~ "{mean} ({sd})"),
  by = "deviance.vars",
  label = list(value ~ "Mean deviances",
               abs.value ~ "Mean abs. deviances"),
  digits = list(value ~ c(3, 2),
                abs.value ~ c(3, 2))
) %>%
  italicize_levels() 

# latex output
xtable_wo_rownames(traitval_gt$table_body[, c("label", "stat_1", "stat_2")])

# Which traits mostly differently classified? ----
traitval_aus_lf_diff[, .(.N, family), by = "variable"] %>% 
  .[order(-N),] %>% View()

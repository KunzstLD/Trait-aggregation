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

# Classify continuous varibale length as size
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

# calculate aggregation again for subset of traits
# aggregate again using only the two grouping features feeding mode and size
preproc_AUS <- trait_dat$Trait_AUS_harmonized.rds[, .SD,
                                                  .SDcols = names(trait_dat$Trait_AUS_harmonized.rds) %like% "species|genus|family|order|feed.+|size.+"] %>%
  normalize_by_rowSum(.,
                      non_trait_cols = c("species",
                                         "genus",
                                         "family",
                                         "order")) %>%
  .[!is.na(family),]

# save
# write.csv(
#   x = preproc_AUS,
#   file = file.path(
#     data_out,
#     "Grouping_feature_DB_harmonized_size_fmode_AUS.csv"
#   ),
#   row.names = FALSE
# )

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

# orders
traitval_aus$order %>% table()

# check overlap between aggregated results and chessman
unique(chessman_raw$family) %in% unique(preproc_AUS_median$family) %>%
  sum()
unique(chessman_raw$order)

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

grouping.feature_names_aus = c("feed" = "Feeding mode",
                               "size" = "Body size")

annotations_aus <-
  cbind(traitval_aus_lf_diff[, .N, by = c("deviance.vars", "grouping.feature")],
        data.frame(x = rep(1:5, each = 2), y = rep(1.2, 10)))
annotations_aus[, label := paste("N =", N)]

# Overview plot differences ----
traitval_aus_lf_diff %>%
  ggplot(., aes(x = as.factor(deviance.vars), y = abs.value)) +
  geom_violin(color = "gray") +
  #  geom_boxplot(alpha = 0.7)+
  geom_jitter(
    size = 1.5,
    width = 0.05,
    alpha = 0.6,
    aes(color = grouping.feature)
  ) +
  geom_point(aes(x = as.factor(deviance.vars), y = mean.abs.value),
             size = 3.5,
             color = "black") +
  geom_errorbar(
    aes(
      x = as.factor(deviance.vars),
      ymin = mean.abs.value - sd.abs.value,
      ymax = mean.abs.value + sd.abs.value
    ),
    width = 0.15,
    color = "black"
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
  labs(x = NULL, y = "Absolute difference") +
  scale_x_discrete(
    labels = c(
      "Difference direct_agg (median) \n and traits assigned at family-level",
      "Difference direct_agg (mean) \n and traits assigned at family-level",
      "Difference stepwise_agg (median) \n and traits assigned at family-level",
      "Difference stepwise_agg (mean) \n and traits assigned at family-level",
      "Difference weighted_agg \n and traits assigned at family-level"
    )
  ) +
  expand_limits(y = c(0, 1.3))+
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 11),
    axis.text.y = element_text(family = "Roboto Mono", size = 11),
    panel.grid = element_blank()
  ) 
ggplot2::ggsave(filename = file.path(data_out, "Deviances_trait_agg_chessman.png"),
                width = 22,
                height = 12,
                units = "cm")

# Overall mean for differing cases per aggregation methods ----
traitval_aus_lf_diff[, .(
  mean.abs.value.overall = mean(abs.value),
  sd.abs.value.overall = sd(abs.value)
),
by = "deviance.vars"]

# How many cases overall have been evaluated differently by Chessman? ----
traitval_aus_lf_diff[, .N/traitval_aus[, .N], by = "deviance.vars"]

# range of deviations ----
traitval_aus_lf[, range(value), by = "deviance.vars"]

# xtable_wo_rownames(traitval_aus[deviance.median.fam <= -0.6 |
#                                   deviance.median.fam >= 0.6,
#                                 .(
#                                   family,
#                                   order,
#                                   variable,
#                                   value.famlvl,
#                                   value.direct.agg.median,
#                                   value.direct.agg.mean
#                                 )],
#                    digits = 2)

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

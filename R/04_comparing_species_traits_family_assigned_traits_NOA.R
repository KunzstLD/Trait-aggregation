# _____________________________________________________________________________
# Here two trait aggregation methods are compared
# to traits assigned on family level for the North American trait databbase
# TODO check if there are rows with just zeros
# North American trait data from Matt Pyne (?)
# TODO: Source
# _____________________________________________________________________________

#### Data preprocessing ####
noa_trait_matrix <-
  read_excel(
    path = file.path(".", "Data", "American Trait Matrix_upgraded(2014).xlsx"),
    sheet = 2
  ) %>%
  setDT(.) %>%
  .[, -("...1")] %>%
  .[, .(Order, Family, Volt, Size, Habt, Trop, Resp)]

# check traits per grouping feature
# describe(noa_trait_matrix)

# rm NAs
noa_trait_matrix <- na.omit(noa_trait_matrix)

# convert categories to presence-absence of trait
noa_trait_matrix_lf <-
  melt(noa_trait_matrix, id.vars = c("Family", "Order"))

noa_trait_matrix <- dcast(
  noa_trait_matrix_lf,
  Order + Family ~ value,
  value.var = "value",
  fun.aggregate = length
)

# change col names
setnames(
  noa_trait_matrix,
  old = c(
    "Habt.Burrow",
    "Habt.Climb",
    "Habt.Cling",
    "Habt.Skate",
    "Habt.Sprawl",
    "Habt.Swim",
    "Resp.Air",
    "Resp.Gills",
    "Resp.Tegument",
    "Size.Large",
    "Size.Medium",
    "Size.Small",
    "Trop.CollectorFilterer",
    "Trop.CollectorGatherer",
    "Trop.Herbivore",
    "Trop.Predator",
    "Trop.Shredder",
    "Volt.MultiVoltine",
    "Volt.SemiVoltine",
    "Volt.UniVoltine",
    "Family",
    "Order"
  ),
  new = c(
    "locom_burrow",
    "locom_climb",
    "locom_cling",
    "locom_skate",
    "locom_sprawl",
    "locom_swim",
    "resp_pls_spi",
    "resp_gil",
    "resp_teg",
    "size_large",
    "size_medium",
    "size_small",
    "feed_filter",
    "feed_gatherer",
    "feed_herbivore",
    "feed_predator",
    "feed_shredder",
    "volt_bi_multi",
    "volt_semi",
    "volt_uni",
    "family",
    "order"
  )
)

# harmonize locomotion so that this grouping features matches with the harmonized trait set
# sessil does not exist as category
# (values are mutual exclusive 0's or 1's, max and sum will result in the same outcome)
noa_trait_matrix[, locom_swim := apply(.SD, 1, max),
  .SDcols = c("locom_swim", "locom_skate")
]
noa_trait_matrix[, locom_crawl := apply(.SD, 1, max),
  .SDcols = c("locom_sprawl", "locom_climb", "locom_cling")
]

# rm unnecessary cols
noa_trait_matrix[, c("locom_skate", "locom_sprawl", "locom_climb", "locom_cling") := NULL]

# noarmalize for meaningful comparison
normalize_by_rowSum(
  x = noa_trait_matrix,
  non_trait_cols = c("order", "family")
)

# convert back to lf
noa_trait_matrix <-
  melt(noa_trait_matrix, id.vars = c("order", "family"))

#### Calculate Aggregation for NOA subset data ####
preproc_NOA <- trait_dat$Traits_US_LauraT_pp_harmonized.rds[, .SD,
  .SDcols =
    names(trait_dat$Traits_US_LauraT_pp_harmonized.rds)
    %like% "species|genus|family|order|feed.+|size.+|resp.+|volt.+|locom.+"
] %>%
  normalize_by_rowSum(.,
    non_trait_cols = c(
      "species",
      "genus",
      "family",
      "order"
    )
  ) %>%
  .[!is.na(family), ]

# save
# write.csv(
#   x = preproc_NOA,
#   file = file.path(
#     data_out,
#     "Grouping_feature_DB_harmonized_size_fmode_NOA.csv"
#   ),
#   row.names = FALSE
# )

# stepwise agg median ----
preproc_NOA_stepwise_median <- spec_genus_agg_alt(
  trait_data = preproc_NOA,
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
    new = c("value.stepwise.agg.median")
  )

# stepwise agg mean ----
preproc_NOA_stepwise_mean <- spec_genus_agg_alt(
  trait_data = preproc_NOA,
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
    new = c("value.stepwise.agg.mean")
  )

# direct agg median ----
preproc_NOA_median <- direct_agg(
  trait_data = preproc_NOA,
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
    new = c("value.direct.agg.median")
  )

# direct agg mean ----
preproc_NOA_mean <- direct_agg(
  trait_data = preproc_NOA,
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
    new = c("value.direct.agg.mean")
  )

# weighted agg ----
preproc_NOA_weighted <- weighted_agg(
  trait_dat = preproc_NOA,
  non_trait_cols = c(
    "order",
    "family",
    "genus",
    "species"
  )
) %>%
  data.table::melt(., id.vars = c("family", "order")) %>%
  setnames(.,
    old = c("value"),
    new = c("value.weighted.agg")
  )

# merge results form aggregation together
traitval_noa <- list(
  noa_trait_matrix,
  preproc_NOA_median,
  preproc_NOA_mean,
  preproc_NOA_stepwise_median,
  preproc_NOA_stepwise_mean,
  preproc_NOA_weighted
) %>%
  Reduce(function(x, y) {
    merge(x, y, by = c("family", "variable", "order"))
  }, .)

setnames(traitval_noa,
  old = "value",
  new = "value.famlvl"
)

# create grouping feature column
traitval_noa[, grouping.feature := sub("(\\_)(.+)", "", variable)]

# orders
traitval_noa$order %>% table()

# check overlap between aggregated results and pyne
unique(noa_trait_matrix$family) %in% unique(preproc_NOA$family) %>%
  sum()

# save for later analysis
saveRDS(object = unique(noa_trait_matrix$family), 
        file = file.path(data_cache, "noa_unique_families.rds"))

# _______________________________________________________________________
#### Analysis ####
# _______________________________________________________________________

# calc deviances to family assignments
traitval_noa[, `:=`(
  deviance.direct.median.fam = value.direct.agg.median - value.famlvl,
  deviance.direct.mean.fam = value.direct.agg.mean - value.famlvl,
  deviance.stepwise.median.fam = value.stepwise.agg.median - value.famlvl,
  deviance.stepwise.mean.fam = value.stepwise.agg.mean - value.famlvl,
  deviance.weighted.fam = value.weighted.agg - value.famlvl
)]

# overview over deviations
traitval_noa_lf <- melt(
  traitval_noa,
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
traitval_noa_lf_diff <- traitval_noa_lf[value != 0, ]
traitval_noa_lf_diff[, `:=`(
  abs.value = abs(value),
  mean.abs.value = mean(abs(value)),
  sd.abs.value = sd(abs(value))
),
by = c("deviance.vars", "grouping.feature")
]
traitval_noa_lf_diff[, deviance.vars := factor(deviance.vars, 
                                               levels = c("deviance.weighted.fam", 
                                                          "deviance.stepwise.mean.fam",
                                                          "deviance.stepwise.median.fam",
                                                          "deviance.direct.mean.fam",
                                                          "deviance.direct.median.fam"))]

# Overview plot differences ----
grouping.feature_names <- c(
  "feed" = "Feeding mode",
  "locom" = "Locomotion",
  "resp" = "Respiration",
  "size" = "Body size",
  "volt" = "Voltinism"
)

annotations <-
  cbind(
    traitval_noa_lf_diff[, .N, by = c("deviance.vars", "grouping.feature")],
    data.frame(x = rep(5:1, each = 5), y = rep(1.25, 25))
  )
annotations[, label := paste("n =", N)]

# order of grouping features same as in the AUS comparison 
traitval_noa_lf_diff[, grouping.feature := factor(grouping.feature,
  levels = c("feed", "size", "resp", "locom", "volt")
)]

# plot
plot_aggr_assig_noa <- traitval_noa_lf_diff[grouping.feature %in% c("feed", "size"), ] %>%
  ggplot(., aes(x = as.factor(deviance.vars), y = abs.value)) +
  geom_violin(
    color = "gray45",
    draw_quantiles = c(0.25, 0.5, 0.75)
  ) +
  geom_jitter(
    size = 1.5,
    width = 0.1,
    alpha = 0.1,
  #  aes(color = grouping.feature)
  ) +
  stat_summary(fun = mean, 
               geom = "point",
               size = 3.5,
               color = "dodgerblue4")+
  geom_errorbar(
    aes(
      x = as.factor(deviance.vars),
      ymin = mean.abs.value - sd.abs.value,
      ymax = mean.abs.value + sd.abs.value
    ),
    size = 0.7, 
    width = 0.3,
    color = "dodgerblue4"
  ) +
  scale_color_uchicago() +
  coord_flip() +
  facet_wrap(~grouping.feature,
    labeller = as_labeller(grouping.feature_names[c("feed", "size")])
  ) +
  geom_text(
    data = annotations[grouping.feature %in% c("feed", "size"), ], aes(
      x = x,
      y = y,
      label = label
    ),
    colour = "black",
    inherit.aes = FALSE,
    parse = FALSE
  ) +
  labs(x = "Aggergation method", 
       y = "Absolute difference")+
  ggtitle("NA") +
  scale_x_discrete(
    labels = c(
      "Weighted_agg",
      "Stepwise_agg \n (mean)",
      "Stepwise_agg \n (median)",
      "Direct_agg \n (mean)",
      "Direct_agg \n (median)"
    )
  ) +
  expand_limits(y = c(0, 1.35)) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 11),
    axis.text.y = element_text(family = "Roboto Mono", size = 11),
    # legend.title = element_text(size = 12),
    # legend.text = element_text(size = 11),
    panel.grid = element_blank(),
    strip.text = element_text(family = "Roboto Mono", size = 11)
  )

# combine plot with plot from AUS
pl_combined <- plot_aggr_assig_aus/plot_aggr_assig_noa
for (link in c(data_out, data_paper)) {
  ggplot2::ggsave(
    filename = file.path(link, "Deviances_trait_agg_combined.pdf"),
    plot = pl_combined,
    width = 24,
    height = 17,
    units = "cm",
    device = cairo_pdf, 
    dpi = 400
  )
}

# Create plot for SI with the remaining grouping features
traitval_noa_lf_diff[grouping.feature %in% c("locom", "resp", "volt"), ] %>%
  ggplot(., aes(x = as.factor(deviance.vars), y = abs.value)) +
  geom_violin(
    color = "gray45",
    draw_quantiles = c(0.25, 0.5, 0.75)
  ) +
  geom_jitter(
    size = 1.5,
    width = 0.1,
    alpha = 0.1,
    #  aes(color = grouping.feature)
  ) +
  stat_summary(fun = mean, 
               geom = "point",
               size = 3.5,
               color = "dodgerblue4")+
  geom_errorbar(
    aes(
      x = as.factor(deviance.vars),
      ymin = mean.abs.value - sd.abs.value,
      ymax = mean.abs.value + sd.abs.value
    ),
    size = 0.7, 
    width = 0.3,
    color = "dodgerblue4"
  ) +
  scale_color_uchicago() +
  coord_flip() +
  facet_wrap(~grouping.feature,
             labeller = as_labeller(grouping.feature_names[c("locom", "resp", "volt")])
  ) +
  geom_text(
    data = annotations[grouping.feature %in% c("locom", "resp", "volt"), ], aes(
      x = x,
      y = y,
      label = label
    ),
    colour = "black",
    inherit.aes = FALSE,
    parse = FALSE
  ) +
  labs(x = "Aggergation method", 
       y = "Absolute difference")+
  ggtitle("NA") +
  scale_x_discrete(
    labels = c(
      "Weighted_agg",
      "Stepwise_agg \n (mean)",
      "Stepwise_agg \n (median)",
      "Direct_agg \n (mean)",
      "Direct_agg \n (median)"
    )
  ) +
  expand_limits(y = c(0, 1.35)) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 11),
    axis.text.y = element_text(family = "Roboto Mono", size = 11),
    # legend.title = element_text(size = 12),
    # legend.text = element_text(size = 11),
    panel.grid = element_blank(),
    strip.text = element_text(family = "Roboto Mono", size = 11)
  )
for (link in c(data_out, data_paper)) {
  ggplot2::ggsave(
    filename = file.path(link, "Deviances_trait_agg_pyne.png"),
    width = 25,
    height = 13,
    units = "cm"
  )
}

#### Maximum differences ####

# Max differences NOA
traitval_noa_lf_diff[abs.value == 1, summary(variable)] %>% 
  .[order(-.)]

# How many cases with maximum difference NOA
traitval_noa_lf_diff[abs.value == 1, .N]/traitval_noa_lf_diff[, .N] * 100

# size medium
traitval_noa_lf_diff[abs.value == 1 & variable == "size_medium", .N] /
  traitval_noa_lf_diff[abs.value == 1, .N] * 100

# resp_pls_spi
traitval_noa_lf_diff[abs.value == 1 & variable == "resp_pls_spi", .N] /
  traitval_noa_lf_diff[abs.value == 1, .N] * 100

# Mean and SD for all investigated traits
traitval_noa_lf_diff[, .(mean_abs_by_var = mean(abs.value),
                         sd_abs_by_var = sd(abs.value)),
                     by = variable] %>%
  .[order(mean_abs_by_var),]

# How often?
traitval_noa_lf_diff[, .N, by = variable] %>% 
  .[order(-N), ] 

#### Minimum differences - most "concsistent" traits ####
traitval_noa_lf[value == 0, .N, by = variable] %>% 
  .[order(-N)]



#### Overall stats ####
# How many cases overall have been evaluated differently by Pyne?
# range of deviations
# (Table 6 in paper)
noa_result_tbl <- traitval_noa_lf_diff[, .(
  dev_cases = .N / traitval_noa[, .N] * 100,
  min_diff_affinity = min(abs.value),
  max_diff_affinity = max(abs.value),
  mean_diff_affinities = mean(abs.value),
  sd_diff_affinities = sd(abs.value)
),
by = "deviance.vars"
]

# combine with table from aus (see 03_comparing_species_traits_family_assigned_traits.R script)
final_tbl <- rbindlist(list(
  "Australia" = aus_result_tbl,
  "North America" = noa_result_tbl
),
idcol = "database"
)

xtable_wo_rownames(final_tbl,
  caption = "",
  digits = 2
)

# gt summary output
# traitval_gt <- tbl_summary(
#   data = traitval_noa_lf_diff[, -c(
#     "family",
#     "variable",
#     "grouping.feature",
#     "value.direct.agg.median",
#     "value.direct.agg.mean",
#     "value.stepwise.agg.median",
#     "value.stepwise.agg.mean",
#     "value.weighted.agg",
#     "value.famlvl"
#   )],
#   statistic = list(all_continuous() ~ "{mean} ({sd})"),
#   by = "deviance.vars",
#   label = list(
#     value ~ "Mean deviances",
#     abs.value ~ "Mean abs. deviances"
#   ),
#   digits = list(
#     value ~ c(3, 2),
#     abs.value ~ c(3, 2)
#   )
# ) %>%
#   italicize_levels()
# 
# # latex output
# xtable_wo_rownames(traitval_gt$table_body[, c("label", "stat_1", "stat_2")])
# 
# # ?
# plot_list <- list()
# for (grf in unique(traitval_summary_noa$grouping_feature)) {
#   # range of deviances?
#   plot_list[[grf]] <- traitval_summary_noa[grouping_feature %in% grf, ] %>%
#     ggplot(., aes(x = order, y = deviance_dir_fam)) +
#     geom_point(size = 2) +
#     geom_jitter(
#       size = 1.5,
#       height = 0,
#       width = 0.15
#     ) +
#     geom_boxplot(alpha = 0.7) +
#     ylim(-1, 1) +
#     geom_hline(yintercept = 0, linetype = "dashed") +
#     labs(
#       x = "Order",
#       y = "Deviance in affinity"
#     ) +
#     coord_flip() +
#     facet_wrap(~variable) +
#     theme_light(base_size = 15) + # ,base_family = "Poppins"
#     theme(
#       legend.position = "none",
#       axis.title = element_text(size = 12),
#       axis.text.x = element_text(family = "Roboto Mono", size = 10),
#       axis.text.y = element_text(family = "Roboto Mono", size = 10)
#     )
# 
#   # save
#   ggplot2::ggsave(
#     filename = file.path(
#       data_out,
#       paste0(
#         "Deviances_dir_median_noa_",
#         names(plot_list[grf]), ".png"
#       )
#     ),
#     width = 22,
#     height = 12,
#     units = "cm",
#     dpi = 800
#   )
# }


# Which traits?

# traits which most often deviated
traitval_noa
traitval_noa[deviance_dir_fam == 0, .N, by = "grouping_feature"] %>%
  .[order(-N), ]

# highest deviation of 1 or -1
traitval_noa[deviance_dir_fam == 1 | deviance_dir_fam == -1, unique(grouping_feature)]

total <- traitval_noa[, .N, by = "variable"] %>% .[, N]
traitval_noa[deviance_dir_fam != 0, .(.N),
  by = c("variable")
] %>%
  .[order(-N), N / total]

# Latex output:
# direct_fam
traitval_noa[deviance_dir_fam != 0, .(.N),
  by = c("variable")
] %>%
  .[order(-N), .(
    Trait = variable,
    `Families differently evaluated [%]` =
      round(N / total * 100, digits = 2)
  )] %>%
  xtable_wo_rownames(
    .,
    caption = "Percentage of families differently evaluated by
    direct aggreation and Pyne et al.
    for all traits were deviating evaluations
    occurred.",
    label = "tab:SI_perc_dir_agg_expert_NOA"
  )

# comp_fam
traitval_noa[deviance_comp_fam != 0, .(.N),
  by = c("variable")
] %>%
  .[order(-N), .(
    Trait = variable,
    `Families differently evaluated [%]` =
      round(N / total * 100, digits = 2)
  )] %>%
  xtable_wo_rownames(
    .,
    caption = "Percentage of families differently evaluated by
    stepwise aggreation and Pyne et al. for all traits were deviating evaluations
    occurred.",
    label = "tab:SI_perc_stepwise_agg_expert_NOA"
  )

#### Which orders?
# Ephmeroptera, Diptera
total <- traitval_noa[, .N, by = c("order")] %>% .[order(-N), ]

# latex output
traitval_noa[deviance_dir_fam != 0, .(variable, .N), by = "order"] %>%
  merge(x = ., y = total, by = "order") %>%
  .[, .(
    Order = order,
    `Families differently evaluated direct_agg [%]` = round((N.x / N.y) * 100, digits = 2)
  )] %>%
  .[!duplicated(Order), ] %>%
  .[order(-`Families differently evaluated direct_agg [%]`), ] %>%
  xtable_wo_rownames(.,
    caption = "Percentage of families differently evaluated
                     by  and Pyne et al. for all compared orders",
    label = "tab:SI_perc_dir_agg_expert_family_NOA"
  )

# stepwise agg latex output
traitval_noa[deviance_comp_fam != 0, .(variable, .N), by = "order"] %>%
  merge(x = ., y = total, by = "order") %>%
  .[, .(
    Order = order,
    `Families differently evaluated stepwise_agg [%]` = round((N.x /
      N.y) * 100, digits = 2)
  )] %>%
  .[!duplicated(Order), ] %>%
  .[order(-`Families differently evaluated stepwise_agg [%]`), ] %>%
  xtable_wo_rownames(.,
    caption = "Percentage of families differently evaluated
                     by  and Pyne et al. for all compared orders",
    label = "tab:SI_perc_stepwise_agg_expert_family_NOA"
  )

# SD in deviance dir_fam
traitval_noa[, sd_dir_fam := sd(deviance_dir_fam), by = c("order", "variable")]
traitval_noa[order(-sd_dir_fam), .(
  family,
  order,
  variable,
  sd_dir_fam,
  value_direct_agg,
  value_famlvl
)] %>% View()

# Regarding deviating classification (trait_val compl_agg > fam_assignment)
# and vice versa: no tendency, almost equal
traitval_noa[deviance_comp_fam > 0, .(.N, variable), by = "order"] %>%
  .[order(-N), ]
traitval_noa[deviance_comp_fam < 0, .N, by = "order"] %>%
  .[order(-N), ]

#### How big are the deviances actually?
# direct aggreation with at family-level assigned traits
total <- traitval_noa[deviance_dir_fam != 0, .N]

# dev 0.25
traitval_noa[deviance_dir_fam != 0, .(
  family,
  order,
  variable,
  value_direct_agg,
  value_famlvl,
  deviance_dir_fam
)] %>%
  .[abs(deviance_dir_fam) <= 0.25, ]


# dev. 0.5
traitval_noa[deviance_dir_fam != 0, .(
  family,
  order,
  variable,
  value_direct_agg,
  value_famlvl,
  deviance_dir_fam
)] %>%
  .[abs(deviance_dir_fam) %between% c(0.26, 0.5), .N / total]
traitval_noa[deviance_dir_fam != 0, .(
  family,
  order,
  variable,
  value_direct_agg,
  value_famlvl,
  deviance_dir_fam
)] %>%
  .[abs(deviance_dir_fam) %between% c(0.26, 0.5), ]


# dev 0.75
traitval_noa[deviance_dir_fam != 0, .(
  family,
  order,
  variable,
  value_direct_agg,
  value_famlvl,
  deviance_dir_fam
)] %>%
  .[abs(deviance_dir_fam) == 0.75, ]

# dev 1
traitval_noa[deviance_dir_fam != 0, .(
  family,
  order,
  variable,
  grouping_feature,
  value_direct_agg,
  value_famlvl,
  deviance_dir_fam
)] %>%
  .[abs(deviance_dir_fam) == 1, ] %>%
  Hmisc::describe()

# stepwise aggregation with at family-level assigned traits
total_comp_agg <- traitval_noa[deviance_comp_fam != 0, .N]

traitval_noa[deviance_comp_fam != 0, .(
  family,
  order,
  variable,
  grouping_feature,
  value_direct_agg,
  value_famlvl,
  deviance_comp_fam
)] %>%
  .[abs(deviance_comp_fam) == 1, ] %>%
  Hmisc::describe()

# How many taxa/cases are classified differntly per order?
nrow(traitval_noa[deviance_comp_fam != 0 &
  order %in% "Ephemeroptera", ]) / nrow(traitval_noa[order %in% "Ephemeroptera", ])
nrow(traitval_noa[deviance_comp_fam != 0 &
  order %in% "Diptera", ]) / nrow(traitval_noa[order %in% "Diptera", ])
nrow(traitval_noa[deviance_comp_fam != 0 &
  order %in% "Trichoptera", ]) / nrow(traitval_noa[order %in% "Trichoptera", ])
nrow(traitval_noa[deviance_comp_fam != 0 &
  order %in% "Coleoptera", ]) / nrow(traitval_noa[order %in% "Coleoptera", ])

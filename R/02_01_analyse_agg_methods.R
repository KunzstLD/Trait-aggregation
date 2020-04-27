# _____________________________________________________________________________
#### Analysis aggregation procedures ####
# _____________________________________________________________________________

# _____________________________________________________________________________
#### How many taxa end up with different trait values after aggregation?(%) ####
# _____________________________________________________________________________

#### Complex and direct aggregation ####
taxa_diff <- lapply(results_agg, function(y) {
  y[, .(deviance, total = .N)] %>%
    .[deviance != 0, .(.N / total * 100,
                       total)] %>%
    .[!duplicated(total), ]
}) %>% rbindlist(., idcol = "file")

setnames(taxa_diff, 
         old = c("file", 
                 "V1",
                 "total"), 
         new = c("Database",
                 "Deviating cases [%]",
                 "Number of cases"))
taxa_diff[, `Deviating cases [%]` := round(`Deviating cases [%]`, digits = 2)]
taxa_diff$Database <- c("Australia", "Europe", "New Zealand", "North America")

# row order (to be in accordance with other tables)
taxa_diff <- taxa_diff[match(c("Europe", "North America",
                               "Australia", "New Zealand"),
                             Database),]

# latex output
xtable_wo_rownames(x = taxa_diff, auto = TRUE)




#### Median and mean direct aggregation ####
taxa_diff_meanComp <- lapply(results_agg_means, function(y) {
  y[, .(deviance, total = .N)] %>%
    .[deviance != 0, .(.N / total * 100,
                       total)] %>%
    .[!duplicated(total), ]
}) %>%
  rbindlist(., idcol = "file")

setnames(taxa_diff_meanComp, 
         old = c("file", 
                 "V1",
                 "total"), 
         new = c("Database",
                 "Deviating cases [%]",
                 "Number of cases"))
taxa_diff_meanComp[, `Deviating cases [%]` := round(`Deviating cases [%]`, digits = 2)]
taxa_diff_meanComp$Database <- c("Australia", "Europe", "New Zealand", "North America")

# row order (to be in accordance with other tables)
taxa_diff_meanComp <-
  taxa_diff_meanComp[match(c("Europe", "North America",
                             "Australia", "New Zealand"),
                           Database), ]

# latex output
xtable_wo_rownames(x = taxa_diff_meanComp, auto = TRUE)


#### Direct aggregation (median) and weighted aggregation ####
taxa_diff_dir_weighted <-
  lapply(results_agg_dir_weighted, function(y) {
    y[, .(deviance, total = .N)] %>%
      .[deviance != 0, .(.N / total * 100,
                         total)] %>%
      .[!duplicated(total),]
  }) %>%
  rbindlist(., idcol = "file")

setnames(taxa_diff_dir_weighted, 
         old = c("file", 
                 "V1",
                 "total"), 
         new = c("Database",
                 "Deviating cases [%]",
                 "Number of cases"))
taxa_diff_dir_weighted[, `Deviating cases [%]` := round(`Deviating cases [%]`, digits = 2)]
taxa_diff_dir_weighted$Database <- c("Australia", "Europe", "New Zealand", "North America")

# row order (to be in accordance with other tables)
taxa_diff_dir_weighted <-
  taxa_diff_dir_weighted[match(c("Europe", "North America",
                             "Australia", "New Zealand"),
                           Database), ]

# latex output
xtable_wo_rownames(x = taxa_diff_dir_weighted, auto = TRUE)


# _____________________________________________________________________________
#### Range of deviance #### 
# _____________________________________________________________________________
look_up_db <- data.table(
  Key_col = c(
    "Trait_AUS_harmonized",
    "Trait_EU_pp_harmonized",
    "Trait_NZ_pp_harmonized",
    "Traits_US_LauraT_pp_harmonized"
  ),
  Database = c("Australia",
               "Europe",
               "New Zealand",
               "North America")
)


#### Complex and direct aggregation ####
Res_stepwise_dir <- rbindlist(results_agg, 
                              idcol = "id")
Res_stepwise_dir[look_up_db, 
                 `:=`(id = i.Database), 
                 on = c(id = "Key_col")]

# display how many families per order deviate with their 
# aggregated trait values for the stepwise_agg_median
# and the direct_agg_median
# take abs of negative 
Res_stepwise_dir_summary <- Res_stepwise_dir[deviance != 0, .(id = id,
                                                   order = order,
                                                   deviance,
                                                   abs_deviance = abs(deviance))]
# gtsummary table
Res_mean_table <- tbl_summary(
  data = Res_stepwise_dir_summary,
  statistic = list(all_continuous() ~ "{mean} ({sd})"),
  by = "id",
  label = list(deviance ~ "Mean dev.",
               abs_deviance ~ "Abs. mean dev."),
  digits = list(deviance ~ c(3, 2))
) %>%
  italicize_levels() 

# latex output
xtable_wo_rownames(Res_stepwise_dir_table$table_body[, c("label", "stat_1", "stat_2",
                                                         "stat_3", "stat_4")])

# Plot of the deviances 
# TODO: change Order of databases
Res_stepwise_dir[deviance != 0, .(deviance, variable, order), 
                 by = "id"] %>% 
  ggplot(., aes(x = order, y = deviance))+
  geom_point(size = 2)+
  geom_boxplot(alpha = 0.7)+
  ylim(-1, 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Order", 
       y = "Deviance in affinity")+
  coord_flip()+
  facet_wrap(~id)+
  theme_light(base_size = 15) + #,base_family = "Poppins"
  theme(
    legend.position = "none", 
    axis.title = element_text(size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 10),
    axis.text.y = element_text(family = "Roboto Mono", size = 10)
  )
ggplot2::ggsave(filename = file.path(data_out, "Deviances_stepwise_dir_overview.png"),
                width = 21, 
                height = 12,
                units = "cm")

# Res_stepwise_dir[deviance != 0 & id == "Australia", ]
# High deviances? What is a high deviance actually?
setcolorder(x = Res_stepwise_dir, 
            c("id", "variable", "order", "family")) 
xtable_wo_rownames(Res_stepwise_dir[deviance <= -0.3 | deviance >= 0.3, ])

# Just deviances for grouping features locomotion, 
# respiration and feeding mode
Res_stepwise_dir[deviance != 0, unique(variable)]

#### Median and mean comparison ####
Res_mean_comp <- rbindlist(results_agg_means, 
                              idcol = "id")
Res_mean_comp[look_up_db, 
                 `:=`(id = i.Database), 
                 on = c(id = "Key_col")]

# display how many families per order deviate with their 
# aggregated trait values for the stepwise_agg_median
# and the direct_agg_median

# take abs of negative 
Res_mean_summary <- Res_mean_comp[deviance != 0, .(id = id,
                                                   order = order,
                                                   deviance,
                                                   abs_deviance = abs(deviance))]

# gtsummary table
Res_mean_table <- tbl_summary(
  data = Res_mean_summary,
  statistic = list(all_continuous() ~ "{mean} ({sd})"),
  by = "id",
  label = list(deviance ~ "Mean deviance"),
  digits = list(deviance ~ c(3, 2))
) %>%
  italicize_levels() 

# latex output
xtable_wo_rownames(Res_mean_table$table_body[, c("label", "stat_1", "stat_2",
                                                         "stat_3", "stat_4")])
# plot
Res_mean_comp[is.na(order), order := "Unknown"]
Res_mean_comp[deviance != 0, .(deviance, variable, order),
              by = "id"] %>%
  ggplot(., aes(x = order, y = deviance)) +
  geom_point(size = 2) +
  geom_boxplot(alpha = 0.7) +
  ylim(-1, 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Order",
       y = "Deviance in affinity") +
  coord_flip() +
  facet_wrap( ~ id) +
  theme_light(base_size = 15) + #,base_family = "Poppins"
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 10),
    axis.text.y = element_text(family = "Roboto Mono", size = 10)
  )
ggplot2::ggsave(filename = file.path(data_out, "Deviances_dir_median_mean_overview.png"),
                width = 21,
                height = 12,
                units = "cm")

# High deviances
setcolorder(x = Res_mean_comp, 
            c("id", "variable", "order", "family")) 
xtable_wo_rownames(Res_mean_comp[deviance <= -0.3 | deviance >= 0.3, ])

# Just deviances for grouping features locomotion, 
# respiration and feeding mode
Res_stepwise_dir[deviance != 0, unique(variable)]



# _____________________________________________________________________________
#### For which families did the aggregation methods yield different trait values? ###
# _____________________________________________________________________________
lapply(results_agg, function(y)
  y[deviance != 0, .(family, order)] %>%
    .[!duplicated(family),])
# lapply(results_agg, function(y) y[deviance != 0 & family %in% "Baetidae", ])


# families per order
lapply(results_agg, function(y) y[deviance != 0, .(family, variable, order)] %>% 
         .[, .(family, variable, .N), by = "order"] %>% 
         .[order(-N)]) 
# lapply(results_agg, function(y) y[deviance > 0, ])
# lapply(results_agg, function(y) y[deviance < 0, ])

# _____________________________________________________________________________

#### Direct aggregation and weighted aggregation ####
Res_dir_weighted <- rbindlist(results_agg_dir_weighted, 
                              idcol = "id")
Res_dir_weighted[look_up_db,
                 `:=`(id = i.Database),
                 on = c(id = "Key_col")]

# display how many families per order deviate with their 
# aggregated trait values for the stepwise_agg_median
# and the direct_agg_median
# take abs of negative 
Res_dir_weighted_summary <- Res_dir_weighted[deviance != 0, .(id = id,
                                                              order = order,
                                                              deviance,
                                                              abs_deviance = abs(deviance))]
# gtsummary table
Res_dir_weighted_table <- tbl_summary(
  data = Res_dir_weighted_summary,
  statistic = list(all_continuous() ~ "{mean} ({sd})"),
  by = "id",
  label = list(deviance ~ "Mean dev.",
               abs_deviance ~ "Abs. mean dev."),
  digits = list(deviance ~ c(3, 2))
) %>%
  italicize_levels() 

# latex output
xtable_wo_rownames(Res_dir_weighted_table$table_body[, c("label", "stat_1", "stat_2",
                                                         "stat_3", "stat_4")])

# Plot of the deviances 
# TODO: change Order of databases
Res_dir_weighted[deviance != 0, .(deviance, variable, order), 
                 by = "id"] %>% 
  ggplot(., aes(x = order, y = deviance))+
  geom_point(size = 2)+
  geom_boxplot(alpha = 0.7)+
  ylim(-1, 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Order", 
       y = "Deviance in affinity")+
  coord_flip()+
  facet_wrap(~id)+
  theme_light(base_size = 15) + #,base_family = "Poppins"
  theme(
    legend.position = "none", 
    axis.title = element_text(size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 10),
    axis.text.y = element_text(family = "Roboto Mono", size = 10)
  )
ggplot2::ggsave(filename = file.path(data_out, "Deviances_stepwise_dir_overview.png"),
                width = 21, 
                height = 12,
                units = "cm")

# Res_stepwise_dir[deviance != 0 & id == "Australia", ]
# High deviances? What is a high deviance actually?
setcolorder(x = Res_dir_weighted, 
            c("id", "variable", "order", "family")) 
xtable_wo_rownames(Res_dir_weighted[deviance <= -0.3 | deviance >= 0.3, ])

# Just deviances for grouping features locomotion, 
# respiration and feeding mode
Res_dir_weighted[deviance != 0, unique(variable)]


# #### Are there families where trait values diverge that occur in several/all datasets? ####
# # _____________________________________________________________________________
# lapply(results_agg, function(y) y[deviance != 0, .(family)]) %>%
#   rbindlist(., idcol = "file") %>%
#   dcast(., formula = family ~ file) %>%
#   .[Trait_AUS_harmonized != 0 & Traits_US_LauraT_pp_harmonized != 0 &
#       Trait_EU_pp_harmonized != 0, ]
# # Trait_NZ_pp_harmonized != 0 & 
# 
# # _____________________________________________________________________________
# #### Which traits were differently classified according to the aggregation methods? ####
# # _____________________________________________________________________________
# lapply(results_agg, function(y) y[, .N, by = "variable"])
# 
# lapply(results_agg, function(y) y[deviance != 0, .(.N, family),
#                                   by = "variable"] %>% 
#          .[order(-N), ])
# 
# lapply(results_agg, function(y) y[deviance != 0, .(family, variable)]) %>%
#   rbindlist(., idcol = "file") %>%
#   dcast(., formula = variable ~ file) %>% 
#   .[order(Trait_AUS_harmonized), ]
# 
# lapply(results_agg, function(y) y[deviance != 0, .(family, variable)]) %>%
#   rbindlist(., idcol = "file") %>% 
#   .[family %in% "Leptophlebiidae", ]
# 
# results_agg[["Trait_AUS_harmonized"]][family %in% "Leptophlebiidae", ]
# 
# traitval_noa[, .N, by = "variable"] %>% .[, N]
# traitval_noa[deviance_dir_fam != 0, .(.N),
#              by = c("variable")] %>%
#   .[order(-N), N / total]
# 
# 
# # _____________________________________________________________________________
# #### Specific traits that diverge in all databases? ####
# # _____________________________________________________________________________
# lapply(results_agg, function(y) y[deviance != 0, .(family, variable)]) %>%
#   rbindlist(., idcol = "file") %>%
#   dcast(., formula = variable ~ file) %>%
#   .[Trait_AUS_harmonized != 0 & Traits_US_LauraT_pp_harmonized != 0 &
#       Trait_NZ_pp_harmonized != 0 & Trait_EU_pp_harmonized != 0, ]
# 
# lapply(results_agg, function(y) y[deviance != 0 & variable %like% "feed.*gatherer", 
#                                   .(family, order, variable)])
# 
# 
# #### For which taxa are deviances greater than 0.5? ####
# lapply(results_agg, function(y) Hmisc::describe(y$deviance))
# lapply(results_agg, function(y) y[deviance >= 0.5, .(family, variable, 
#                                                      deviance)])

# _____________________________________________________________________________
#### Plotting deviance data ####
# Plots for every family 
# Might use later or for SI if interesting
# _____________________________________________________________________________

# Plot of deviance per family
# Example Australia
# results_aus <- results_agg[["Trait_EU_pp_harmonized"]]
# 
# #results_aus[order %in% c("Trichoptera") & variable %like% "feed.*|resp.*|size.*|volt.*", ] %>%
# results_aus[deviance != 0, ] %>%
#   ggplot(., aes(
#     x = as.factor(variable), y = deviance,
#     label = deviance
#   )) +
#   geom_point(stat = "identity", aes(col = family), size = 12) +
#   geom_segment(aes(y = 0, 
#                    x = variable, 
#                    yend = deviance, 
#                    xend = variable,
#                    col = family))+ 
#   geom_text(color = "white", size = 3) +
#   labs(
#     title = "Comparison complex aggregation vs direct aggregation",
#     y = "Deviance",
#     x = "Traits"
#   ) +
#   ylim(-1.5, 1.5) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   coord_flip() +
#   facet_wrap(~family) +
#   theme_light(base_size = 15) + #,base_family = "Poppins"
#   theme(
#     legend.position = "none", 
#     axis.title = element_text(size = 12),
#     axis.text.x = element_text(family = "Roboto Mono", size = 10),
#     axis.text.y = element_text(family = "Roboto Mono", size = 10)
#   )
# 
# results_aus[deviance <= 0.1 & deviance > 0, ]
# 
# # save
# ggsave(
#   filename = "Trait_agg.png", plot = last_plot(),
#   path = file.path(data_out),
#   dpi = 400
# )
# 
# # summary plot: 
# rbindlist(results_agg, idcol = "file") %>%
#   .[deviance != 0,] %>%
#   ggplot(., aes(as.factor(variable), y = deviance,
#                 label = deviance)) +
#   geom_point(stat = "identity", aes(col = family), size = 12) +
#   geom_segment(aes(
#     y = 0,
#     x = variable,
#     yend = deviance,
#     xend = variable,
#     col = family
#   )) +
#   geom_text(color = "white", size = 3)+
#   coord_flip()+
#   facet_wrap(~file)+
#   theme(legend.position = "none")

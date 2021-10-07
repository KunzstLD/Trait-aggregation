
# _________________________________________________________________________________________________
#### Analysis single runs ####
# Find cases that with large differences produced by aggr. methods
# calculate differences within each run
# _________________________________________________________________________________________________

# load simulation results
res_base_sim <- readRDS(file = file.path(data_cache, "res_base_sim.rds"))

diffs <- list()
for(i in unique(res_base_sim$run_id)){
  # subset to each run
  subs <- res_base_sim[run_id == i,]
  
  # create template with all unique comb of aggr. methods
  template <- combn(unique(subs$method), m = 2) %>% t() %>% as.data.table()
  template[, comparison := paste0(V1, "_VS_",V2)]
  
  # include also the different simulation types
  template_1 <- expand.grid("comparison" = template$comparison, 
                            "simulation" = unique(subs$simulation))
  setDT(template_1)
  
  # paste V1 and V2 back 
  template_1[template, `:=`(V1 = i.V1,
                            V2 = i.V2),
             on = "comparison"]
  
  # merge to obtain trait information for comparison
  template_1[subs, `:=`(T1_fromV1 = i.T1,
                        T2_fromV1 = i.T2,
                        T3_fromV1 = i.T3),
             on = c(V1 = "method", 
                    simulation = "simulation")]
  template_1[subs, `:=`(T1_fromV2 = i.T1,
                        T2_fromV2 = i.T2,
                        T3_fromV2 = i.T3),
             on = c(V2 = "method",
                    simulation = "simulation")]
  
  # calc differences of trait affinities for the different methods
  template_1[, `:=`(
    diff_T1 = T1_fromV1 - T1_fromV2,
    diff_T2 = T2_fromV1 - T2_fromV2,
    diff_T3 = T3_fromV1 - T3_fromV2
  )]
  
  # save output
  diffs[[i]] <- template_1
}

# 10 comparisons * 500 run_ids * 3 sim_methods
res_single_runs <- rbindlist(diffs, idcol = "run_id")
res_single_runs[, sd := as.numeric(sub("[0-9]{1,}\\_", "", run_id))]
# res_single_runs[simulation == "sim_extreme" & abs(diff_T3) > 0.1, ]

# lf
res_single_runs <- melt(
  res_single_runs,
  measure.vars = c("diff_T1", "diff_T2", "diff_T3"),
  value.name = "differences"
)
# take abs of differences
res_single_runs[, abs_differences := abs(differences)]

# _________________________________________________________________________________________________
#### Graphical analysis ####
# _________________________________________________________________________________________________
res_base_sim[, method := factor(
  method,
  levels = c(
    "direct_mean",
    "stepwise_mean",
    "weighted_agg",
    "direct_median",
    "stepwise_median"
  )
)]

# - Overview of aggregated trait values for T1
res_base_sim %>%
  melt(., measure.vars = c("T1", "T2", "T3")) %>%
  .[variable == "T1", ] %>%
  ggplot(., aes(x = as.factor(sd), y = value)) +
  geom_boxplot(aes(fill = as.factor(method)), alpha = 0.9) +
  scale_fill_colorblind( 
    labels = c(
      "direct_agg \n (mean)",
      "stepwise_agg \n (mean)",
      "weighted_agg",
      "direct_agg \n (median)",
      "stepwise_agg \n (median)"
    )
  ) + # labels = c("A", "B")
  facet_grid(. ~ as.factor(simulation)) +
  labs(x = "Trait variability (standard deviation)",
       y = "Trait affinitiy",
       fill = "Aggregation method") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 11),
    axis.text.y = element_text(family = "Roboto Mono", size = 11),
    legend.title = element_text(size = 12),
    legend.text = element_text(family = "Roboto Mono", size = 11)
  )
for (link in c(data_out, data_paper)) {
  ggplot2::ggsave(
    filename = file.path(link, "Overview_sim_results.png"),
    width = 25,
    height = 13,
    units = "cm"
  )
}

# - Overview of aggregated trait values for other traits (SI)
res_base_sim %>%
  melt(., measure.vars = c("T1", "T2", "T3")) %>%
  .[variable %in% c("T1", "T2"),] %>%
  ggplot(., aes(x = as.factor(sd), y = value)) +
  geom_boxplot(aes(fill = as.factor(method)), alpha = 0.9) +
  scale_fill_colorblind(
    labels = c(
      "direct_agg \n (mean)",
      "stepwise_agg \n (mean)",
      "weighted_agg",
      "direct_agg \n (median)",
      "stepwise_agg \n (median)"
    )
  ) + # labels = c("A", "B")
  facet_grid(as.factor(variable) ~ as.factor(simulation)) +
  labs(x = "Trait variability (standard deviation)",
       y = "Trait affinitiy",
       fill = "Aggregation method") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 11),
    axis.text.y = element_text(family = "Roboto Mono", size = 11),
    legend.title = element_text(size = 12),
    legend.text = element_text(family = "Roboto Mono", size = 11),
    strip.text = element_text(family = "Roboto Mono", size = 11)
  )
for (link in c(data_out, data_paper)) {
  ggplot2::ggsave(
    filename = file.path(link, "Overview_sim_results_T2_T3.png"),
    width = 25,
    height = 13,
    units = "cm"
  )
}

# - Overview of individual datasets: 

# How often differences between Aggr. methods 
# greater than - e.g. 0.1 - for each run per simulation?
res_single_runs[abs_differences >= 0.1, 
                .(.N, abs_differences, differences, sd, 
                  perc_all = (.N/15000)*100), 
                by = simulation] 

# which comparisons show the greatest differences? 
res_single_runs[abs_differences >= 0.1, 
                .N,
                by = .(simulation, comparison)] 

# maximum of differences
res_single_runs[abs_differences == max(abs_differences), ]

# plot preparations
res_single_runs[, comparison := factor(
  comparison,
  levels = c(
    "stepwise_median_VS_weighted_agg",
    "direct_median_VS_stepwise_median",
    "direct_mean_VS_stepwise_median",
    "stepwise_mean_VS_weighted_agg",
    "stepwise_mean_VS_stepwise_median",
    "direct_median_VS_weighted_agg",
    "direct_median_VS_stepwise_mean",
    "direct_mean_VS_direct_median",
    "direct_mean_VS_stepwise_mean",
    "direct_mean_VS_weighted_agg"
  )
)]

# extract only diffs greater 0.1
single_run_diffs <- res_single_runs[abs_differences >= 0.1,
                                    .(differences, abs_differences, comparison, simulation, sd)]
single_run_diffs[, `:=`(
  max_abs_diff = max(abs_differences),
  min_abs_diff = min(abs_differences)
),
by = .(comparison,
       simulation,
       sd)]

# label names
label_names <- c("0.2" = "sd = 0.2",
                 "0.4" = "sd = 0.4",
                 "0.6" = "sd = 0.6",
                 "0.8" = "sd = 0.8",
                 "1" = "sd = 1")

# plot
ggplot() +
  geom_pointrange(
    data = single_run_diffs,
    mapping = aes(
      x = as.factor(comparison),
      y = abs_differences, 
      ymin = min_abs_diff,
      ymax = max_abs_diff,
      color = as.factor(simulation)
    ),
    position = position_dodge(width = .8),
    alpha = 0.7
  ) +
  labs(x = "Comparison", 
       y = "Absolute differences in \n aggregated trait affinities", 
       color = "Simulation type")+
  scale_color_uchicago() +
  scale_x_discrete(
    labels = c("7)", #"Stepwise_agg (median) - \n Weighted_agg", 
               "6)", #"Direct_agg (median) - \n Stepwise_agg (median)", 
               "5)", #"Direct_agg (mean) - \n Stepwise_agg (median)", 
               "4)", #"Stepwise_agg (mean) - \n Weighted_agg",
               "3)", #"Stepwise_agg (mean) - \n Stepwise_agg (median)",
               "2)", #"Direct_agg (median) - \n Weighted_agg",
               "1)"))+ #"Direct_agg (median) - \n Stepwise_agg (mean)"))+
  facet_wrap( ~  as.factor(sd), labeller = as_labeller(label_names)) +
  coord_flip() +
  theme_classic() +
  theme(
    #legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 11),
    axis.text.y = element_text(family = "Roboto Mono", size = 11),
    legend.title = element_text(size = 12),
    legend.text = element_text(family = "Roboto Mono", size = 11),
    legend.position = c(0.85, 0.25),
    strip.text = element_text(family = "Roboto Mono", size = 11)
  )
for (link in c(data_out, data_paper)) {
  ggplot2::ggsave(
    filename = file.path(link, "Diffs_indiv_runs_sim.png"),
    width = 25,
    height = 17,
    units = "cm"
  )
}

# TODO: Create a label plot or something similar?
# dt_label <- data.table(
#   x = rep(1, 7),
#   y = c(1:7),
#   label = c(
#     "7) Stepwise_agg (median) - \n Weighted_agg",
#     "6) Direct_agg (median) - \n Stepwise_agg (median)",
#     "5) Direct_agg (mean) - \n Stepwise_agg (median)",
#     "4) Stepwise_agg (mean) - \n Weighted_agg",
#     "3) Stepwise_agg (mean) - \n Stepwise_agg (median)",
#     "2) Direct_agg (median) - \n Weighted_agg",
#     "1) Direct_agg (median) - \n Stepwise_agg (mean)"
#   )
# )
# label_plot <- ggplot(dt_label)+
#   geom_text(aes(x = as.factor(x),
#                 y = y,
#                 label = label))+
#   theme_classic() +
#   theme(
#     #legend.position = "none",
#     axis.title = element_blank(),
#     axis.text = element_blank(),
#     axis.line = element_blank(),
#     axis.ticks = element_blank()
#   )

# Number of deviating comparisons for result section
# single_run_diffs[, .(comparison, simulation, sd, N)] %>% View


## =================================================
# Results:
# base example:
# - high variation in traits leads to different results
# - seems (almost) not to influence methods using the mean
# - when differences occur, mainly for:
# dir_median vs stepwise median 

# when Trait variability: 0.3 and greater
# - Max diff: ~ 0.2
# - Highest variation in trait affinities by median aggr
# methods (stepwise > direct)
## =================================================
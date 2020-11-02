# Simulation Idea Ben:
# I wonder if some simulation could be useful to determine in what
# circumstances the different aggregation methods will produce similar
# or different results. E.g. If all families were to have the same number of
# genus and all genus the same number of species (something which would
#  never happen),
# then it will not matter (much) whether direct, stepwise or
# weighted aggregation is used.
# It only becomes important if direct, stepwise or weighted is used,
# as the number of genus per family and/or species per genus differ.
# Some simulation that varies the taxonomic hierarchy and variability in traits
# between genus within a family and species within genus, ….,
# could be useful to show, under what circumstances the choice of
# aggregation method becomes important.
# Then can look back to the databases and see if
# these circumstances come up much.
# If these circumstances are rare,
# then do not need to consider the aggregation methods.

# simulate different levels of taxonomic hierarchy:
# from species - genus - family
# which traits? traits variability should also vary
# Additional factor: taxonomic resolution

# Fix: Macropelopiini 1 in NZ Data


# _______________________________________________________________________________________________
#### Simulation ####

# Pre-thoughts:
# 25 values for a fixed sd are simulated in each run
# 100 runs + 5 sds -> 500
# 500*25 = 12500 rows!
# 12500 * simulated datasets (3)
# _______________________________________________________________________________________________
set.seed(1234)

# - simulate taxonomic data
sim_data <- list(
  # Base example:
  # same number of species per genus
  # (5 species per genus, in total 5 genera -> 25 taxa)
  "sim_base" = data.table(
    order = "O1",
    family = "F1",
    genus = rep(paste0("G", 1:5), each = 5),
    species = paste0(rep(paste0("SP", 1:5), each = 5), "_", 1:5)
  ),
  
  # "Extreme" example:
  # One genus covering ~50 of the species listed in one family
  # rest equal
  "sim_extreme" = data.table(
    order = "O1",
    family = "F1",
    genus = c(rep("G1", 13), rep(paste0("G", 2:5), each = 3)),
    species = c(paste0("SP1_", 1:13), paste0(rep(
      paste0("SP", 2:5), each = 3
    ), "_", 1:3))
  ),
  
  # "Variation" example:
  # every genus has different amount of species
  "sim_variation" = data.table(
    order = "O1",
    family = "F1",
    genus = c(
      rep("G1", 8),
      rep("G2", 2),
      rep("G3", 7),
      rep("G4", 3),
      rep("G2", 5)
    ),
    species = c(
      paste0("SP1_", 1:8),
      paste0("SP2_", 1:2),
      paste0("SP3_", 1:7),
      paste0("SP2_", 1:3),
      paste0("SP2_", 1:5)
    )
  )
)

# - do the simulation
variability <- seq(0.2, 1, 0.2)

sim_variab <- list()
sim_intm <- list()

for (i in 1:100) {
  for (k in seq_along(variability)) {
    dat <- data.table(sd = rep(variability[[k]], 25))
    sim_variab[[k]] <-
      dat[, c("T1", "T2", "T3") := replicate(25,
                                             sim_trait_vals(mean = 0.5, sd = variability[[k]]),
                                             simplify = "matrix") %>%
            t() %>%
            as.data.table()]
  }
  sim_intm[[i]] <- sim_variab
}

# bind together and take advantage of the fact that R 
# "recycles" rows when binding
sim_intm <- lapply(sim_intm, rbindlist) %>%
  rbindlist(., idcol = "run")
sim_result <- lapply(sim_data, function(y) cbind(y, sim_intm)) %>% 
  rbindlist(., idcol = "simulation")

# - apply aggr. methods
# 5 aggr methods * 2 Simulations * 100 runs * 5 sd levels = 5000 rows
res_base_sim <- sim_result[, meta_agg(
  data = .SD,
  non_trait_cols = c(
    "species",
    "genus",
    "family",
    "order",
    "run",
    "sd",
    "simulation"
  )
),
by = .(simulation, run, sd)
]
res_base_sim[, run_id := paste0(run, "_", sd)]


#### Analysis single runs ####
# Find cases that with large differences produced by aggr. methods
# calculate differences within each run
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

# lf
res_single_runs <- melt(
  res_single_runs,
  measure.vars = c("diff_T1", "diff_T2", "diff_T3"),
  value.name = "differences"
)
# take abs of differences
res_single_runs[, abs_differences := abs(differences)]


#### Graphical analysis ####
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

# - overview of aggregated trait values :
res_base_sim %>%
  melt(., measure.vars = c("T1", "T2", "T3")) %>% 
    ggplot(., aes(x = as.factor(sd), y = value)) +
    geom_boxplot(aes(fill = as.factor(method)), alpha = 0.5) +
    scale_fill_uchicago(labels = c("direct_agg \n (mean)", 
                                   "stepwise_agg \n (mean)",
                                   "weighted_agg",
                                   "direct_agg \n (median)",
                                   "stepwise_agg \n (median)")) + # labels = c("A", "B")
    facet_grid(as.factor(variable) ~ as.factor(simulation)) +
    labs(x = "Standard deviation",
         y = "Trait affinitiy",
         fill = "Aggregation method") +
    theme_bw() +
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

# - Overview of individual runs: 
# How often differences between Aggr. methods 
# greater than - e.g. 0.1 - for each run per simulation?
res_single_runs[abs_differences >= 0.1, 
                .(.N, abs_differences, differences, sd), 
                by = simulation] 

# which comparisons show the greatest differences? 
res_single_runs[abs_differences >= 0.1, 
                .N,
                by = .(simulation, comparison)] 

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
    labels = c("Stepwise_agg (median) - \n Weighted_agg", 
               "Direct_agg (median) - \n Stepwise_agg (median)", 
               "Direct_agg (mean) - \n Stepwise_agg (median)", 
               "Stepwise_agg (mean) - \n Weighted_agg",
               "Stepwise_agg (mean) - \n Stepwise_agg (median)",
               "Direct_agg (median) - \n Weighted_agg",
               "Direct_agg (median) - \n Stepwise_agg (mean)"))+
  facet_wrap( ~  as.factor(sd), labeller = as_labeller(label_names)) + 
  coord_flip() +
  theme_bw() +
  theme(
    #legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 11),
    axis.text.y = element_text(family = "Roboto Mono", size = 11),
    legend.title = element_text(size = 12),
    legend.text = element_text(family = "Roboto Mono", size = 11)
  )
for (link in c(data_out, data_paper)) {
  ggplot2::ggsave(
    filename = file.path(link, "Diffs_indiv_runs_sim.png"),
    width = 25,
    height = 13,
    units = "cm"
  )
}

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

    






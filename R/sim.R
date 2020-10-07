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


#### Retrieve tax. hierarchy ####
preproc_cp <- copy(preproc_dat)

# tax. hierarchy for each and tax. resolution
lapply(preproc_cp, function(y) retrieve_tax_hierarchy(x = y))

# overview tax. hierarchy
# most frequent case: few (1- ~5) genera
# & few species
# There are some extremes per DB, i.e. few families with
# a lot of different genera and sometimes also many species
# Trend: the more genera per family, more species
# Use frequencies of how many genera per family to
# explain differences (or lack thereof) in aggr. methods
# -> many families with 1 genus -> no difference between
# direct, weighted and stepwise
overview_hierarchy <- lapply(preproc_cp, function(y) {
  y[, .(
    nr_unique_genus,
    entry_on_species,
    family
  )] %>%
    .[!is.na(nr_unique_genus), ] %>%
    .[!duplicated(family), ]
})

# nr_unique_genus vs entry_on_species
overview_hierarchy %>%
  rbindlist(., idcol = "dataset") %>%
  .[dataset == "Trait_freshecol_2020_pp_harmonized", ] %>%
  .[nr_unique_genus <= 50, ] %>%
  ggplot(., aes(x = nr_unique_genus, y = entry_on_species)) +
  geom_point() +
  facet_wrap(~dataset) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 9),
    axis.text.y = element_text(family = "Roboto Mono", size = 9)
  )

#
lapply(overview_hierarchy, function(y) {
  Hmisc::describe(y)
})

# derive frequency unique genus entries
lapply(overview_hierarchy, function(y) {
  Hmisc::describe(y) %>%
    .[["nr_unique_genus"]] %>%
    .[["values"]]
}) %>%
  rbindlist(., idcol = "dataset")

# frequency species entries
lapply(overview_hierarchy, function(y) {
  Hmisc::describe(y) %>%
    .[["entry_on_species"]] %>%
    .[["values"]]
}) %>%
  rbindlist(., idcol = "dataset")

# plot
lapply(preproc_cp, function(y) {
  y[, .(
    nr_unique_genus,
    entry_on_species,
    family
  )] %>%
    .[!is.na(nr_unique_genus), ] %>% # don't regard entries on family-lvl
    .[!duplicated(family), ]
}) %>%
  rbindlist(., idcol = "dataset") %>%
  melt(., measure.vars = c(
    "nr_unique_genus",
    "entry_on_species"
  )) %>%
  ggplot(.) +
  geom_violin(aes(x = as.factor(dataset), y = value)) +
  facet_wrap(~variable) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 9),
    axis.text.y = element_text(family = "Roboto Mono", size = 9),
  )

# bind
overview_bind <- overview_hierarchy %>%
  rbindlist(., idcol = "dataset")



#### Simulation ####

# look at the real data:
# test <- copy(preproc_cp$Trait_freshecol_2020_pp_harmonized)
# # # nr. species per genus
# test <- test[family == "Baetidae", .SD,
#   .SDcols = names(test) %like% "^species|^genus|^family|^order|feed.*"
# ]
# test <- test[!is.nan(feed_shredder), ]

## =====================================================
# Pre-thoughts:
# 25 values for a fixed sd are simulated in each run
# 100 runs + 5 sds -> 500
# 500*25 = 12500 rows!
# 12500 * simulated datasets (3)
## =====================================================
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
  "sim_extrm" = data.table(
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
variability <- seq(0.1, 0.5, 0.1)

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
res_single_runs[, sd := as.numeric(sub("V[0-9]{1,}\\_", "", run_id))]

# lf
res_single_runs <- melt(
  res_single_runs,
  measure.vars = c("diff_T1", "diff_T2", "diff_T3"),
  value.name = "differences"
)
# take abs of differences
res_single_runs[, abs_differences := abs(differences)]
res_single_runs[differences > 0.1, .(comparison, run_id, .N), by = simulation] %>% View


#### Graphical analysis ####

# 

# How often differences between Aggr. methods 
# greater than - e.g. 0.1 - for each run per simulation?



## =================================================
# Results:
# base example:
# - high variation in traits leads to different results
# - seems (almost) not to influence methods using the mean
# - when differences occur, only for:
# dir_median vs stepwise median
# direct mean vs stepwise median
# stepwise mean vs stepwise median
# direct mean vs direct median
# stepwise median vs weighed agg
# when Trait variability: 0.3 and greater
# - Max diff: ~ 0.2
# - Highest variation in trait affinities by median aggr
# methods (stepwise > direct)

## =================================================

    






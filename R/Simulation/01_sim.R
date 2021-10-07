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
sim_result <- lapply(sim_data, function(y)
  cbind(y, sim_intm)) %>%
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

# save as .rds
saveRDS(object = res_base_sim,
        file = file.path(data_cache, "res_base_sim.rds"))
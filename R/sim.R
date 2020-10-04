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
    entry_on_specis,
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
  ggplot(., aes(x = nr_unique_genus, y = entry_on_specis)) +
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
    .[["entry_on_specis"]] %>%
    .[["values"]]
}) %>%
  rbindlist(., idcol = "dataset")

# plot
lapply(preproc_cp, function(y) {
  y[, .(
    nr_unique_genus,
    entry_on_specis,
    family
  )] %>%
    .[!is.na(nr_unique_genus), ] %>% # don't regard entries on family-lvl
    .[!duplicated(family), ]
}) %>%
  rbindlist(., idcol = "dataset") %>%
  melt(., measure.vars = c(
    "nr_unique_genus",
    "entry_on_specis"
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
test <- copy(preproc_cp$Trait_freshecol_2020_pp_harmonized)

# nr. species per genus
test <- test[family == "Baetidae", .SD,
  .SDcols = names(test) %like% "^species|^genus|^family|^order|feed.*"
]
test <- test[!is.nan(feed_shredder), ]


# simulate trait values with different variabilities
sim_trait_vals <- function(n_traits = 3, mean, sd) {
  val <- rnorm(n = n_traits, mean = mean, sd = sd)
  # set values smaller as zero to zero
  val[val < 0] <- 0
  val / sum(val)
}

#### Base example ####
# same number of species per genus
# (5 species per genus, in total 5 genera -> 25 taxa)
# high variation in traits leads to different results
# between stepwise median and direct median
# seems not to influence methods using the mean

# sim_data_base <- data.table(
#   order = "O1",
#   family = "F1",
#   genus = rep(paste0("G", c(1:5)), each = 5),
#   species = paste0(rep(paste0("SP", c(1:5)), each = 5), "_", c(1:5))
# )

output <- list()
variability <- seq(0.1, 0.5, 0.1)
sim_variab <- list()

for (i in 1:100) {
  for (j in seq_along(variability)) {
    sim_data_base <- data.table(
      order = "O1",
      family = "F1",
      genus = rep(paste0("G", c(1:5)), each = 5),
      species = paste0(rep(paste0("SP", c(1:5)), each = 5), "_", c(1:5)),
      sd = variability[[j]]
    )

    sim_variab[[j]] <- sim_data_base[, c("T1", "T2", "T3") := replicate(25,
      sim_trait_vals(mean = 0.5, sd = variability[[j]]),
      simplify = "matrix"
    ) %>%
      t() %>%
      as.data.table()]
  }
  output[[i]] <- sim_variab
}
base_sim <- lapply(output, rbindlist) %>% rbindlist(., idcol = "run")

# apply aggr. methods
res_base_sim <- base_sim[, meta_agg(
  data = .SD,
  non_trait_cols = c(
    "species",
    "genus",
    "family",
    "order",
    "run",
    "sd"
  )
),
by = .(run, sd)
]

# create id col from run and sd
res_base_sim[, run_id := paste0(run, "_", sd)]

#### Analysis base example ####
# overview of differences per method and variation
label_names <- c("0.1" = "sd = 0.1",
                 "0.2" = "sd = 0.2",
                 "0.3" = "sd = 0.3",
                 "0.4" = "sd = 0.4",
                 "0.5" = "sd = 0.5")
res_base_sim %>%
  ggplot(., aes(x = method, y = T1)) +
  geom_boxplot() +
  facet_wrap( ~ as.factor(sd), 
              labeller = as_labeller(label_names)) +
  coord_flip() +
  labs(x = "Aggregation method",
       y = "Trait affinity") +
  scale_x_discrete(
    labels = c(
      "Direct_agg (mean)",
      "Direct_agg (median)",
      "Stepwise_agg (mean)",
      "Stepwise_agg (median)",
      "Weighted_agg"
    )
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 11),
    axis.text.y = element_text(family = "Roboto Mono", size = 11)
  )


# Cases where differences between aggr. methods are
# big -> how big?
# calculate differences for each run and then plot
result <- list()
for(i in unique(res_base_sim$run_id)) {
  # subset to each run
  subs <- res_base_sim[run_id == i,]
  
  # create template with all unique comb of aggr. methods
  template <- combn(test$method, m = 2) %>% t() %>% as.data.table()
  template[subs, `:=`(T1_fromV1 = i.T1,
                      T2_fromV1 = i.T2,
                      T3_fromV1 = i.T3),
           on = c(V1 = "method")]
  template[subs, `:=`(T1_fromV2 = i.T1,
                      T2_fromV2 = i.T2,
                      T3_fromV2 = i.T3),
           on = c(V2 = "method")]
  template[, `:=`(
    diff_T1 = T1_fromV1 - T1_fromV2,
    diff_T2 = T2_fromV1 - T2_fromV2,
    diff_T3 = T3_fromV1 - T3_fromV2
  )]
  
  # save output
  result[[i]] <- template
}
res_single_runs <- rbindlist(result, idcol = "run_id")
res_single_runs[, comparison := paste0(V1, "_VS_", V2)]
res_single_runs[, sd := as.numeric(sub("[0-9]{1,}\\_", "", run_id))]

# lf
res_single_runs <- melt(
  res_single_runs,
  measure.vars = c("diff_T1", "diff_T2", "diff_T3"),
  value.name = "differences"
)
# when differences occur, only for 
# dir_median vs stepwise median
# direct mean vs stepwise median
# stepwise mean vs stepwise median
# direct mean vs direct median
# stepwise median vs weighed agg
# Trait variability: 0.3 and greater
# Max diff ~ 0.2
res_single_runs[differences > 0.1, ] %>% 
  .[, unique(comparison)]

res_single_runs[, summary(differences)] #%>% 
  .[, unique(comparison)]






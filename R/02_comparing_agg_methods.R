# Script that compares two trait aggregation methods
# 1) Species - Genus - Family (using Mode)
# 2) Species - Family
# TODO:
# Provide Details on aggregation methods
# compare accross regions

# Methods used:
# Aggregation over genus and family (using median and mode)
# Aggregation directly to family level
# (Aggregation using Power Average Operator?)

# Test for:
# fuzzy coded data
# binary coded data
# binary data transformed to fuzzy coded (on genus level, by nr. of species)

# _____________________________________________________________________________
#### Aggreagation ####
# 1) Species - Genus - Family (using Mode) ####
# - Aggregation procedure:
# Median to Genus/
# Mode for Family
# -> differing amount of values to calculate value for each modality
# sum(table(AUS_subset[grepl("Chirono.*", family)]$temp_eurytherm))

# Poff et al. 2006: most common genus-level modality assigned (genus level
# trait data with family-level monitoring data)
# which(table(AUS_subset$genus) == max(table(AUS_subset$genus), na.rm = TRUE))
# _____________________________________________________________________________

# aggregation over genus level
data_complex_agg <- lapply(preproc_dat, function(y) {
  spec_genus_agg(
    trait_data = y,
    non_trait_cols = c(
      "order",
      "family",
      "genus",
      "species"
    )
  ) %>%
    data.table::melt(., id.vars = c("family", "N")) %>%
    setnames(.,
      old = c("value"),
      new = c("value_genus_fam_agg")
    )
})

# Aggregation directly to family level
data_direct_agg <- lapply(preproc_dat, function(y) {
  y[!is.na(family), ] %>%
  direct_agg(
    trait_data = .,
    non_trait_cols = c(
      "order",
      "family",
      "genus",
      "species"
    )
  ) %>%
    data.table::melt(., id.vars = c("family")) %>%
    setnames(.,
      old = c("value"),
      new = c("value_direct_agg")
    )
})

# _____________________________________________________________________________
#### Analysis ####
# _____________________________________________________________________________

# resulted lists have the same row order -> can be bind
results_agg <- mapply(function(x, y) merge(x, y, by = c("family", "variable")),
  data_complex_agg,
  data_direct_agg,
  SIMPLIFY = FALSE
)

# calculate deviance
results_agg <- lapply(
  results_agg,
  function(y) y[, deviance := value_genus_fam_agg - value_direct_agg]
)

# How many taxa end up with different trait values after aggregation?
lapply(results_agg, function(y) {
  (nrow(y[deviance != 0, ]) / nrow(y)) * 100
})

# families where aggregation methods yield different trait values
lapply(results_agg, function(y) y[deviance != 0, ])
lapply(results_agg, function(y) y[deviance > 0, ])
lapply(results_agg, function(y) y[deviance < 0, ])

# families where trait values diverge that occur in several datasets?
lapply(results_agg, function(y) y[deviance != 0, .(family)]) %>%
  rbindlist(., idcol = "file") %>%
  dcast(., formula = family ~ file) %>%
  .[Trait_AUS_harmonized.rds != 0 & Traits_US_LauraT_pp_harmonized.rds != 0 &
  Trait_NZ_pp_harmonized.rds != 0 & Trait_EU_pp_harmonized.rds != 0, ]

# specific traits where traits diverge more often?
lapply(results_agg, function(y) y[deviance != 0, .(family, variable)]) %>%
  rbindlist(., idcol = "file") %>%
  dcast(., formula = variable ~ file) %>%
  .[Trait_AUS_harmonized.rds != 0 & Traits_US_LauraT_pp_harmonized.rds != 0 &
    Trait_NZ_pp_harmonized.rds != 0 & Trait_EU_pp_harmonized.rds != 0, ]

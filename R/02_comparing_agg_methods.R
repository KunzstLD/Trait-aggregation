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
# 1) Species - Genus - Family (using Mode)
# - Aggregation procedure:
# Median to Genus/
# Mode for Family if exists, otherwise mean/median
# -> differing amount of values to calculate value for each modality
# sum(table(AUS_subset[grepl("Chirono.*", family)]$temp_eurytherm))

# Poff et al. 2006: most common genus-level modality assigned (genus level
# trait data with family-level monitoring data)
# which(table(AUS_subset$genus) == max(table(AUS_subset$genus), na.rm = TRUE))
# _____________________________________________________________________________

# leave the fuzzy coded NOA dataset out and investigate in separate script
comp_agg_data <- preproc_dat[c(
  "Trait_AUS_harmonized",
  "Trait_EU_pp_harmonized",
  "Trait_NZ_pp_harmonized",
  "Traits_US_LauraT_pp_harmonized"
)]

# aggregation over genus level
data_complex_agg <- lapply(comp_agg_data, function(y) {
  spec_genus_agg_alt(
    data = y,
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
      new = c("value_genus_fam_agg")
    )
})

# Aggregation directly to family level
data_direct_agg <- lapply(comp_agg_data, function(y) {
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
    data.table::melt(., id.vars = c("family", "order")) %>%
    setnames(.,
      old = c("value"),
      new = c("value_direct_agg")
    )
})

# weighted aggregation
# calculate weights 
# weights_list <- mapply(function(x, y){
#   compute_weights(init = x,
#                   preproc = y)},
#   trait_dat,
#   preproc_dat,
#   SIMPLIFY = FALSE)
# 
# weights_list <- weights_list[c(
#   "Trait_AUS_harmonized.rds",
#   "Trait_EU_pp_harmonized.rds",
#   "Trait_NZ_pp_harmonized.rds",
#   "Traits_US_LauraT_pp_harmonized.rds"
# )]
# 
# # aggregate
# data_weighted_agg <- mapply(function(x, y) {
#   weighted_agg(
#     data = x,
#     non_trait_cols =
#       c("order",
#         "family",
#         "genus",
#         "species"),
#     weights = y
#   ) %>%
#     data.table::melt(., id.vars = c("family", 
#                                     "order")) %>%
#     setnames(.,
#              old = c("value"),
#              new = c("value_weighted_agg")
#     )
# },
# comp_agg_data,
# weights_list,
# SIMPLIFY = FALSE)


# merge results
results_agg <-
  mapply(function(x, y)
    merge(x, y, by = c("family", "variable", "order")),
    data_complex_agg,
    data_direct_agg,
    SIMPLIFY = FALSE)

# calculate deviance
results_agg <- lapply(results_agg,
                      function(y)
                        y[, deviance := value_genus_fam_agg - value_direct_agg])

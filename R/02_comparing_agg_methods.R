# Script that compares two trait aggregation methods
# 1) Species - Genus - Family (using Mode)
# 2) Species - Family
# TODO:
# Provide Details on aggregation methods
# Updata Github

# Methods used:
# Aggregation over genus and family (using median and mode)
# Aggregation directly to family level
# Weighted Aggregation

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
agg_data <- preproc_dat[c(
  "Trait_AUS_harmonized",
  "Trait_EU_pp_harmonized",
  "Trait_NZ_pp_harmonized",
  "Traits_US_LauraT_pp_harmonized"
)]

# save
# for(i in names(agg_data)) {
#   write.csv(
#     x = agg_data[[i]],
#     file = file.path(data_out, paste0(names(agg_data[i]), ".csv")),
#     row.names = FALSE
#   )
# } 

#### Complex Aggregation ####
# aggregation over genus level (Median, Median)
data_complex_agg_median <- lapply(agg_data, function(y) {
  spec_genus_agg_alt(
    trait_data = y,
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
      new = c("value_genus_fam_agg_median")
    )
})

# aggregation over genus level (Median, Mean)
data_complex_agg_mean <- lapply(agg_data, function(y) {
  spec_genus_agg_alt(
    trait_data = y,
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
             new = c("value_genus_fam_agg_mean")
    )
})


#### Direct aggregation ####
# Aggregation directly to family level median
data_direct_agg_median <- lapply(agg_data, function(y) {
    direct_agg(
      trait_data = y,
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
      new = c("value_direct_agg_median")
    )
})

# Aggregation directly to family level mean
data_direct_agg_mean <- lapply(agg_data, function(y) {
    direct_agg(
      trait_data = y,
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
             new = c("value_direct_agg_mean")
    )
})

#### weighted Aggregation ####
# calculate weightes per DB
data_weighted <- lapply(agg_data, function(y) {
  weighted_agg(
    trait_data = y,
    non_trait_cols = c("order",
                       "family",
                       "genus",
                       "species",
                       "coeff")
  ) %>%
    data.table::melt(., id.vars = c("family", "order")) %>%
    setnames(.,
             old = c("value"),
             new = c("value_weighted_agg"))
})

#### Results complex vs direct aggregation ####
# TODO: Check again, weighted agg changed
# merge results
results_agg <-
  mapply(function(x, y)
    merge(x, y, by = c("family", "variable", "order")),
    data_complex_agg_median,
    data_direct_agg_median,
    SIMPLIFY = FALSE)

# calculate deviance
results_agg <- lapply(results_agg,
                      function(y)
                        y[, deviance := value_genus_fam_agg_median - value_direct_agg_median])

results_agg <- lapply(results_agg, function(y) y[!is.na(value_genus_fam_agg_median), ])

#### Results using mean instead of median in direct aggregation ####
results_agg_means <- mapply(
  function(x, y)
    merge(x, y, by = c("family", "variable", "order")),
  data_direct_agg_median,
  data_direct_agg_mean,
  SIMPLIFY = FALSE
)

# calculate deviance
results_agg_means <- lapply(results_agg_means,
                            function(y)
                              y[, deviance := value_direct_agg_median - value_direct_agg_mean])

results_agg_means <- lapply(results_agg_means, function(y) y[!is.na(value_direct_agg_mean), ])

#### Results weighted aggregation ####
# comparison direct agg median
results_agg_weighted_median <- mapply(
  function(x, y)
    merge(x, y, by = c("family", "variable", "order")),
  data_weighted,
  data_direct_agg_median,
  SIMPLIFY = FALSE
)

# calculate deviance
results_agg_weighted_median <- lapply(results_agg_weighted_median,
                                      function(y)
                                        y[, deviance := value_direct_agg_median - value_weighted_agg])

results_agg_weighted_median <-
  lapply(results_agg_weighted_median, function(y)
    y[!is.na(value_direct_agg_median),])

# comparison direct agg mean
results_agg_weighted_mean <- mapply(
  function(x, y)
    merge(x, y, by = c("family", "variable", "order")),
  data_weighted,
  data_direct_agg_mean,
  SIMPLIFY = FALSE
)

# calculate deviance
results_agg_weighted_mean <- lapply(results_agg_weighted_mean,
                               function(y)
                                 y[, deviance := value_direct_agg_mean - value_weighted_agg])

results_agg_weighted_mean <-
  lapply(results_agg_weighted_mean, function(y)
    y[!is.na(value_direct_agg_mean),])


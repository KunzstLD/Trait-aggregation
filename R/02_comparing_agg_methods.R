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
    data.table::melt(., id.vars = c("family", "order", "N")) %>%
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
    data.table::melt(., id.vars = c("family", "order")) %>%
    setnames(.,
      old = c("value"),
      new = c("value_direct_agg")
    )
})

# _____________________________________________________________________________
#### Analysis ####
# _____________________________________________________________________________

# results merged
results_agg <- mapply(function(x, y) merge(x, y, by = c("family", "variable", "order")),
  data_complex_agg,
  data_direct_agg,
  SIMPLIFY = FALSE
)

# calculate deviance
results_agg <- lapply(
  results_agg,
  function(y) y[, deviance := value_genus_fam_agg - value_direct_agg]
)

# How many taxa end up with different trait values after aggregation?(%)
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

# specific traits that diverge more often?
lapply(results_agg, function(y) y[deviance != 0, .(family, variable)]) %>%
  rbindlist(., idcol = "file") %>%
  dcast(., formula = variable ~ file) %>%
  .[Trait_AUS_harmonized.rds != 0 & Traits_US_LauraT_pp_harmonized.rds != 0 &
    Trait_NZ_pp_harmonized.rds != 0 & Trait_EU_pp_harmonized.rds != 0, ]

#### Plotting deviance data ####

# Example Australia
results_aus <- results_agg[["Trait_AUS_harmonized.rds"]]

results_aus[order %in% c("Ephemeroptera"), ] %>%
  ggplot(., aes(
    x = as.factor(variable), y = deviance,
    label = deviance
  )) +
  geom_point(stat = "identity", aes(col = family), size = 8) +
  geom_text(color = "white", size = 3) +
  labs(
    title = "Comparison complex aggregation vs direct aggregation",
    y = "Deviance in trait values",
    x = "Trait states"
  ) +
  ylim(-1.5, 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  facet_wrap(~family) +
  theme(
    axis.title = element_text(size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 9),
    #### For altering USGS trait databases
    ## goal: summarize trait information into one line per genus
    ## data is trimmed to only genera found in the Grand Lake Meadows

    axis.text.y = element_text(family = "Roboto Mono", size = 9),
    legend.text = element_text(size = 11),
    legend.title = element_blank()
  )

# save
ggsave(
  filename = "Trait_agg.png", plot = last_plot(),
  path = file.path(data_out),
  dpi = 400
)
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


# _____________________________________________________________________________
#### Analysis ####
# _____________________________________________________________________________


#### How many taxa end up with different trait values after aggregation?(%) ####
taxa_diff <- lapply(results_agg, function(y) {
  (nrow(y[deviance != 0, ]) / nrow(y)) * 100
}) %>%
  unlist(.) %>%
  as.data.table(., keep.rownames = TRUE) %>%
  setnames(.,
           old = c("rn", "."),
           new = c("Database", "Deviating cases"))

taxa_diff[, `Deviating cases` := round(`Deviating cases`, digits = 2)]
taxa_diff$Database <- c("Australia", "Europe", "New Zealand", "North America")
# latax output
# xtable_wo_rownames(x = taxa_diff, auto = TRUE)


#### For which families did the aggregation methods yield different trait values? ###
lapply(results_agg, function(y)
  y[deviance != 0, .(family, order)] %>%
    .[!duplicated(family),])
# lapply(results_agg, function(y) y[deviance != 0 & family %in% "Baetidae", ])


# families per order
lapply(results_agg, function(y) y[deviance != 0, .(family, order)] %>% 
         .[!duplicated(family), .(family, .N), by = "order"] %>% 
         .[order(-N)]) 
# lapply(results_agg, function(y) y[deviance > 0, ])
# lapply(results_agg, function(y) y[deviance < 0, ])


#### Are there families where trait values diverge that occur in several/all datasets? ####
# Baetidae
# Glossomatidae
lapply(results_agg, function(y) y[deviance != 0, .(family)]) %>%
  rbindlist(., idcol = "file") %>%
  dcast(., formula = family ~ file) %>%
  .[Trait_AUS_harmonized != 0 & Traits_US_LauraT_pp_harmonized != 0 &
  Trait_EU_pp_harmonized != 0, ]
# Trait_NZ_pp_harmonized != 0 & 

lapply(results_agg, function(y) y[deviance != 0 & family %in% "Hydroptilidae", ])


#### Which traits were differently classified according to the aggregation methods? ####
lapply(results_agg, function(y) y[deviance != 0, .(family, variable)]) %>%
  rbindlist(., idcol = "file") %>%
  dcast(., formula = variable ~ file) 

lapply(results_agg, function(y) y[deviance != 0, .(family, variable)]) %>%
  rbindlist(., idcol = "file") %>% 
  .[family %in% "Baetidae", ]

#### Specific traits that diverge in all databases? ####
lapply(results_agg, function(y) y[deviance != 0, .(family, variable)]) %>%
  rbindlist(., idcol = "file") %>%
  dcast(., formula = variable ~ file) %>%
  .[Trait_AUS_harmonized != 0 & Traits_US_LauraT_pp_harmonized != 0 &
    Trait_NZ_pp_harmonized != 0 & Trait_EU_pp_harmonized != 0, ]


#### Plotting deviance data ####

# Plot of deviance per family
# Example Australia
results_aus <- results_agg[["Trait_EU_pp_harmonized"]]

#results_aus[order %in% c("Trichoptera") & variable %like% "feed.*|resp.*|size.*|volt.*", ] %>%
results_aus[deviance != 0, ] %>%
  ggplot(., aes(
    x = as.factor(variable), y = deviance,
    label = deviance
  )) +
  geom_point(stat = "identity", aes(col = family), size = 12) +
  geom_segment(aes(y = 0, 
                   x = variable, 
                   yend = deviance, 
                   xend = variable,
                   col = family))+ 
  geom_text(color = "white", size = 3) +
  labs(
    title = "Comparison complex aggregation vs direct aggregation",
    y = "Deviance",
    x = "Traits"
  ) +
  ylim(-1.5, 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  facet_wrap(~family) +
  theme_light(base_size = 15) + #,base_family = "Poppins"
  theme(
    legend.position = "none", 
    axis.title = element_text(size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 10),
    axis.text.y = element_text(family = "Roboto Mono", size = 10)
  )

results_aus[deviance <= 0.1 & deviance > 0, ]

# save
ggsave(
  filename = "Trait_agg.png", plot = last_plot(),
  path = file.path(data_out),
  dpi = 400
)

# summary plot: 
rbindlist(results_agg, idcol = "file") %>%
  .[deviance != 0,] %>%
  ggplot(., aes(as.factor(variable), y = deviance,
                label = deviance)) +
  geom_point(stat = "identity", aes(col = family), size = 12) +
  geom_segment(aes(
    y = 0,
    x = variable,
    yend = deviance,
    xend = variable,
    col = family
  )) +
  geom_text(color = "white", size = 3)+
  coord_flip()+
  facet_wrap(~file)+
  theme(legend.position = "none")




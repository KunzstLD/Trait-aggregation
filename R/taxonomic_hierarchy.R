# __________________________________________________________________________________________________
#### Retrieve tax. hierarchy ####
# __________________________________________________________________________________________________
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
    .[!duplicated(family), ]
})

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
  rbindlist(., idcol = "dataset") %>% 
  .[, sum(frequency), by = "dataset"]

# frequency species entries
lapply(overview_hierarchy, function(y) {
  Hmisc::describe(y) %>%
    .[["entry_on_species"]] %>%
    .[["values"]]
}) %>%
  rbindlist(., idcol = "dataset")

# bind
overview_bind <- overview_hierarchy %>%
  rbindlist(., idcol = "dataset")

# subset to North American and Australian data that have been 
# used in the comparison of aggregated traits
# NOA:
noa_unique_families <- readRDS(file.path(data_cache, "noa_unique_families.rds"))
overview_bind[dataset == "Traits_US_LauraT_pp_harmonized" & family %in% noa_unique_families, ] %>% 
  Hmisc::describe()

# AUS: 
aus_unique_families <- readRDS(file.path(data_cache, "aus_unique_families.rds"))
overview_bind[dataset == "Trait_AUS_harmonized" & family %in% aus_unique_families, ] %>% 
  Hmisc::describe()

#### Plot taxonomic hierarchy ####

# AUS
dat_AUS <- overview_bind[dataset == "Trait_AUS_harmonized" &
                           family %in% aus_unique_families, ]

# percentage of families having 1 genus
dat_AUS[nr_unique_genus == 1,
        round(.N / nrow(dat_AUS) * 100,
              digits = 2)]

# percentage of families having 5 or less genera
perc_gen <- dat_AUS[nr_unique_genus <= 5,
                    round(.N / nrow(dat_AUS) * 100,
                          digits = 2)]
tax_hierarchy_plot(plotting_data = dat_AUS,
                   label_unique_genera = 7, 
                   annotate_region = "AUS", 
                   perc_genera = perc_gen) 
for(link in c(data_out, data_paper)) {
  ggplot2::ggsave(
    filename = file.path(link, "taxonomic_hierarchy_AUS.png"),
    width = 22,
    height = 12,
    units = "cm"
  )
}

# EU
# dat_EU <- overview_hierarchy$Trait_freshecol_2020_pp_harmonized
# perc_gen <- dat_EU[nr_unique_genus <= 5,
#                     round(.N / nrow(dat_EU) * 100,
#                           digits = 2)]
# tax_hierarchy_plot(plotting_data = dat_EU,
#                    label_unique_genera = 10, 
#                    y_lim_max = 400, 
#                    annotate_region = "EU")
# for(link in c(data_out, data_paper)) {
#   ggplot2::ggsave(
#     filename = file.path(link, "taxonomic_hierarchy_EU.png"),
#     width = 22,
#     height = 12,
#     units = "cm"
#   )
# }

# NOA
dat_NOA <- overview_bind[dataset == "Traits_US_LauraT_pp_harmonized" &
                           family %in% noa_unique_families, ]

# percentage of families having 1 genus
dat_NOA[nr_unique_genus == 1,
        round(.N / nrow(dat_NOA) * 100,
              digits = 2)]

# percentage of families having 5 or less genera
perc_gen <- dat_NOA[nr_unique_genus <= 5,
                   round(.N / nrow(dat_NOA) * 100,
                         digits = 2)]
tax_hierarchy_plot(plotting_data = dat_NOA,
                   label_unique_genera = 15, 
                   y_lim_max = 210, 
                   annotate_region = "NOA")
for(link in c(data_out, data_paper)) {
  ggplot2::ggsave(
    filename = file.path(link, "taxonomic_hierarchy_NOA.png"),
    width = 22,
    height = 12,
    units = "cm"
  )
}

# NZ
# dat_NZ <- overview_hierarchy$Trait_NZ_pp_harmonized
# perc_gen <- dat_NZ[nr_unique_genus <= 5,
#                     round(.N / nrow(dat_NZ) * 100,
#                           digits = 2)]
# tax_hierarchy_plot(plotting_data = dat_NZ,
#                    label_unique_genera = 5, 
#                    y_lim_max = 100,
#                    annotate_region = "NZ")
# for(link in c(data_out, data_paper)) {
#   ggplot2::ggsave(
#     filename = file.path(link, "taxonomic_hierarchy_NZ.png"),
#     width = 22,
#     height = 12,
#     units = "cm"
#   )
# }

#### Extreme cases ####

# How often do we find an "extreme" taxonomic hierarchy? 
# I.e. one or few genera with a high number of species
dat_AUS[, genus_spec_ratio := nr_unique_genus/entry_on_species]
dat_AUS[genus_spec_ratio < 0.5, ]

preproc_cp$Trait_AUS_harmonized[family == "Notonectidae", ]








  
  
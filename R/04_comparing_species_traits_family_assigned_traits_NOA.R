# _____________________________________________________________________________
# Here two trait aggregation methods are compared
# to traits assigned on family level for the North American trait databbase
# TODO check if there are rows with just zeros
# North American trait data from Matt Pyne (?)
# TODO: Source
# _____________________________________________________________________________

#### Data preprocessing ####
noa_trait_matrix <-
  read_excel(
    path = file.path(".", "Data", "American Trait Matrix_upgraded(2014).xlsx"),
    sheet = 2
  ) %>%
  setDT(.) %>%
  .[, -("...1")] %>%
  .[, .(Order, Family, Volt, Size, Habt, Trop, Resp)]

# check traits per grouping feature
# describe(noa_trait_matrix)

# rm NAs
noa_trait_matrix <- na.omit(noa_trait_matrix)

# convert categories to presence-absence of trait 
noa_trait_matrix_lf <- melt(noa_trait_matrix, id.vars = c("Family", "Order"))
noa_trait_matrix <- dcast(
  noa_trait_matrix_lf,
  Order + Family ~ value,
  value.var = "value",
  fun.aggregate = length
)

# change col names
setnames(
  noa_trait_matrix,
  old = c(
    "Habt.Burrow",
    "Habt.Climb",
    "Habt.Cling",
    "Habt.Skate",
    "Habt.Sprawl",
    "Habt.Swim",
    "Resp.Air",
    "Resp.Gills",
    "Resp.Tegument",
    "Size.Large",
    "Size.Medium",
    "Size.Small",
    "Trop.CollectorFilterer",
    "Trop.CollectorGatherer",
    "Trop.Herbivore",
    "Trop.Predator",
    "Trop.Shredder",
    "Volt.MultiVoltine",
    "Volt.SemiVoltine",
    "Volt.UniVoltine",
    "Family",
    "Order"
  ),
  new = c(
    "locom_burrow",
    "locom_climb",
    "locom_cling",
    "locom_skate",
    "locom_sprawl",
    "locom_swim",
    "resp_pls_spi",
    "resp_gil",
    "resp_teg",
    "size_large",
    "size_medium",
    "size_small",
    "feed_filter",
    "feed_gatherer",
    "feed_herbivore",
    "feed_predator",
    "feed_shredder",
    "volt_bi_multi",
    "volt_semi",
    "volt_uni",
    "family",
    "order"
  )
)

# harmonize locomotion so that this grouping features matches with the harmonized trait set
# sessil does not exist as category
noa_trait_matrix[, locom_swim := apply(.SD, 1, max),
                 .SDcols = c("locom_swim", "locom_skate")]
noa_trait_matrix[, locom_crawl := apply(.SD, 1, max),
                 .SDcols = c("locom_sprawl", "locom_climb", "locom_cling")]
# rm unnecessary cols
noa_trait_matrix[, c("locom_skate", "locom_sprawl", "locom_climb", "locom_cling") := NULL]

# convert back to lf 
# NOA data just contain aquatic insects
noa_trait_matrix <- melt(noa_trait_matrix, id.vars = c("order", "family"))

# merge together with values from trait aggregation
traitval_noa <-
  merge(results_agg[["Traits_US_LauraT_pp_harmonized"]],
        noa_trait_matrix,
        by = c("family", "variable", "order"))

setnames(traitval_noa,
         old = c(
           "deviance",
           "value"
         ),
         new = c(
           "deviance_complex_direct_agg",
           "value_famlvl"
         )
)

# create grouping feature column
traitval_noa[, grouping_feature := sub("(\\w)(\\_)(.+)", "\\1", variable)]

# create subset restricted to certain orders
traitval_noa_aqi <- traitval_noa[order %in% c(
  "Ephemeroptera",
  "Hemiptera",
  "Odonata",
  "Trichoptera",
  "Coleoptera",
  "Plecoptera",
  "Diptera",
  "Lepidoptera",
  "Megaloptera",
  "Neuroptera"
), ]

# _______________________________________________________________________
#### Analysis ####
# _______________________________________________________________________

# calc deviances to family assignments
traitval_noa[, `:=`(
  deviance_dir_fam = value_direct_agg - value_famlvl,
  deviance_comp_fam = value_genus_fam_agg - value_famlvl
)]

# How many cases overall have been evaluated differently by Chessman?
# 22 % and 21 % respectively 
nrow(traitval_noa[deviance_dir_fam != 0, ]) / nrow(traitval_noa)
nrow(traitval_noa[deviance_comp_fam != 0, ]) / nrow(traitval_noa)

# Which traits?
# size_medium 33 times,...
traitval_noa[deviance_comp_fam != 0, .(.N), by = c("variable")] %>% 
  .[order(-N),]
traitval_noa[deviance_comp_fam != 0 & variable %in% "size_medium", ] %>%
  .[, .(.N), by = order] %>% 
  .[order(-N),]

# Regarding deviating classification (trait_val compl_agg > fam_assignment)
# and vice versa: no tendency, almost equal
traitval_noa[deviance_comp_fam > 0, .N, by = "order"] %>%
  .[order(-N), ]
traitval_noa[deviance_comp_fam < 0, .N, by = "order"] %>%
  .[order(-N), ]

# Which orders?
# Ephmeroptera, Diptera
traitval_noa[deviance_comp_fam != 0, .(.N), by = c("order"), ] %>%
  .[order(-N), ]

# How many taxa/cases are classified differntly per order?
nrow(traitval_noa[deviance_comp_fam != 0 &
                    order %in% "Ephemeroptera", ]) / nrow(traitval_noa[order %in% "Ephemeroptera", ])
nrow(traitval_noa[deviance_comp_fam != 0 &
                    order %in% "Diptera", ]) / nrow(traitval_noa[order %in% "Diptera", ])
nrow(traitval_noa[deviance_comp_fam != 0 &
                    order %in% "Trichoptera", ]) / nrow(traitval_noa[order %in% "Trichoptera", ])
nrow(traitval_noa[deviance_comp_fam != 0 &
                    order %in% "Coleoptera", ]) / nrow(traitval_noa[order %in% "Coleoptera", ])


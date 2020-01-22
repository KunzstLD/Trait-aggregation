# _____________________________________________________________________________
#### Aggregation ####
# Here two trait aggregation methods are compared
# to traits assigned on family level
# ?For later, include respiration information:
# functional spiracles -> resp_spi
# air respiration   -> resp_spi
# gills -> resp_gil
# TODO check rows with just zeros
# _____________________________________________________________________________


#### AUS ####
chessman_raw <- read_excel(
  path = file.path(
    ".",
    "Data",
    "Chessman part 2.xlsx"
  ),
  skip = 1
)
setDT(chessman_raw)

# subset to feeding information, taxonomical information & size
chessman_raw <- chessman_raw[, .SD,
  .SDcols = names(chessman_raw) %like% "Order|Family|.*feeding|.*length"
]

# Classify continuous varibale length as size
chessman_raw[, `:=`(
  size_small = ifelse(`Maximum length (mm)` < 9,
    1, 0
  ),
  size_medium = ifelse(
    `Maximum length (mm)` >= 9 &
      `Maximum length (mm)` <= 16,
    1,
    0
  ),
  size_large = ifelse(`Maximum length (mm)` > 16,
    1, 0
  )
)]

# change col names so that data can be merged
# with results from other aggregations
setnames(chessman_raw,
  old = c(
    "Order",
    "Family",
    "Shredder (proportion of feeding)",
    "Scraper (proportion of feeding)",
    "Predator (proportion of feeding)",
    "Gatherer (proportion of feeding)",
    "Filterer (proportion of feeding)"
  ),
  new = c(
    "order",
    "family",
    "feed_shredder",
    "feed_herbivore",
    "feed_predator",
    "feed_gatherer",
    "feed_filter"
  )
)

# del maximum length variable
chessman_raw[, `Maximum length (mm)` := NULL]

# transform to lf
chessman_raw <- melt(chessman_raw, id.vars = c("family", "order"))

# combine with results from other aggregation methods
traitval_aus <- merge(
  results_agg[["Trait_AUS_harmonized.rds"]],
  chessman_raw,
  by = c("family", "variable", "order")
)

setnames(traitval_aus,
  old = c(
    "deviance",
    "value"
  ),
  new = c(
    "deviance_complex_direct_agg",
    "value_famlvl"
  )
)

# _____________________________________________________________________________
#### Analysis
# TODO restrict to certain orders
# _____________________________________________________________________________

# For Australia
traitval_aus[, `:=`(
  deviance_dir_fam = value_direct_agg - value_famlvl,
  deviance_comp_fam = value_genus_fam_agg - value_famlvl
)]

# How many cases have been evaluated differently by Chessman?
nrow(traitval_aus[deviance_dir_fam != 0, ]) / nrow(traitval_aus)
nrow(traitval_aus[deviance_comp_fam != 0, ]) / nrow(traitval_aus)

# Which traits?
# feed_shredder 44 times classified differently
# size_medium 43,....
traitval_aus[deviance_comp_fam != 0, .(.N), by = c("variable"), ]
traitval_aus[deviance_comp_fam != 0 & variable %in% "feed_shredder", ] %>%
  .[, .(.N), by = order]

# Which orders?
# Diptera
# Trichoptera
# Coleoptera,...
traitval_aus[deviance_comp_fam != 0, .(.N), by = c("order"), ] %>%
  .[order(-N), ]

# TODO: Make function out of this!
# About 50 % of Diptera differently classified regarding the traits feeding and size
nrow(traitval_aus[deviance_comp_fam != 0 & order %in% "Diptera", ])/nrow(traitval_aus[order %in% "Diptera", ])
# 42 % of Trichoptera differently classified
nrow(traitval_aus[deviance_comp_fam != 0 & order %in% "Trichoptera", ])/nrow(traitval_aus[order %in% "Trichoptera", ])
# 29 % of Coleoptera differntly classified 
nrow(traitval_aus[deviance_comp_fam != 0 & order %in% "Coleoptera", ])/nrow(traitval_aus[order %in% "Coleoptera", ])
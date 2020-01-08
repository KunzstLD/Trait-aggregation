# Script that compares two trait aggregation methods
# 1) Species - Genus - Family (using Mode)
# 2) Species - Family
# TODO:
# Provide Details on aggregation methods
# Write functions for aggregation
# compare accross regions

# Methods used:
# Aggregation over genus and family (using median and mode)
# Aggregation directly to family level
# (Aggregation using Power Average Operator?)

# Test for:
# fuzzy coded data
# binary coded data
# binary data transformed to fuzzy coded (on genus level, by nr. of species)

# _________________________________________________________________________
#### 1) Species - Genus - Family (using Mode) ####
# - Aggregation procedure:
# Median to Genus/
# Mode for Family
# -> differing amount of values to calculate value for each modality
# sum(table(AUS_subset[grepl("Chirono.*", family)]$temp_eurytherm))

# Poff et al. 2006: most common genus-level modality assigned (genus level
# trait data with family-level monitoring data)
# which(table(AUS_subset$genus) == max(table(AUS_subset$genus), na.rm = TRUE))
# _________________________________________________________________________

# get names of trait columns
trait_col <- names(AUS_subset[, -c("family",
                                  "genus",
                                  "species",
                                  "order")])

# subset so that no NA values occur in Species data
# (otherwise all NA entries are viewed as a group &
# aggregated as well)
AUS_subset[!is.na(species),
    (trait_col) := lapply(.SD, median, na.rm = TRUE),
    .SDcols = trait_col,
    by = genus
]

# _________________________________________________________________________
#### Aggregate to family level ####
# take mode if duplicates, otherwise maximum
# test <- AUS_subset_genus[, lapply(.SD, Mode, na.rm = TRUE),
#                .SDcols = names(AUS_subset_genus) %like% "^temp",
#                by = "family"]
# Trait_fam <- AUS_subset_genus[, c(lapply(.SD, function(y) {
#   if (length(unique(y)) == length(y) & length(y) > 1) {
#     max(y)
#   } else {
#     Mode(x = y)
#   }
# }), .N),
# .SDcols = names(AUS_subset_genus) %like% pat_traitname,
# by = family]
# _________________________________________________________________________
AUS_subset[, c(lapply(.SD, function(y) {
  if (length(unique(y)) == length(y) & length(y) > 1) {
    max(y, na.rm = TRUE)
    # e.g. in case (0,0,3)
  } else if (Mode(y, na.rm = TRUE) == 0 & !all((y) == 0))  {
    Mode(y[y != 0], na.rm = TRUE)
  }
  else{
    Mode(y, na.rm = TRUE)
  }
}), .N),
.SDcols = trait_col,
by = "family"]
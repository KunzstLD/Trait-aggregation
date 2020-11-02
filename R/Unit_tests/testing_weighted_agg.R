#---- weighted average ----#

# manual testing:
test <- trait_dat$Trait_AUS_harmonized.rds
test_preproc <- copy(preproc_dat$Trait_AUS_harmonized)

# weight genera after species
# init 
N_species_init <- test[!is.na(species), .N, by = "genus"]

# preproc
N_species_preproc <- test_preproc[!is.na(species), .(species,
                                                     family,
                                                     order,
                                                     .N), by = "genus"]
# merge
weight_species <- merge(
  N_species_preproc,
  N_species_init,
  by = "genus",
  suffixes = c("_preproc", "_init")
)

# calculate weight
weight_species[, coeff := N_preproc / N_init]

# Second step: 
# merge weights with preproc data and aggregate

# define trait cols
trait_col <- grep("species|genus|family|order|coeff", 
                  names(test_preproc),
                  value = TRUE,
                  invert = TRUE)

# merge with preproc dat
test_AUS <- test$Trait_AUS_harmonized.rds
preproc_AUS <- preproc_dat$Trait_AUS_harmonized

# manual:
# merge 
preproc_AUS[test_AUS,
            `:=`(coeff = i.coeff),
            on = "species"]

# put 1 as coeff where NA (genus or family entries)
preproc_AUS[is.na(coeff), coeff := 1]

# define trait cols and calculate weighted mean
preproc_AUS[, lapply(.SD, function(y) weighted.mean(y, coeff)), 
            .SDcols = trait_col,
            by = "family"]

#---- mean ----#
test_preproc[, lapply(.SD, mean), .SDcols = trait_col,
             by = "family"]

#---- median ----#
test_preproc[, lapply(.SD, median), .SDcols = trait_col,
             by = "family"]

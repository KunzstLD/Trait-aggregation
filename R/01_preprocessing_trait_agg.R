# _________________________________________________________________________
#### Preprocessing ####
# TODO: investigate fuzzy coded version as well
# _________________________________________________________________________

# load
trait_dat <- load_data(path = "./Data/", pattern = "harmonized\\.rds")

# taxonomical corrections
source(file = "./R/taxonomical_corrections.R")

# feeding mode parasite: not present in NZ data
# Add these taxa to critical taxa table!
# lapply(trait_dat[c("Trait_AUS_harmonized.rds",
#                    "Trait_EU_pp_harmonized.rds",
#                    "Traits_US_LauraT_pp_harmonized.rds")],
#        function(y) y[feed_parasite > 0, ])
trait_dat$Trait_EU_pp_harmonized.rds[,  feed_parasite := NULL]
trait_dat$Trait_AUS_harmonized.rds[,  feed_parasite := NULL]
trait_dat$Traits_US_LauraT_pp_harmonized.rds[,  feed_parasite := NULL]

# chose traits, normalize and omit incomplete information
# for trait aggregation comparison
preproc_dat <- trait_dat %>%
  lapply(., function(y) {
    y[, .SD,
      .SDcols = names(y) %like% "species|genus|family|order|feed.+|resp.+|locom.+|volt.+|size.+|ovip.+|bf.+"] %>%
      normalize_by_rowSum(.,
                          non_trait_cols = c("species",
                                             "genus",
                                             "family",
                                             "order")) %>%
      .[!is.na(family),]
  })

# improve names
names(preproc_dat) <- sub("(.+)(\\.rds)", "\\1", names(preproc_dat))
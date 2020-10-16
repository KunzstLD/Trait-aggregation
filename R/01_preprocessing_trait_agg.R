# _________________________________________________________________________
#### Preprocessing ####
# TODO: investigate fuzzy coded version as well
# _________________________________________________________________________

# load
trait_dat <- load_data(path = "./Data/", pattern = "harmonized\\.rds")

# taxonomic corrections
source(file = "./R/taxonomical_corrections.R")

# feeding mode parasite: not present in NZ data 
# has been checked, probably most of the taxa there are no parasites
# 0 assigned
trait_dat$Trait_NZ_pp_harmonized.rds[, feed_parasite := 0]

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
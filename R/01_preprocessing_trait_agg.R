# _________________________________________________________________________
#### Preprocessing ####
# TODO: investigate fuzzy coded version as well
# _________________________________________________________________________
trait_dat <- load_data(path = "./Data/", pattern = "harmonized\\.rds")

# chose traits, normalize and omit incomplete information
# for trait aggregation comparison
# TODO: Only choose traits with sufficient coverage?
preproc_dat <- trait_dat %>%
  lapply(., function(y) {
    y[, .SD,
      .SDcols = names(y) %like% "species|genus|family|order|feed.+|resp.+|locom.+|bf.+|volt.+|size.+|ovip.+"
    ] %>%
      normalize_by_rowSum(.,
        non_trait_cols = c(
          "species",
          "genus",
          "family",
          "order"
        )
      ) %>%
      na.omit(., cols = names(.[, -c(
        "species",
        "genus",
        "family",
        "order"
      )])) %>% 
      .[!is.na(family), ]
  })

# improve names
names(preproc_dat) <- sub("(.+)(\\.rds)", "\\1", names(preproc_dat))

# feeding mode parasite:
# remove trait and normalize again
lapply(preproc_dat[c("Trait_AUS_harmonized",
                   "Trait_EU_pp_harmonized",
                   "Traits_US_LauraT_pp_harmonized")],
       function(y) y[feed_parasite > 0, ])
preproc_dat$Trait_EU_pp_harmonized[,  feed_parasite := NULL]
preproc_dat$Trait_AUS_harmonized[,  feed_parasite := NULL]
preproc_dat$Traits_US_LauraT_pp_harmonized[,  feed_parasite := NULL]

# normalize Europe data again
normalize_by_rowSum(preproc_dat$Trait_EU_pp_harmonized, 
                    non_trait_cols = c(
                      "species",
                      "genus",
                      "family",
                      "order"))
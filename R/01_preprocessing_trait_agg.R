# _________________________________________________________________________
#### Preprocessing ####
# _________________________________________________________________________
trait_dat <- load_data(path = "./Data/", pattern = "harmonized.*\\.rds")

# choose traits, normalize and omit incomplete information
preproc_dat <- trait_dat %>%
  lapply(., function(y) {
    y[, .SD,
      .SDcols = names(y) %like% "species|genus|family|order|feed.+|locom.+|size.+|volt.+|resp.+|bf.+"
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

#hi Snucks
#Schnucks, ich bau ein Haus in Panama.

# _________________________________________________________________________
#### Preprocessing ####
# _________________________________________________________________________

trait_dat <- load_data(path = "./Data/", pattern = "harmonized\\.rds")

preproc_dat <- trait_dat %>%
  lapply(., function(y) {
    y[, .SD,
      .SDcols = names(y) %like% "species|genus|family|order|feed.+|locom.+|size.+|volt.+|resp.+"
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
      )]))
  })

# AUS_subset <- Trait_AUS %>%
#   .[, .SD, .SDcols = names(.) %like% "species|genus|family|order|
#   feed.+|locom.+|size.+|volt.+|resp.+"] %>%
#   normalize_by_rowSum(.,
#     non_trait_cols = c(
#       "species",
#       "genus",
#       "family",
#       "order"
#     )
#   ) %>%
#   na.omit(., cols = names(.[, -c(
#     "species",
#     "genus",
#     "family",
#     "order"
#   )]))


# # read in AST traits
# Trait_AUS <- readRDS(
#   file = file.path(
#     ".",
#     "Data",
#     "Trait_AUS_harmonized.rds"
#   )
# )

# search for candidate families
# Trait_AUS[, .(.N), by = family] %>%
#   .[order(N), ] %>%
#   tail(., n = 20)
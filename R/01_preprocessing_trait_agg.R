# _________________________________________________________________________
#### Preprocessing ####
# _________________________________________________________________________

# read in AST traits
Trait_AUS <- readRDS(
  file = file.path(
    ".",
    "Data",
    "Australia",
    "Trait_AUS_harmonized.rds"
  )
)

# search for candidate families
Trait_AUS[, .(.N), by = family] %>% 
  .[order(N), ] %>% 
  tail(., n = 20)

# create subset 
candidates <- c("Chironomidae", "Elmidae", "Dytiscidae", "Ceratopogonidae", "Tipulidae")

AUS_subset <- Trait_AUS[!is.na(genus), ] %>%
  melt(., id.vars = c("unique_id", "species", "genus", "family", "order")) %>%
  .[variable %like% "feed.+|locom.+|size.+|volt.+|resp.+", ] %>%
  .[family %in% candidates, ] %>%
  dcast(.,
        unique_id + species + genus + family + order ~ variable,
        value.var = "value") %>%
  normalize_by_rowSum(.,
                      non_trait_cols = c("unique_id",
                                         "species",
                                         "genus",
                                         "family",
                                         "order")) %>%
  na.omit(., cols = names(.[, -c("unique_id",
                                 "species",
                                 "genus",
                                 "family",
                                 "order")]))

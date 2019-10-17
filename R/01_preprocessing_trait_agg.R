# ----------------------------------------------------------------------------
#### Preprocessing ####
# ----------------------------------------------------------------------------

# read in AST traits
Trait_AUS <- readRDS(
  file = file.path(
    ".",
    "Data",
    "Australia",
    "Trait_AUS_harmonized.rds"
  )
)

# create subset 
# Candidates: "Hydrophilidae", "Elmidae", "Dytiscidae", "Gripopterygidae"
candidates <- c("Hydrophilidae", "Gripopterygidae", "Elmidae", "Dytiscidae")

AUS_subset <- Trait_AUS[!is.na(species),] %>%
  melt(., id.vars = c("unique_id", "species", "genus", "family", "order")) %>%
  .[variable %like% "feed.+|locom.+|size.+|volt.+|resp.+|ovip.+",] %>%
  .[family %in% candidates,] %>%
  dcast(.,
        unique_id + species + genus + family + order ~ variable,
        value.var = "value")


# 1) test how complete trait sets are

# get trait columns
trait_col <-
  grep(
    "feed.+|locom.+|size.+|volt.+|resp.+|ovip.+",
    names(AUS_subset),
    value = TRUE
  )

# just get the trait name
name_vec <- sub("\\_.*", "", trait_col) %>% unique()

# output matrix used in for loop
output <- matrix(ncol = 2, nrow = length(name_vec))
for (i in seq_along(name_vec)) {
  vec <- AUS_subset[, base::sum(.SD) == 0,
                    .SDcols = names(AUS_subset) %like% name_vec[i],
                    by = 1:nrow(AUS_subset)]$V1
  
  # percentage of how many entries per trait lack information
  output[i,] <-
    c(round((sum(vec) / nrow(AUS_subset)) * 100), name_vec[i])
}
output

# just return rows where for each trait there is an observation 
data <- get_complete_trait_data(
  trait_data = AUS_subset,
  non_trait_col = c("unique_Id",
                    "species",
                    "genus",
                    "family",
                    "order")
)

# merge dt in list together
AST_subset <- Reduce(merge, data[c("locom",
                                   "feed",
                                   "resp",
                                   "volt",
                                   "ovip",
                                   "size")])

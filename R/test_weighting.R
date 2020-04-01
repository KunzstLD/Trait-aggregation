#### IDEA: 1 ####
# species per genera
# Introduce coefficient for spec-genus aggr. based on
# proportion of initial amount of species per genus
# vs. amount of species entries in preproc dataset
# Makes maybe sense only for some traits?
test <- preproc_dat[["Trait_AUS_harmonized"]] 
nr_taxa <- test[!is.na(genus), .N, by = "genus"] 

weighting <- merge(nr_taxa,
                   trait_dat[["Trait_AUS_harmonized.rds"]] %>%
                     .[!is.na(genus), .(family, .N), by = "genus"],
                   by = "genus",
                   suffixes = c("_preproc", "_init")) %>%
  .[, .(genus,
        family,
        N_preproc,
        N_init,
        Coeff = N_preproc / N_init)] %>%
  .[, .(family, genus, Coeff, N_init, N_preproc)] %>%
  .[!duplicated(genus),]
weighting[, sum_Coeff := sum(Coeff), 
          by = "family"] 
weighting[, stand_Coeff := Coeff/sum_Coeff]

# first aggr step: 
trait_col <- c("volt_bi_multi", "volt_semi", "volt_uni")

# merge weighting
test[weighting,
     `:=`(coeff = i.Coeff,
          stand_coeff = i.stand_Coeff),
     on = "genus"]

# Procedure: Trait values on species & genus-level are aggregated
# using weights* Trait_value per genera
weighting[family %in% "Hydrobiosidae", ]

test[family %in% "Hydrobiosidae", .(volt_bi_multi, volt_semi, volt_uni,
                                    species, genus, family, order,
                                    coeff, stand_coeff)]

test[!is.na(genus), lapply(.SD, function(y)
         y * stand_coeff),
     .SDcols = trait_col,
     by = "genus"] %>% 
  .[!duplicated(genus), ] %>% 
  .[, lapply(.SD, sum), 
    .SDcols = trait_col,
    by = "family"] 

test[family %in% "Hydrobiosidae" &
       !is.na(genus),]

test[family %in% "Hydrobiosidae" &
       !is.na(genus), lapply(.SD, function(y)
         y * coeff),
     .SDcols = trait_col,
     by = "genus"] %>% 
  .[, lapply(.SD, sum), 
    .SDcols = trait_col]


test[!is.na(genus), (trait_col) := lapply(.SD, function(y)
  y * stand_coeff),
  .SDcols = trait_col,
  by = "genus"] 

test[!duplicated(genus), ] %>% 
  .[, lapply(.SD, sum), 
    .SDcols = trait_col,
    by = "family"] 

# merge information on order level back
# ...


#### IDEA 2: Intorduce weighting based on species ####
# in genera and genera per family
# e.g. Gripopterygidae aggr. by 5 genera with varying number of species
# Dinopterla 5/15, Illiesoperla 1/15, Leptoperla 1/15, Riekoperla 6/15,
# Trinotoperla 2/15
test[family %in% "Gripopterygidae", ] %>% 
  .[order(genus), ]

# genera per family     
test_genus[!duplicated(genus), ] %>% 
  .[order(family), ]
test_genus[!duplicated(genus), .N, by = "family"]

test_genus[, c(lapply(.SD, function(y) {
  # for cases like c(0,1), c(0,0,1,1) and c(0,1,0.5)
  if (length(unique(y)) == length(y) |
      sum(duplicated(y)) == sum(!duplicated(y))) {
    median(y, na.rm = TRUE)
    # e.g. in case (0,0,3)
    # } else if (Mode(y, na.rm = TRUE) == 0 & !all((y) == 0)) {
    #   Mode(y[y != 0], na.rm = TRUE)
  }
  else {
    Mode(y, na.rm = TRUE)
  }
})),
  .SDcols = trait_col,
  by = "family"]
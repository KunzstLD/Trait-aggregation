# _________________________________________________________________________
#### basic analysis of trait datasets ####
# Completeness of trait data 
# Information on taxonomical coverage
# _________________________________________________________________________

# _________________________________________________________________________
# completeness of trait data
# _________________________________________________________________________
completeness <- trait_dat %>%
  lapply(., function(y) {
    y[, .SD,
      .SDcols = names(y) %like% "species|genus|family|order|feed.+|locom.+|size.+|volt.+|resp.+|bf.+|ovip.+"
      ] %>% 
      normalize_by_rowSum(.,
                          non_trait_cols = c(
                            "species",
                            "genus",
                            "family",
                            "order"
                          )
      ) %>% 
      completeness_trait_data(x = ., 
                              non_trait_cols = c("species", 
                                                 "genus",
                                                 "family",
                                                 "order"))}
  )
completeness <- lapply(completeness, as.data.table) %>%
  rbindlist(., idcol = "file") 

# change colnames
setnames(completeness,
         old = c("file",
                 "V1",
                 "V2"),
         new = c("Database",
                 "Trait covered [%]",
                 "Grouping feature"))

# create look up tables 
look_up_db <- data.table(
  Key_col = c(
    "Trait_AUS_harmonized.rds",
    "Trait_freshecol_2020_pp_harmonized.rds",
    "Trait_NZ_pp_harmonized.rds",
    "Traits_US_LauraT_pp_harmonized.rds"
  ),
  Database = c("Australia",
               "Europe",
               "New Zealand",
               "North America")
)
look_up_grf <- data.table(
  Key_col = c("^bf",
              "^locom",
              "^size",
              "^volt",
              "^feed",
              "^resp", 
              "^ovip"),
  Grouping_feature = c(
    "Body form",
    "Locomotion",
    "Size",
    "Voltinism",
    "Feeding mode",
    "Respiration",
    "Oviposition"
  )
)

# merge
completeness[look_up_db,
     `:=`(Database = i.Database), 
     on = c(Database = "Key_col")]
completeness[look_up_grf,
     `:=`(`Grouping feature` = i.Grouping_feature),
     on = c(`Grouping feature` = "Key_col")]

# convert Trait covered to numeric
completeness[, `Trait covered [%]` := as.numeric(`Trait covered [%]`)]

# which grf and which db has highest "cumulative" completeness in % 
completeness[, base::sum(`Trait covered [%]`), by = "Grouping feature"] %>%
  .[order(V1), `Grouping feature`]
target <-
  completeness[, base::sum(`Trait covered [%]`), by = "Database"] %>%
  .[order(V1), Database]

# wide format
completeness_output <- dcast(data = completeness,
                      Database ~  `Grouping feature`,
                      value.var = "Trait covered [%]") %>%
  .[match(target, Database),]

# change col order 
setcolorder(
  completeness_output,
  neworder = c(
    "Database",
    "Body form",
    "Oviposition",
    "Voltinism",
    "Locomotion",
    "Size",
    "Respiration",
    "Feeding mode"
  )
)

# latex output
xtable_wo_rownames(
  x = completeness_output,
  caption = "Percentage of entries that have
        information for the individual grouping features
        shown per trait dataset",
  label = "tab:trait_coverage",
  digits = 0
)


# _________________________________________________________________________
#### Taxonomic coverage ####
# removed taxa on a higher level
# _________________________________________________________________________

# overview
# lapply(trait_dat, function(y) Hmisc::describe(y))

# What we need: 
# - taxa pool 
TaxaPool <- lapply(trait_dat, function(y)
  y[, .N]) %>% 
  lapply(., as.data.table) %>%
  rbindlist(.,
            idcol = "id")

# How many resolved on species, genus and family-level?
TaxLvl <- lapply(trait_dat, taxonomic_lvl) %>% 
  rbindlist(., idcol = "id")

# % of aquatic insects
# How much % of aquatic insects per Database?
orders_aq_ins <- c(
  "Ephemeroptera",
  "Hemiptera",
  "Odonata",
  "Trichoptera",
  "Coleoptera",
  "Plecoptera",
  "Diptera",
  "Lepidoptera",
  "Megaloptera",
  "Neuroptera"
)
AqIns <- lapply(trait_dat, function(y)
  y[order %in% orders_aq_ins, .N]) %>% 
  lapply(. , as.data.table) %>% 
  rbindlist(.,
            idcol = "id")
setnames(AqIns, "V1", "nr_aq_insects")

# combine 
TaxCoverage <- Reduce(merge, list(TaxaPool, 
                   TaxLvl,
                   AqIns))

# calculate relative frequencies
cols <- c("species", "genus", "family", "nr_aq_insects")
TaxCoverage[, c("species_rf",
                "genus_rf",
                "family_rf",
                "aq_insects_rf") := lapply(.SD, function(y)
                  round(y / V1 * 100, digits = 0)),
            .SDcols = cols]

# create char vectors by pasting together results
TaxCoverage[, `:=`(
  species = paste0(species, " (", species_rf, ")"),
  genus = paste0(genus, " (", genus_rf, ")"),
  family = paste0(family, " (", family_rf, ")"),
  nr_aq_insects = paste0(nr_aq_insects, " (", aq_insects_rf, ")")
)]

# change colnames
setnames(TaxCoverage,
         old = c("id", 
                 "V1", 
                 "species", 
                 "genus",
                 "family",
                 "nr_aq_insects"),
         new = c("Database", 
                 "Nr. of taxa",
                 "Species",
                 "Genus",
                 "Family",
                 "Nr. aquatic insects"))

# change database names
TaxCoverage[look_up_db,
            `:=`(Database = i.Database),
            on = c(Database = "Key_col")]

# change row order
TaxCoverage <- TaxCoverage[match(c("Europe",
                                   "North America",
                                   "Australia",
                                   "New Zealand"),
                                 Database),]

# rm not used columns
TaxCoverage[, c("species_rf", 
                "genus_rf",
                "family_rf",
                "aq_insects_rf") := NULL]

# latex output 
xtable_wo_rownames(x = TaxCoverage,
                   caption = "",
                   label = "tab:tax_coverage",
                   digits = 0)

#____________________________________________________________________________________________
#### Re-analyzing Szöcs et al. 2014 - data processing ####
#____________________________________________________________________________________________


#_______________________________________
#### Load and prepare data from Edi ####
#_______________________________________
ecor_data <- load_data(path = "./Data/Edi_salinity_study/cache",
                       pattern = "*.rds")

# multiple assignment
c(ecor_L, ecor_Q, ecor_R, ordos) %<-% ecor_data[c("ecor_L.rds",
                                                  "ecor_Q.rds",
                                                  "ecor_R.rds",
                                                  "ordos.rds")]

# rm "sp." and "spp." from ecor data
rownames(ecor_Q) <- sub(" sp\\.| spp\\.", "", rownames(ecor_Q))
names(ecor_L) <- sub(" sp\\.| spp\\.", "", names(ecor_L))

# correct one taxa name
rownames(ecor_Q)[rownames(ecor_Q) == "Stratiomyiidae"] <-
  "Stratiomyidae"
names(ecor_L)[names(ecor_L) == "Stratiomyiidae"]  <- "Stratiomyidae"

# save copy of ecor_Q to preserve original matrix
ecor_Q_cp <- ecor_Q

# load trait lookup from Edi
trait_edi_lookup <-
  fread(file.path("./Data/Edi_salinity_study/data/trait_lookup.csv"),
        sep = ";")
trait_edi_lookup[, ID_trait := paste0("X", ID_trait)]

# extend lookup table with harmonized traits
trait_ext_lookup <- rbind(trait_edi_lookup,
                          data.table(
                            ID = rep(c(22, 23, 24, 25, 26, 27),
                                     c(6, 4, 3, 3, 3, 3)),
                            ID_name = c(
                              "herbivore piercer/shredder & scraper (grazer)",
                              "collector gatherer (gatherers, detritivores)",
                              "predator (engulfers & pierce prey tissues)",
                              "shredder \n (chewers, \n miners, xylophagus, \n decomposing plants)",
                              "collector filterer (active filterers, passive filterers, absorbers)",
                              "parasite",
                              "swimmer, scater (active & passive)",
                              "burrower",
                              "crawlers, walkers & sprawlers",
                              "sessil (attached)",
                              "cutaneous/tegument",
                              "gills",
                              "plastron & spiracle",
                              "semivoltine",
                              "univoltine",
                              "bi- or multivoltine",
                              "terrestric eggs",
                              "aquatic eggs",
                              "ovoviviparity",
                              "size_small: size < 10 mm",
                              "size_medium: 10 mm <= size > 20 mm",
                              "size_large: EU: size >= 20 mm"
                            ),
                            ID_trait = c(
                              "feed_herbivore",
                              "feed_gatherer",
                              "feed_predator",
                              "feed_shredder",
                              "feed_filter",
                              "feed_parasite",
                              "locom_swim",
                              "locom_burrow",
                              "locom_crawl",
                              "locom_sessil",
                              "resp_teg",
                              "resp_gil",
                              "resp_pls_spi",
                              "volt_semi",
                              "volt_uni",
                              "volt_bi_multi",
                              "ovip_ter",
                              "ovip_aqu",
                              "ovip_ovo",
                              "size_large",
                              "size_medium",
                              "size_small"
                            ),
                            ID_trait_name = rep(
                              c(
                                "Feeding Mode harm.",
                                "Locomotion harm.",
                                "Respiration harm.",
                                "Voltinism harm.",
                                "Reproduction harm.",
                                "Body Size harm."
                              ),
                              c(6, 4, 3, 3, 3, 3)
                            )
                          ))

# load tachet Edi used back in the day
tachet_edi <-
  fread(file.path("./Data/Edi_salinity_study/data/tachet.csv"),
        sep = ";")
# create lookup vector
# names are the keys
# values are the vector entries
# lookup_vec <- trait_edi_lookup$ID_name
# names(lookup_vec) <- trait_edi_lookup$ID_trait
# colnames(ecor_Q) <- unname(lookup_vec[colnames(ecor_Q)])


#_______________________________________
#### Preprocess European trait data ####
#_______________________________________
trait_dat <-
  load_data(path = "./Data/", pattern = "harmonized.*\\.rds")

# European trait data are already normalized
trait_eu <- trait_dat[["Trait_freshecol_2020_pp_harmonized.rds"]]

# select traits
# taxon_cp includes taxa names from the raw data 
cols <- "feed|locom|resp|volt|ovip|size|order|family|genus|species|taxon_cp"
trait_eu_subset <-
  trait_eu[, .SD, .SDcols = names(trait_eu) %like% cols]

# change col order
setcolorder(
  x = trait_eu_subset,
  neworder = c(
    "order",
    "family",
    "genus",
    "species",
    "feed_herbivore",
    "feed_gatherer",
    "feed_predator",
    "feed_shredder",
    "feed_filter",
    "feed_parasite",
    "locom_swim",
    "locom_burrow",
    "locom_crawl",
    "locom_sessil",
    "resp_teg",
    "resp_gil",
    "resp_pls_spi"
  )
)


#_______________________________________
#### Preparation for re-analysis ####
# few taxa have to be re-named or re-assigned
#_______________________________________

# Chaetopterygini to genus column
trait_eu_subset[taxon_cp %like% "Chaetopterygini", genus := "Chaetopterygini"]

# ORTHOCLADIINAE/DIAMESINAE is also PRODIAMESINAE
trait_eu_subset[taxon_cp %like% "Orthocladiinae", genus := "Prodiamesinae"]

# Pediciinae subfamily moved to genus
# trait_eu_subset[taxon_cp %like% "Pediciinae.*", genus := "Pediciinae"]

# Some taxa on subfamily level re-assigned to genus-lvl to merge them
# later to ecor_Q
search <- paste0(
  c(
    "Chironomini",
    "Limnephilini",
    "Tanypodinae",
    "Tanytarsini",
    "Hemerodromiinae",
    "Clinocerinae",
    "Pediciinae"
  ),
  collapse = "|"
)
trait_eu_subset[taxon_cp %like% search, genus := sub(" Gen\\. sp\\.", "", taxon_cp)]

# add taxa column for merge
trait_eu_subset[, taxa := coalesce(species, genus, family, order)]

# taxa that are on sub-family/family-lvl in ecor_L:
# (and genus or species-level in tachet/freshwaterecol)
# need to be aggregated via median (likewise in Szöcs et al. 2014)
trait_cols <- grep("order|family|genus|species|tax.*", 
                   names(trait_eu_subset),
                   value = TRUE,
                   invert = TRUE)
agg_traits <- trait_eu_subset[family %in% c(
  "Ceratopogonidae",
  "Empididae",
  "Lepidostomatidae",
  "Limoniidae",
  "Perlodidae",
  "Prodiamesinae",
  "Psychomyiidae",
  "Spongillidae",
  "Chironomidae",
  "Tubificidae",
  "Limnephilidae",
  "Coenagrionidae"
), lapply(.SD, median, na.rm = TRUE), .SDcols = trait_cols, by = "family"] %>%
  normalize_by_rowSum(
    x = .,
    non_trait_cols = c("order",
                       "family",
                       "genus",
                       "species",
                       "taxa",
                       "taxon_cp")
  )

# create taxa column
agg_traits[, taxa := family]

# aggregate all Oligochaeta taxa according to Szöcs et al. 2014
# -> are actually (sub)class
trait_eu_subset[family %in% c(
  "Haplotaxidae",
  "Tubificidae",
  "Enchytraeidae",
  "Propappidae",
  "Lumbriculidae",
  "Dorydrilidae",
  "Lumbricidae",
  "Sparganophilidae",
  "Branchiobdellidae"
),   sub_class := "Oligochaeta"]

# TODO: Can just aggregate Oligochaeta data in tachet Version from Edi and check
# with the values in the cache file
trait_eu_oligo <- trait_eu_subset[sub_class %in% "Oligochaeta",
                                  lapply(.SD, median, na.rm = TRUE),
                                  .SDcols = trait_cols,
                                  by = "sub_class"] %>% 
  normalize_by_rowSum(x = .,
                    non_trait_cols = "sub_class")
trait_eu_oligo[, taxa := "Oligochaeta"]

# Still two taxa not present in fwe:
# "Simuliini" extrapolated from "Simuliidae"
# "Lasiocephala basalis" inferred from family data
crit_taxa <-
  rbind(trait_eu_subset[is.na(species) &
                          is.na(genus) & family == "Simuliidae",],
        trait_eu_subset[is.na(species) &
                          is.na(genus) & family %like% "Lepidostomatidae", ])
crit_taxa[family == "Simuliidae", taxa := "Simuliini"]
crit_taxa[family == "Lepidostomatidae", taxa := "Lasiocephala basalis"]

# rbind different taxa together
# rm those that are in agg_traits from trait_eu_subset
trait_eu_subset <- trait_eu_subset[!taxa %in% agg_traits$taxa, ]
trait_eu_sal <- rbind(trait_eu_subset, 
                      trait_eu_oligo, 
                      agg_traits,
                      crit_taxa,
                      fill = TRUE)

# few NA values in traits: set to 0 (according to Edi's Paper, missing information
# for traits was set to 0)
for(j in trait_cols) {
  data.table::set(trait_eu_sal, which(is.na(trait_eu_sal[[j]])), j, 0)
}

# change col order so that all trait columns of one grouping features
# are next to each other
setcolorder(
  trait_eu_sal,
  c(
    names(trait_eu_sal)[1:18],
    "volt_semi",
    "volt_uni",
    "volt_bi_multi",
    "ovip_ter",
    "ovip_aqu",
    "ovip_ovo"
  )
)

# rm those traits from ecor_Q that have been harmonized:
rm_col <-
  trait_edi_lookup[ID_trait_name %like% "(?i)size|locom|feed|resp|repr|number.*cycles",
                   ID_trait]
ecor_Q <- ecor_Q[, !names(ecor_Q) %in% rm_col]

# Normalize, could also use normalize_by_rowSum_df function
# calc. number of traits per grouping feature
vec <- sub("\\..*", "\\1", names(ecor_Q))
blocks <- rle(vec)$lengths
# convert to %-traits
ecor_Q <- prep.fuzzy.var(ecor_Q, blocks)

# merge to trait_eu_sal
ecor_Q$taxa <- rownames(ecor_Q)
trait_eu_sal <- base::merge(trait_eu_sal,
                            ecor_Q,
                            by = "taxa")
# rm duplicates
trait_eu_sal <- trait_eu_sal[!duplicated(taxa)]

# check if all taxa are covered
# rownames(ecor_Q)[!rownames(ecor_Q) %in% trait_eu_sal$taxa]


#_____________________________________________________________________________________
#### Create aggregated versions ####
#_____________________________________________________________________________________

# ___________________________________________________________________
# original data ####
# ___________________________________________________________________

# order alphabetically
ecor_Q_cp <- ecor_Q_cp[order(rownames(ecor_Q_cp)),]

# TODO: use normalize_by_rowSum_df function
# do the same for the original data:
# calc. number of traits per grouping feature
vec <- sub("\\..*", "\\1", names(ecor_Q_cp))
blocks <- rle(vec)$lengths

# convert to %-traits
ecor_Q_cp <- prep.fuzzy.var(ecor_Q_cp, blocks)


# ___________________________________________________________________
# stepwise agg median ####
# ___________________________________________________________________
eu_sal_stepwise_median <- spec_genus_agg_alt(
  trait_data = trait_eu_sal,
  non_trait_cols = c("taxa",
                     "sub_class",
                     "order",
                     "family",
                     "genus",
                     "species",
                     "taxon_cp"),
  method = median
) %>%
  merge(x = .,
        y = trait_eu_sal[taxa != "Oligochaeta", .(family, taxa)],
        by = "family",
        all = TRUE) %>%
  rbind(., trait_eu_sal[taxa == "Oligochaeta",],
        fill = TRUE) %>% 
  .[order(taxa), ]

# rm taxonomical information, create df and assign rownames 
eu_sal_stepwise_median[, c("sub_class",
                           "order",
                           "family",
                           "genus",
                           "species",
                           "taxon_cp") := NULL]
setDF(eu_sal_stepwise_median)
rownames(eu_sal_stepwise_median) <- eu_sal_stepwise_median$taxa
eu_sal_stepwise_median$taxa <- NULL

# ___________________________________________________________________
# stepwise agg mean ####
# ___________________________________________________________________
eu_sal_stepwise_mean <- spec_genus_agg_alt(
  trait_data = trait_eu_sal,
  non_trait_cols = c("taxa",
                     "sub_class",
                     "order",
                     "family",
                     "genus",
                     "species",
                     "taxon_cp"),
  method = mean
) %>%
  merge(x = .,
        y = trait_eu_sal[taxa != "Oligochaeta", .(family, taxa)],
        by = "family",
        all = TRUE) %>%
  rbind(., trait_eu_sal[taxa == "Oligochaeta", ],
        fill = TRUE) %>% 
  .[order(taxa), ]

# rm taxonomical information, create df and assign rownames 
eu_sal_stepwise_mean[, c("sub_class",
                         "order",
                         "family",
                         "genus",
                         "species",
                         "taxon_cp") := NULL]
setDF(eu_sal_stepwise_mean)
rownames(eu_sal_stepwise_mean) <- eu_sal_stepwise_mean$taxa
eu_sal_stepwise_mean$taxa <- NULL


# ___________________________________________________________________
# direct agg median ###
# ___________________________________________________________________
eu_sal_direct_median <- direct_agg(
  trait_data = trait_eu_sal,
  non_trait_cols = c("taxa",
                     "sub_class",
                     "order",
                     "family",
                     "genus",
                     "species",
                     "taxon_cp"),
  method = median
) %>%
  merge(x = .,
        y = trait_eu_sal[taxa != "Oligochaeta", .(family, taxa)],
        by = "family",
        all = TRUE) %>%
  rbind(., trait_eu_sal[taxa == "Oligochaeta",],
        fill = TRUE) %>% 
  .[order(taxa), ]

# rm taxonomical information, create df and assign rownames 
eu_sal_direct_median[, c("sub_class",
                         "order",
                         "family",
                         "genus",
                         "species",
                         "taxon_cp") := NULL]
setDF(eu_sal_direct_median)
rownames(eu_sal_direct_median) <- eu_sal_direct_median$taxa
eu_sal_direct_median$taxa <- NULL


# ___________________________________________________________________
# direct agg mean ####
# ___________________________________________________________________
eu_sal_direct_mean <- direct_agg(
  trait_data = trait_eu_sal,
  non_trait_cols = c("taxa",
                     "sub_class",
                     "order",
                     "family",
                     "genus",
                     "species",
                     "taxon_cp"),
  method = mean
) %>%
  merge(x = .,
        y = trait_eu_sal[taxa != "Oligochaeta", .(family, taxa)],
        by = "family",
        all = TRUE) %>%
  rbind(., trait_eu_sal[taxa == "Oligochaeta",],
        fill = TRUE) %>% 
  .[order(taxa), ]

# rm taxonomical information, create df and assign rownames 
eu_sal_direct_mean[, c("sub_class",
                       "order",
                       "family",
                       "genus",
                       "species",
                       "taxon_cp") := NULL]
setDF(eu_sal_direct_mean)
rownames(eu_sal_direct_mean) <- eu_sal_direct_mean$taxa
eu_sal_direct_mean$taxa <- NULL


# ___________________________________________________________________
# weighted agg ####
# ___________________________________________________________________
eu_sal_weighted <- weighted_agg(
  trait_data = trait_eu_sal,
  non_trait_cols = c("taxa",
                     "sub_class",
                     "order",
                     "family",
                     "genus",
                     "species",
                     "taxon_cp")
) %>%
  merge(x = .,
        y = trait_eu_sal[taxa != "Oligochaeta", .(family, taxa)],
        by = "family",
        all = TRUE) %>%
  rbind(., trait_eu_sal[taxa == "Oligochaeta", ],
        fill = TRUE) %>% 
  .[order(taxa), ]

# rm taxonomical information, create df and assign rownames 
eu_sal_weighted[, c("sub_class",
                    "order",
                    "family",
                    "genus",
                    "species",
                    "taxon_cp") := NULL]
setDF(eu_sal_weighted)
rownames(eu_sal_weighted) <- eu_sal_weighted$taxa
eu_sal_weighted$taxa <- NULL


# ___________________________________________________________________
# not aggregated but harmonized ####
# ___________________________________________________________________

# create dataset with taxonomical information
taxa <- trait_eu_sal[, .(taxa, sub_class, order, family, genus, species)]

# rm taxonomical information, create df and assign rownames 
trait_eu_sal[, c("sub_class",
                 "order",
                 "family",
                 "genus",
                 "species",
                 "taxon_cp") := NULL]
setDF(trait_eu_sal)
rownames(trait_eu_sal) <- trait_eu_sal$taxa
trait_eu_sal$taxa <- NULL


# ___________________________________________________________________
#### Final data processing ####
# ___________________________________________________________________

# Abundance data and ecor_Q_cp need to be ordered
# in the same way as the other datasets (otherwise weighted
# trait calculation gets wrong)
ecor_L <- ecor_L[, rownames(trait_eu_sal)] 

# finally combine all datasets 
agg_data <- list(
  stepw_median = eu_sal_stepwise_median,
  stepw_mean = eu_sal_stepwise_mean,
  direct_median = eu_sal_direct_median,
  direct_mean = eu_sal_direct_mean,
  weighted = eu_sal_weighted,
  not_aggregated = trait_eu_sal,
  original = ecor_Q_cp
)

# normalize again (aggregation yields to non normalized traits)
agg_data <- lapply(agg_data, function(y) normalize_by_rowSum_df(y))

# save
saveRDS(object = agg_data, 
        file = file.path("./Data/Re-analysis_cache/agg_data.rds"))

# test how similar aggregated columns are
test <- rbindlist(agg_data[1:6], use.names = TRUE,
                  idcol = "approach")
res <- list()
for(cols in names(test)[-1]) {
  res[[cols]] <-
    test[, {
      x = get(cols)
      test[, .(cor(x, get(cols))), by = approach]
    },
    by = approach]
}

# most correlation values seem very high
lapply(res, function(y) y[V1 != 1,
                          mean(V1, na.rm = TRUE),
                          by = "approach"])

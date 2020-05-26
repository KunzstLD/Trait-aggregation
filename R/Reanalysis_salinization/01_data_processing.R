#____________________________________________________________________________________________
#### Re-analyzing Szöcs et al. 2014 - data processing ####
#____________________________________________________________________________________________

#### Load and prepare data from Edi ####
ecor_data <- load_data(path = "./Data/Edi_salinity_study/cache", pattern = "*.rds")

# multiple assignment
c(ecor_L, ecor_Q, ecor_R, ordos) %<-% ecor_data[c("ecor_L.rds", "ecor_Q.rds", "ecor_R.rds", "ordos.rds")]

# rm "sp." and "spp." from ecor data
rownames(ecor_Q) <- sub(" sp\\.| spp\\.", "", rownames(ecor_Q))
names(ecor_L) <- sub(" sp\\.| spp\\.", "", names(ecor_L))

# load trait lookup from Edi
trait_edi_lookup <- fread(file.path("./Data/Edi_salinity_study/data/trait_lookup.csv"), sep = ";")
trait_edi_lookup[, ID_trait := paste0("X", ID_trait)]

# extend lookup table with harmonized traits
trait_ext_lookup <- rbind(trait_edi_lookup, data.table(
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
tachet_edi <- fread(file.path("./Data/Edi_salinity_study/data/tachet.csv"), sep = ";")

# create lookup vector
# names are the keys
# values are the vector entries
# lookup_vec <- trait_edi_lookup$ID_name
# names(lookup_vec) <- trait_edi_lookup$ID_trait
# colnames(ecor_Q) <- unname(lookup_vec[colnames(ecor_Q)])

# ____________________________________________________________________________________________
#### Preprocess european trait data ####
trait_dat <- load_data(path = "./Data/", pattern = "harmonized.*\\.rds")

# European trait data are already normalized
trait_eu <- trait_dat[["Trait_EU_pp_harmonized.rds"]] 

# select traits:
cols <- "feed|locom|resp|volt|ovip|size|order|family|genus|species"
trait_eu_subset <- trait_eu[, .SD, .SDcols = names(trait_eu) %like% cols]

# add taxa column for merge
trait_eu_subset[, taxa := coalesce(species, genus, family, order)]

# change col order
setcolorder(
  x = trait_eu_subset,
  neworder = c(
    "order",
    "family",
    "genus",
    "species",
    "taxa",
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

# ____________________________________________________________________________________________
#### Preparation for re-analysis ####

# TODO: Check why these taxa on genus level do not end up in final list!
# "Serratella" is Ephemerella (rename to old name back)
trait_eu_subset[is.na(species) & grepl("Ephemerella", genus), `:=`(genus = "Serratella",
                                                                   taxa = "Serratella")]

# Chaetopterygini is Stenophylacini -> rename back
trait_eu_subset[genus %in% "Stenophylacini", `:=`(genus = "Chaetopterygini",
                                                  taxa = "Chaetopterygini")]

# ORTHOCLADIINAE/DIAMESINAE is also PRODIAMESINAE
trait_eu_subset[genus %in% "Orthocladiinae", `:=`(genus = "Prodiamesinae",
                                                  taxa = "Prodiamesinae")]

# "Naididae" is Tubificidae (old name, rename back)
trait_eu_subset[family %in% "Naididae", family := "Tubificidae"]

# Pediciinae -> taxonomical data are on subfamily lvl
# while trait data are on species lvl and one entry also on sub-family
# (Pediciini) -> assigned to Pedcciniae
trait_eu_subset[genus %in% "Pediciini", `:=`(genus = "Pediciinae",
                                             taxa = "Pediciinae")]

# taxa that are on family-lvl in ecor_L:
# (and genus or species-level in tachet/freshwaterecol)
# need to be aggregated via median (likewise in Szöcs et al. 2014)
trait_cols <- grep("order|family|genus|species|taxa", 
                   names(trait_eu_subset),
                   value = TRUE,
                   invert = TRUE)
agg_traits <- trait_eu_subset[family %in% c(
  "Ceratopogonidae",
  "Empididae",
  "Lepidostomatidae",
  "Limoniidae",
  "Pediciinae",
  "Perlodidae",
  "Prodiamesinae",
  "Psychomyiidae",
  "Spongillidae",
  "Chironomidae",
  "Tubificidae",
  "Limnephilidae",
  "Coenagrionidae"
), lapply(.SD, median, na.rm = TRUE), .SDcols = trait_cols, by = "family"] %>% 
  normalize_by_rowSum(x = ., 
                      non_trait_cols = c("order",
                                         "family",
                                         "genus",
                                         "species",
                                         "taxa"))
setnames(agg_traits,
         old = "family",
         new = "taxa")

# aggregate all Oligochaeta taxa -> are actually (sub)class
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

# subset trait_eu_subset to taxa in ecor_Q
trait_eu_sal <- trait_eu_subset[taxa %in% rownames(ecor_Q),]
trait_eu_sal <- rbind(trait_eu_sal, trait_eu_oligo, agg_traits, 
                      fill = TRUE)

# few NA values in traits: set to 0 (according to Edi's Paper, missing information
# for traits was set to 0)
trait_cols <-
  grep(
    "species|genus|family|order|taxa|.*class",
    names(trait_eu_sal),
    value = TRUE,
    invert = TRUE
  )
for(j in trait_cols) {
  data.table::set(trait_eu_sal, which(is.na(trait_eu_sal[[j]])), j, 0)
}

# add additional traits from ecor_Q:
# rm those traits that have been harmonized
rm_col <-
  trait_edi_lookup[ID_trait_name %like% "(?i)size|locom|feed|resp|repr|number.*cycles", ID_trait]
ecor_Q <- ecor_Q[, !names(ecor_Q) %in% rm_col]

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

# check if all taxa are covered
# rownames(ecor_Q)[!rownames(ecor_Q) %in% trait_eu_sal$taxa]

# create dataset with taxonomical information
taxa <- trait_eu_sal[, .(taxa, sub_class, order, family, genus, species)]

# rm taxonomical information, create df and assign rownames 
trait_eu_sal[, c("sub_class", "order", "family", "genus", "species") := NULL]
setDF(trait_eu_sal)
rownames(trait_eu_sal) <- trait_eu_sal$taxa
trait_eu_sal$taxa <- NULL

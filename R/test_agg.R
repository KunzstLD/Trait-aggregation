# Differences between old and new aggregated data files ----

agg_new <- readRDS(file = "./Cache/Re-analysis_cache/agg_data.rds")
agg_old <- readRDS(file = "./Cache/Re-analysis_cache/agg_data_old.rds")

# Big differences seem to arise for 
# - Oligochaeta 
# - Agraylea
# - Hydroptila
# - Ithytrichia
# -> why?
diff_dat <- agg_new$weighted - agg_old$weighted
diff_dat$taxon <- rownames(diff_dat)
setDT(diff_dat)

melt(diff_dat, id.vars = "taxon") %>%
  .[value >= 0.5| value <= -0.5, ]

agg_old$weighted[rownames(agg_old$weighted) == "Oligochaeta", ]
agg_new$weighted[rownames(agg_new$weighted) == "Oligochaeta", ]


# Differences in old and new trait data ----
# New version uses first fwe and than tachet,
# + problem with sum(c()) solved 
trait_dat <-
  load_data(path = "./Data/", pattern = "harmonized.*\\.rds")

# European trait data are already normalized
trait_eu <- trait_dat[["Trait_freshecol_2020_pp_harmonized.rds"]]
trait_eu_old <- trait_dat[["Trait_freshecol_2020_pp_harmonized_old.rds"]]

trait_datasets <- list("new" = trait_eu,
                       "old" = trait_eu_old)
output <- list()
for(nam in names(trait_datasets)) {
  
  dat <- trait_datasets[[nam]]
  # Select traits
  # taxon_cp includes taxa names from the raw data
  cols <-
    "feed|locom|resp|volt|ovip|size|order|family|genus|species|taxon_cp"
  trait_eu_subset <- dat[, .SD, .SDcols = names(trait_eu) %like% cols]
  
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
  
  # clean genus and family column from sp. (only for merging later!)
  trait_eu_subset[is.na(species), genus := sub("(?i) Gen\\. sp\\.| sp\\.", "", genus)]
  trait_eu_subset[is.na(species) &
                    is.na(genus), family := sub("(?i) Gen\\. sp\\.| sp\\.", "", family)]
  
  #____________________________________________________________________________________________
  # Preparation for re-analysis ----
  # few taxa have to be re-named or re-assigned
  #____________________________________________________________________________________________
  
  # Chaetopterygini to genus column
  trait_eu_subset[taxon_cp %like% "Chaetopterygini", genus := "Chaetopterygini"]
  
  # ORTHOCLADIINAE/DIAMESINAE is also PRODIAMESINAE
  trait_eu_subset[taxon_cp %like% "Orthocladiinae", genus := "Prodiamesinae"]
  
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
  trait_cols <- grep(
    "order|family|genus|species|tax.*",
    names(trait_eu_subset),
    value = TRUE,
    invert = TRUE
  )
  
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
  ), lapply(.SD, median, na.rm = TRUE),
  .SDcols = trait_cols,
  by = "family"] %>%
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
  
  output[[nam]] <- trait_eu_subset 
}

# Here, the difference between the falsely (old) and newly harmonised trait dataset
# becomes apparent 
# -> in "old" sometimes 0 has been inserted instead of NA (sum(c()) = 0 in R)
# Therefore, e.g. all locom traits are assigned 0 for Oligochaeta.
# Instead, new, locom_burrow == 1, the others == 0
output$old[sub_class == "Oligochaeta", lapply(.SD, median, na.rm = TRUE),
           .SDcols = trait_cols,
           by = "sub_class"] %>%
  normalize_by_rowSum(x = .,
                      non_trait_cols = "sub_class") %>% print()
output$new[sub_class == "Oligochaeta", lapply(.SD, median, na.rm = TRUE),
           .SDcols = trait_cols,
           by = "sub_class"]


#####
trait_eu_old[species == "Cernosvitoviella atrata", ]
trait_eu[species == "Cernosvitoviella atrata", ]
trait_eu_old[genus %like% "Agraylea", ]
trait_eu[genus %like% "Agraylea", ]
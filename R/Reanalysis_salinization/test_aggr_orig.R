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


# create copy
ecor_Q_new <- ecor_Q

# lookup
trait_edi_lookup[ID_trait_name %like% "(?i)size|locom|feed|resp|repr|number.*cycles",
                 full_name := paste0(ID_name, "_", ID_trait_name)] 

gettraitnames <- trait_edi_lookup$full_name
names(gettraitnames) <- trait_edi_lookup$ID_trait
  
# target
col_names <- names(ecor_Q_new)

# do the lookup
vec <- gettraitnames[col_names]

for(i in seq_along(vec)) {
  if (is.na(vec[[i]])) {
    vec[[i]] <- names(vec[i])
  }
}
vec <- unname(vec)

# assign
names(ecor_Q_new) <- vec

#### harmonize ####
ecor_Q_new$taxa <- rownames(ecor_Q_new)
setDT(ecor_Q_new)

# - voltinism:
setnames(ecor_Q_new, 
         c("< 1_Potential number of cycles per year", 
           "1_Potential number of cycles per year", 
           "> 1_Potential number of cycles per year"),
         c("volt_semi",
           "volt_uni",
           "volt_bi_multi"))
         
# - feeding mode:
setnames(ecor_Q_new, 
         c("shredder_Feeding habits", 
           "deposit feeder_Feeding habits",
           "parasite_Feeding habits",
           "scraper_Feeding habits"),
         c("feed_shredder", 
           "feed_gatherer",
           "feed_parasite",
           "feed_herbivore"))

# few piercers are actually herbivore piercers!
ecor_Q_new[taxa %in% c("Hydroptila", "Agraylea"), `:=`(feed_herbivore = `piercer_Feeding habits`,
                                                       `piercer_Feeding habits` = 0)]

# filterer
ecor_Q_new[, feed_filter := apply(.SD, 1, max) ,
           .SDcols = c("filter-feeder_Feeding habits",
                       "absorber_Feeding habits")]

# predator
ecor_Q_new[, feed_predator := apply(.SD, 1, max), 
           .SDcols = c("predator_Feeding habits",
                       "piercer_Feeding habits")]

# rm
ecor_Q_new[, c("filter-feeder_Feeding habits",
               "absorber_Feeding habits",
               "predator_Feeding habits",
               "piercer_Feeding habits") := NULL]

# - locomotion: 
setnames(ecor_Q_new,
         "crawler_Locomotion and substrate relation",
         "locom_crawl")

# swimmer
ecor_Q_new[, locom_swim := apply(.SD, 1, max), 
           .SDcols = c("surface swimmer_Locomotion and substrate relation",
                       "full water swimmer_Locomotion and substrate relation")]

# burrower 
ecor_Q_new[, locom_burrow := apply(.SD, 1, max),
           .SDcols = c(
             "burrower_Locomotion and substrate relation",
             "interstitial_Locomotion and substrate relation"
           )]

# sessil
ecor_Q_new[, locom_sessil := apply(.SD, 1, max),
           .SDcols = c(
             "temporarily attached_Locomotion and substrate relation",
             "permanently attached_Locomotion and substrate relation"
           )]

# rm
ecor_Q_new[, c(
  "surface swimmer_Locomotion and substrate relation",
  "full water swimmer_Locomotion and substrate relation",
  "burrower_Locomotion and substrate relation",
  "interstitial_Locomotion and substrate relation",
  "temporarily attached_Locomotion and substrate relation",
  "permanently attached_Locomotion and substrate relation",
  "flier_Locomotion and substrate relation"
) := NULL]

# - respiration: 
setnames(ecor_Q_new, 
         c("gill_Respiration",
           "tegument_Respiration"), 
         c("resp_gil",
           "resp_teg"))

# spiracle/plastron
ecor_Q_new[, resp_pls_spi := apply(.SD, 1, max),
           .SDcols = c(
             "plastron_Respiration",                    
             "spiracle_Respiration"
           )]

# rm
ecor_Q_new[, c("hydrostatic vesicle_Respiration",
               "plastron_Respiration",                    
               "spiracle_Respiration") := NULL]

# - reproduction/oviposition: 
setnames(
  ecor_Q_new,
  c(
    "ovoviviparity_Reproduction",
    "clutches, terrestrial_Reproduction"
  ),
  c("ovip_ovo",
    "ovip_ter")
)

# aquatic eggs
ecor_Q_new[, ovip_aqu := apply(.SD, 1, max),
           .SDcols = c(
             "isolated eggs, free_Reproduction",
             "isolated eggs, cemented_Reproduction",
             "clutches, cemented or fixed_Reproduction",
             "clutches, free_Reproduction",
             "clutches, in vegetation_Reproduction"
           )]

# rm
ecor_Q_new[, c("isolated eggs, free_Reproduction",
               "isolated eggs, cemented_Reproduction",
               "clutches, cemented or fixed_Reproduction",
               "clutches, free_Reproduction",
               "clutches, in vegetation_Reproduction",
               "asexual reproduction_Reproduction") := NULL]

# - size: 
setnames(ecor_Q_new, 
         "> 1-2 cm_Maximal potential size",
         "size_medium")

# small
ecor_Q_new[, size_small := apply(.SD, 1, max),
         .SDcols = c(
           "≤ .25 cm_Maximal potential size",
           "> .25-.5 cm_Maximal potential size",
           "> .5-1 cm_Maximal potential size"
         )
]

# large
ecor_Q_new[, size_large := apply(.SD, 1, max),
         .SDcols = c(
           "> 2-4 cm_Maximal potential size",
           "> 4-8 cm_Maximal potential size",
           "> 8 cm_Maximal potential size"
         )
]

# rm
ecor_Q_new[, c(
  "≤ .25 cm_Maximal potential size",
  "> .25-.5 cm_Maximal potential size",
  "> .5-1 cm_Maximal potential size",
  "> 2-4 cm_Maximal potential size",
  "> 4-8 cm_Maximal potential size",
  "> 8 cm_Maximal potential size") := NULL]

# order columns
setcolorder(
  ecor_Q_new,
  c(
    "size_medium",
    "size_small",
    "size_large",
    "X2.1",
    "X2.2",
    "volt_semi",
    "volt_uni",
    "volt_bi_multi",
    "X4.1",
    "X4.2",
    "X4.3",
    "X4.4",
    "ovip_ovo",
    "ovip_ter",
    "ovip_aqu",
    "X6.1",
    "X6.2",
    "X6.3",
    "X6.4",
    "X7.1",
    "X7.2",
    "X7.3",
    "X7.4",
    "X7.5",
    "resp_teg",
    "resp_gil",
    "resp_pls_spi",
    "locom_crawl",
    "locom_swim",
    "locom_burrow",
    "locom_sessil",
    "X10.1",
    "X10.2",
    "X10.3",
    "X10.4",
    "X10.5",
    "X10.6",
    "X10.7",
    "X10.8",
    "X10.9",
    "feed_gatherer",
    "feed_shredder",
    "feed_herbivore",
    "feed_parasite",
    "feed_filter",
    "feed_predator",
    "X12.1",
    "X12.2",
    "X12.3",
    "X12.4",
    "X12.5",
    "X12.6",
    "X12.7",
    "X13.1",
    "X13.2",
    "X13.3",
    "X13.4",
    "X13.5",
    "X13.6",
    "X13.7",
    "X13.8",
    "X14.1",
    "X14.2",
    "X14.3",
    "X15.1",
    "X15.2",
    "X15.3",
    "X15.4",
    "X15.5",
    "X15.6",
    "X15.7",
    "X15.8",
    "X15.9",
    "X16.1",
    "X16.2",
    "X16.3",
    "X16.4",
    "X17.1",
    "X17.2",
    "X17.3",
    "X18.1",
    "X18.2",
    "X19.1",
    "X19.2",
    "X19.3",
    "X20.1",
    "X20.2",
    "X20.3",
    "X20.4",
    "X20.5",
    "X21.1",
    "X21.2",
    "X21.3",
    "X21.4",
    "X21.5",
    "X21.6",
    "taxa"
  )
)

# ___________________________________________________________________
#### aggregation ####
# ___________________________________________________________________

# add taxonomic information from tachet edi
tachet_edi[grepl(".*sp\\.|spp\\.", Species), Species := NA]
tachet_edi[!is.na(Species), Species := paste(Genus, Species)]
tachet_edi[, Family := simple_cap(Family)]
tachet_edi[, taxa := coalesce(Species, Genus, Family, Taxa.Group)] 
ecor_Q_new[tachet_edi, `:=`(species = i.Species, 
                         genus = i.Genus, 
                         family = i.Family,
                         order = Taxa.Group), 
           on = "taxa"]


# normalize prior aggregation
normalize_by_rowSum(x = ecor_Q_new, 
                    non_trait_cols = c("species",
                                       "genus",
                                       "family",
                                       "order",
                                       "taxa"))

#### stepwise agg median ####
stepwise_median <- spec_genus_agg_alt(
  trait_data = ecor_Q_new,
  non_trait_cols = c("taxa",
                     "order",
                     "family",
                     "genus",
                     "species"),
  method = median
) %>%
  merge(x = .,
        y = ecor_Q_new[taxa != "Oligochaeta", .(family, taxa)],
        by = "family",
        all = TRUE) %>%
  rbind(., ecor_Q_new[taxa == "Oligochaeta",],
        fill = TRUE) %>% 
  .[order(taxa), ]

stepwise_median[, c("order",
                    "family",
                    "genus",
                    "species") := NULL]
setDF(stepwise_median)
rownames(stepwise_median) <- stepwise_median$taxa
stepwise_median$taxa <- NULL


#### stepwise agg mean ####
stepwise_mean <- spec_genus_agg_alt(
  trait_data = ecor_Q_new,
  non_trait_cols = c("taxa",
                     "order",
                     "family",
                     "genus",
                     "species"),
  method = mean
) %>%
  merge(x = .,
        y = ecor_Q_new[taxa != "Oligochaeta", .(family, taxa)],
        by = "family",
        all = TRUE) %>%
  rbind(., ecor_Q_new[taxa == "Oligochaeta", ],
        fill = TRUE) %>%
  .[order(taxa),]

stepwise_mean[, c("order",
                  "family",
                  "genus",
                  "species") := NULL]
setDF(stepwise_mean)
rownames(stepwise_mean) <- stepwise_mean$taxa
stepwise_mean$taxa <- NULL


#### direct agg median ####
direct_median <- direct_agg(
  trait_data = ecor_Q_new,
  non_trait_cols = c("taxa",
                     "order",
                     "family",
                     "genus",
                     "species"),
  method = median
) %>%
  merge(x = .,
        y = ecor_Q_new[taxa != "Oligochaeta", .(family, taxa)],
        by = "family",
        all = TRUE) %>%
  rbind(., ecor_Q_new[taxa == "Oligochaeta",],
        fill = TRUE) %>% 
  .[order(taxa), ]

# rm taxonomical information, create df and assign rownames 
direct_median[, c("order",
                  "family",
                  "genus",
                  "species") := NULL]
setDF(direct_median)
rownames(direct_median) <- direct_median$taxa
direct_median$taxa <- NULL


#### direct agg mean ####
direct_mean <- direct_agg(
  trait_data = ecor_Q_new,
  non_trait_cols = c("taxa",
                     "order",
                     "family",
                     "genus",
                     "species"),
  method = mean
) %>%
  merge(x = .,
        y = ecor_Q_new[taxa != "Oligochaeta", .(family, taxa)],
        by = "family",
        all = TRUE) %>%
  rbind(., ecor_Q_new[taxa == "Oligochaeta",],
        fill = TRUE) %>% 
  .[order(taxa), ]

# rm taxonomical information, create df and assign rownames 
direct_mean[, c("order",
                "family",
                "genus",
                "species") := NULL]
setDF(direct_mean)
rownames(direct_mean) <- direct_mean$taxa
direct_mean$taxa <- NULL

#### weighted agg ####
weighted <- weighted_agg(
  trait_data = ecor_Q_new,
  non_trait_cols = c("taxa",
                     "order",
                     "family",
                     "genus",
                     "species")
) %>%
  merge(x = .,
        y = ecor_Q_new[taxa != "Oligochaeta", .(family, taxa)],
        by = "family",
        all = TRUE) %>%
  rbind(., ecor_Q_new[taxa == "Oligochaeta", ],
        fill = TRUE) %>% 
  .[order(taxa), ]

# rm taxonomical information, create df and assign rownames 
weighted[, c("order",
             "family",
             "genus",
             "species") := NULL]
setDF(weighted)
rownames(weighted) <- weighted$taxa
weighted$taxa <- NULL

#### harmonized but not aggregated ####
ecor_Q_new[, c("species",
               "genus",
               "family",
               "order") := NULL]

setDF(ecor_Q_new)
rownames(ecor_Q_new) <- ecor_Q_new$taxa
ecor_Q_new$taxa <- NULL

#### Original ####

# order alphabetically
ecor_Q <- ecor_Q[order(rownames(ecor_Q)),]

vec <- sub("\\..*", "\\1", names(ecor_Q))
blocks <- rle(vec)$lengths

# convert to %-traits
ecor_Q <- prep.fuzzy.var(ecor_Q, blocks)


#### Final data processing ####

# Abundance data and ecor_Q_cp need to be ordered
# in the same way as the other datasets (otherwise weighted
# trait calculation gets wrong)
ecor_L <- ecor_L[, rownames(ecor_Q_new)] 

# finally combine all datasets 
agg_data <- list(
  stepw_median = stepwise_median,
  stepw_mean = stepwise_mean,
  direct_median = direct_median,
  direct_mean = direct_mean,
  weighted = weighted,
  not_aggregated = ecor_Q_new,
  original = ecor_Q
)

# normalize again (aggregation yields to non normalized traits)
agg_data <- lapply(agg_data, function(y) normalize_by_rowSum_df(y))


# ___________________________________________________________________
#### Analysis ####
# results not different from fwe 2018 Analysis
# but slightly different to fwe 2020, i.e. resp_gil is missing 
# infwe 2020 example
# Q: Do we actually need NZ and EU harmonization and so on?

# ___________________________________________________________________

# rda
trait_dat <- lapply(agg_data, function(y) weighted.rda(trait.data = y))

# explained variance
lapply(trait_dat, function(y) explained.var(y[[1]])) %>%
  rbindlist(., idcol = "id")

# site and species scores (only RDA axis)
trait_dat_species_scores <- lapply(trait_dat, function(y) extract.species.scores(y[[1]])) %>%
  rbindlist(., idcol = "id")


# merge traits
trait_dat_species_scores <- merge(trait_dat_species_scores,
                                  trait_ext_lookup,
                                  by.x = "traits",
                                  by.y = "ID_trait"
)

trait_dat_site_scores <- lapply(trait_dat, function(y) {
  extract.site.scores(y[[1]])
}) %>%
  rbindlist(., idcol = "id")

# species scores to indicate which traits shape
# different parts of the stream
trait_dat_species_scores[, id := factor(
  id,
  levels = c(
    "weighted",
    "direct_mean",
    "direct_median",
    "stepw_mean",
    "stepw_median",
    "not_aggregated",
    "original"
  )
)]

# Results suggest that harmonization changed slightly the result
# aggregation methdos seem not to change anything from the harmonized
# version
trait_dat_species_scores[, maha_dist := dist_to_axis(x = RDA_species_scores), by = "id"]

# traits responding to high salinity
trait_high_sal <- trait_dat_species_scores[maha_dist > qchisq(0.975, df = 1), ] %>%
  .[order(id), ]
trait_dat_species_scores[, median(RDA_species_scores),
                         by = "id"
]


#### Plot species scores ####
ggplot(
  trait_dat_species_scores,
  aes(x = as.factor(id), y = RDA_species_scores)
) +
  geom_boxplot() +
  # geom_jitter(size = 1.5,
  #             width = 0.05,
  #             alpha = 0.6,
  #             col = "darkgray")+
  coord_flip() +
  labs(x = NULL, y = "RDA species scores") +
  scale_x_discrete(
    labels = c(
      "Weighted_agg",
      "Direct_agg \n (mean)",
      "Direct_agg \n (median)",
      "Stepwise_agg \n (mean)",
      "Stepwise_agg \n (median)",
      "Harmonized; \n not aggregated",
      "Original"
    )
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 11),
    axis.text.y = element_text(family = "Roboto Mono", size = 11),
    panel.grid = element_blank()
  )















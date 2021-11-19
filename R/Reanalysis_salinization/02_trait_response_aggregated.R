# _________________________________________________________________________________________________
# aggregate everything on family level using different aggregation methods
# combine with ecor_L:
# - Maybe the best way would be to leave the taxa names as they are
# - Just assign aggregated information
# - Then calculate RDA scores (site and species)
# - Find a good way to represent this graphically
# _________________________________________________________________________________________________

# _________________________________________________________________________________________________
#### load data ####
# _________________________________________________________________________________________________

# aggregated trait datasets
agg_data <- readRDS(file = "./Cache/Re-analysis_cache/agg_data.rds")

# original matrices (species, CWM traits, site and env. variables, taxonomy)
# ecor_data <- load_data(path = "./Data/Edi_salinity_study/cache",
#                       pattern = "*.rds")
ecor_data <-
  load_data(path = "./Cache/Re-analysis_cache/",
            pattern = "corr*.rds")

# multiple assignment
c(ecor_L, ecor_Q, ecor_R, ordos) %<-% ecor_data[c("ecor_L_corr.rds",
                                                  "ecor_Q_corr.rds",
                                                  "ecor_R_corr.rds",
                                                  "ordos_corr.rds")]

# trait table 
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
                                "Feeding Mode",
                                "Locomotion",
                                "Respiration",
                                "Voltinism",
                                "Reproduction",
                                "Body Size"
                              ),
                              c(6, 4, 3, 3, 3, 3)
                            )
                          ))

# _________________________________________________________________________________________________
#### calculate RDA ####
# _________________________________________________________________________________________________
trait_dat <- lapply(agg_data, function(y) weighted.rda(trait.data = y))

# explained variance
lapply(trait_dat, function(y) explained.var(y[[1]])) %>%
  rbindlist(., idcol = "id")

# summary
lapply(trait_dat, function(y) summary(y$cwmRDA))[[2]]

# site and species scores (only RDA axis)
species_scores <- lapply(trait_dat, function(y) extract.species.scores(y[[1]])) %>%
  rbindlist(., idcol = "id")

# merge traits
species_scores <- merge(species_scores,
  trait_ext_lookup,
  by.x = "traits",
  by.y = "ID_trait"
)

site_scores <- lapply(trait_dat, function(y) {
  extract.site.scores(y[[1]])
}) %>%
  rbindlist(., idcol = "id")

# species scores to indicate which traits shape
# different parts of the stream
species_scores[, id := factor(
  id,
  levels = c(
    "weighted",
    "stepw_mean",
    "stepw_median",
    "direct_mean",
    "direct_median",
    "not_aggregated",
    "original"
  )
)]

# Multiply scores from original with -1 for comparability reasons
species_scores[id == "original", RDA_species_scores := RDA_species_scores*(-1)]
site_scores[id == "original", RDA_site_scores := RDA_site_scores*(-1)]

# Results suggest that harmonization changed slightly the result
# aggregation methods seem not to change anything from the harmonized version

# Mahalanobis distance 
species_scores[, maha_dist := dist_to_axis(x = RDA_species_scores), by = "id"]

# traits responding to either extreme high or low salinity
results_high_low_sal <- species_scores[maha_dist > qchisq(0.975, df = 1), ] %>%
  .[order(id), ]
species_scores[, median(RDA_species_scores),
                         by = "id"
]

# _________________________________________________________________________________________________
#### Plot species scores ####
# _________________________________________________________________________________________________
ggplot(species_scores,
       aes(x = id, y = RDA_species_scores*(-1))) +
  geom_violin(color = "gray45", 
              draw_quantiles = 0.5,
              linetype = "solid",
              size = 0.8) +
  geom_jitter(
    size = 2,
    width = 0.05,
    alpha = 0.3,
    col = "black"
  ) +
  ylim(c(-0.6, 0.55)) +
  coord_flip() +
  labs(y = "RDA species scores", x = "") +
  scale_x_discrete(
    labels = c(
      "Weighted_agg",
      "Stepwise_agg \n (mean)",
      "Stepwise_agg \n (median)",
      "Direct_agg \n (mean)",
      "Direct_agg \n (median)",
      "Harmonised; \n not aggregated",
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
for (path in c(data_out, data_paper)) {
  ggplot2::ggsave(
    filename = file.path(path, "Species_scores_rda.pdf"),
    width = 22,
    height = 12,
    units = "cm",
    device = cairo_pdf,
    dpi = 400
  )
}
# For graphical display of species and site scores
# use script: 03_plots_species_scores_comp.R

# _________________________________________________________________________________________________
#### Post-analysis aggregated traits and species scores ####
# _________________________________________________________________________________________________
agg_data_post <- rbindlist(agg_data, idcol = "dataset", fill = TRUE)
 
## feeding mode shredder/X11.3
ggplot(agg_data_post)+
  geom_violin(data = ~ .x[dataset != "original", ],
               mapping = aes(x = as.factor(dataset), y = feed_shredder))+
  geom_violin(data = ~ .x[dataset == "original", ],
              mapping = aes(x = as.factor(dataset), y = X11.3))+
  geom_jitter(data = ~ .x[dataset != "original", ],
              mapping = aes(x = as.factor(dataset), y = feed_shredder),
              width = 0.15)+
  geom_jitter(data = ~ .x[dataset == "original", ],
              mapping = aes(x = as.factor(dataset), y = X11.3),
              width = 0.15)
agg_data_post[, .(mean_shredder = mean(X11.3),
                  median_shredder = median(X11.3),
                  sd_shredder = sd(X11.3)),
              by = "dataset"]
agg_data_post[, .(mean(feed_shredder),
                  median(feed_shredder),
                  sd(feed_shredder)),
              by = "dataset"]

species_scores[traits == "feed_shredder" | traits == "X11.3", ]

## Respiration gills
ggplot(agg_data_post)+
  geom_violin(data = ~ .x[dataset != "original", ],
               mapping = aes(x = as.factor(dataset), y = resp_gil))+
  geom_violin(data = ~ .x[dataset == "original", ],
               mapping = aes(x = as.factor(dataset), y = X8.2))+
  geom_jitter(data = ~ .x[dataset != "original", ],
              mapping = aes(x = as.factor(dataset), y = resp_gil),
              width = 0.15)+
  geom_jitter(data = ~ .x[dataset == "original", ],
              mapping = aes(x = as.factor(dataset), y = X8.2),
              width = 0.15)

agg_data_post[, .(mean(X8.2),
                  median(X8.2),
                  sd(X8.2)),
              by = "dataset"]

agg_data_post[, .(mean(resp_gil),
                  median(resp_gil),
                  sd(resp_gil)),
              by = "dataset"]
agg_data_post[X8.2 == 0, .N]

species_scores[traits == "resp_gil" | traits == "X8.2", ]


## X2.1 - short life cycle duration
ggplot(agg_data_post, aes(x = as.factor(dataset), y = X2.1)) +
  geom_violin()+
  geom_jitter()

agg_data_post[, .(mean(X2.1),
                  median(X2.1),
                  sd(X2.1)), by = "dataset"]

species_scores[traits == "X2.1", ]

## X2.2 - long life cycle duration
ggplot(agg_data_post, aes(x = as.factor(dataset), y = X2.2)) +
  geom_violin()+
  geom_jitter()

agg_data_post[, .(mean(X2.2),
                  median(X2.2),
                  sd(X2.2)), by = "dataset"]


## final table for SI
SI_responsetraits <- melt(agg_data_post,
     measure.vars = c("feed_shredder",
                      "X11.3",
                      "resp_gil",
                      "X8.2",
                      "X2.1",
                      "X2.2")) %>%
  .[, .(
    mean_trait_value = mean(value, na.rm = TRUE),
    median_trait_value = median(value, na.rm = TRUE),
    sd_trait_value = sd(value, na.rm = TRUE)
  ),
  by = .(dataset, variable)] %>%
  .[!is.na(mean_trait_value) &
      !is.na(median_trait_value) & !is.na(sd_trait_value),]

SI_responsetraits[, variable := case_when(
  variable %in% c("feed_shredder", "X11.3") ~ "Shredder",
  variable %in% c("resp_gil", "X8.2") ~ "Gills",
  variable == "X2.1" ~ "Short life cycle",
  variable == "X2.2" ~ "Long life cylce"
)]
SI_responsetraits[,
                  dataset := case_when(
                    dataset == "not_aggregated" ~ "Harmonized; not_aggregated",
                    TRUE ~ as.character(dataset)
                  )]
SI_responsetraits[, dataset := simple_cap(dataset)]

cols <- c("mean_trait_value", "median_trait_value", "sd_trait_value")
SI_responsetraits[, (cols) := lapply(.SD, function(y) round(y, digits = 2)),
                  .SDcols = cols]

setnames(SI_responsetraits,
         c("dataset",
           "variable",
           cols),
         c("Dataset",
           "Trait",
           "Mean",
           "Median",
           "SD"))

# latex output
xtable_wo_rownames(
  SI_responsetraits,
  caption = "",
  label = "tab:SI_resp_traits_summary_stats"
)


# # _________________________________________________________________________________________________
# #### Biplot ####
# # _________________________________________________________________________________________________
# plot(trait_dat$not_aggregated$cwmRDA, display = "sites", type = "none")
# points(
#   trait_dat$not_aggregated$cwmRDA,
#   display = "sites",
#   col = colvec[class],
#   pch = 25,
#   cex = 0.8
# )
# text(trait_dat$stepw_median$cwmRDA,
#      display = "bp",
#      col = "blue",
#      arrow.mul = 0.7)
# legend(
#   "topright",
#   legend = levels(class),
#   bty = "n",
#   col = colvec,
#   pch = 16
# )
# ordisurf(trait_dat$stepw_median$cwmRDA ~ cond, data = ecor_R, add = TRUE)



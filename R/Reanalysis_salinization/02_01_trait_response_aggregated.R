# ________________________________________________________________________
# aggregate everything on family level using different aggregation methods
# combine with ecor_L:
# - Maybe the best way would be to leave the taxa names as they are
# - Just assign aggregated information
# - Then calculate RDA scores (site and species)
# - Find a good way to represent this graphically
# ________________________________________________________________________

# load data
agg_data <- readRDS(file = "./Data/Re-analysis_cache/agg_data.rds")

# calculate rda ----
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
for (path in c(data_out, data_paper)) {
  ggplot2::ggsave(
    filename = file.path(path, "Species_scores_rda.png"),
    width = 22,
    height = 12,
    units = "cm"
  )
}


#### boxplot comparison ####
# TODO: Needs improvement, especially label size
# png(filename = file.path(data_out, "RDA_traits.png"),
#     width = 1600,
#     height = 866)
par(mar = c(5, 4, 4, 15))
par(cex.lab=1.1) # is for y-axis
par(cex.axis=1.1) # is for x-axis
boxplot(
  scores(trait_dat$not_aggregated$cwmRDA)$sites[, 1] ~ class,
  ylab = "Scores on constrained axis",
  xlab = "Position",
  col = colors,
  main = "RDA of traits constrained by electric conductivity using harmonized traits"
)
abline(h = 0, lty = "dotted")
rug(trait_dat_species_scores$RDA_species_scores, side = 4)
linestack(
  trait_high_sal[id == "not_aggregated", RDA_species_scores],
  labels = paste(trait_high_sal[id == "not_aggregated", ID_trait_name],
                 trait_high_sal[id == "not_aggregated", ID_name],
                 sep = ":"),
  at = par("usr")[2],
  add = TRUE,
  hoff = 1,
  cex = 0.9
)


#### biplot ####
class <- factor(ecor_R$salinisati)
scl = 3
colvec <- c("red", "orange", "green")

plot(trait_dat$not_aggregated$cwmRDA, display = "sites", type = "none")
points(
  trait_dat$not_aggregated$cwmRDA,
  display = "sites",
  col = colvec[class],
  pch = 25,
  cex = 0.8
)
text(trait_dat$stepw_median$cwmRDA,
     display = "bp",
     col = "blue",
     arrow.mul = 0.7)
legend(
  "topright",
  legend = levels(class),
  bty = "n",
  col = colvec,
  pch = 16
)
ordisurf(trait_dat$stepw_median$cwmRDA ~ cond, data = ecor_R, add = TRUE)


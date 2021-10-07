# ____________________________________________________________________________________________
#### Trait-responses ####
# Most code taken from Eduard Szöcs and adapted:
# script "do_ecor.R"
# calculation uses harmonized trait data (were available) 
# and the original (not aggregated) trait information
# ____________________________________________________________________________________________

# ____________________________________________________________________________________________
#### cwmRDA ####
# ____________________________________________________________________________________________

# Calculate weighted trait-abundances:
# subset taxon pool in ecor_L to taxa in trait_eu_sal
# ecor_L <-
#   ecor_L[, names(ecor_L)[names(ecor_L) %in% rownames(trait_eu_sal)]]

original <- agg_data$original

# weight by square-root transformed abundance
trans <- 0.5
ecor_cwm_new <-
  prop.table(as.matrix(ecor_L^ trans), 1) %*% as.matrix(original)
class <- factor(ecor_R$salinisati)

# rda
cwmRDA_new <- rda(ecor_cwm_new ~ cond, data = ecor_R)
anova(cwmRDA_new, strata = ecor_R$year, step = 1000)

# 2D plot
class <- factor(ecor_R$salinisati)
scl = 3
colvec <- c("forestgreen", "steelblue", "red")
plot(cwmRDA_new, display = "sites", type = "none")
points(
  cwmRDA_new,
  display = "sites",
  col = colvec[class],
  pch = 25,
  cex = 0.8
)
text(cwmRDA_new,
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
ordisurf(cwmRDA_new ~ cond, data = ecor_R, add = TRUE)

# preprae results of traits characterizing down and upstream
trait_rda_res <-
  data.frame(RDA1 = scores(cwmRDA_new)$species[, 1],
             traits = rownames(scores(cwmRDA_new)$species))

# merge with trait description
trait_rda_res <-
  merge(trait_rda_res, trait_ext_lookup, by.x = "traits", by.y = "ID_trait")

# outlier detection (via Mahalanobis-Distance) 
responding <- mahalanobis(matrix(trait_rda_res$RDA1), mean(trait_rda_res$RDA1),
                          cov(matrix(trait_rda_res$RDA1))) > qchisq(0.975, df = 1)

# traits responding to high salinity
traitdf_high_sal <- trait_rda_res[responding,]
traitdf_high_sal[order(traitdf_high_sal$RDA1),]

# Plot scores on constrained axis
class <- factor(ecor_R$salinisati)
colors <- c("forestgreen", "steelblue", "red")

# TODO: Needs improvement, especially label size
# png(filename = file.path(data_out, "RDA_traits.png"),
#     width = 1600,
#     height = 866)
par(mar = c(5, 4, 4, 15))
par(cex.lab=1.1) # is for y-axis
par(cex.axis=1.1) # is for x-axis
boxplot(
  scores(cwmRDA_new)$sites[, 1] ~ class,
  ylab = "Scores on constrained axis",
  xlab = "Position",
  col = colors,
  main = "RDA of traits constrained by electric conductivity using harmonized traits"
)
abline(h = 0, lty = "dotted")
rug(trait_rda_res$RDA1, side = 4)
linestack(
          traitdf_high_sal$RDA1,
  labels = paste(traitdf_high_sal$ID_trait_name, traitdf_high_sal$ID_name, sep = ":"),
  at = par("usr")[2],
  add = TRUE,
  hoff = 1,
  cex = 0.9
)
# dev.off()

# Trait distribution on first axis ----
trait_rda_res$trait_comb <-
  paste(trait_rda_res$ID_trait_name, trait_rda_res$ID_name, sep = ":")
setDT(trait_rda_res)

# all traits
ggplot(trait_rda_res, 
       aes(x = reorder(trait_comb, RDA1), y = RDA1)) +
  geom_point() +
  theme_bw() +
  labs(y = "RDA1", x = "Trait") +
  coord_flip()

# only harmonized traits
ggplot(trait_rda_res[ID_trait_name %like% "Feeding|Locomotion|Reproduction|Respiration|Body|Voltinism|Life.*cycle", ], 
       aes(x = reorder(trait_comb, RDA1), y = RDA1)) +
  geom_point(size = 2) +
  labs(y = "RDA1", x = "Trait") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))+
  coord_flip()
# ggsave(filename = file.path(data_out, "Trait_distrb_harmonized.png"))
# rect <- data.frame (
#   xmin = c(0.5, 109.5),
#   xmax = c(5.5, 113.5),
#   ymin = -Inf,
#   ymax = Inf,
#   col = c("up", "down")
# )
# trait_distr_col <- trait_distr +
#   geom_rect(
#     data = rect,
#     aes(
#       xmin = xmin,
#       xmax = xmax,
#       ymin = ymin,
#       ymax = ymax,
#       fill = col
#     ),
#     alpha = 0.5,
#     inherit.aes = FALSE
#   ) +
#   guides(fill = FALSE) +
#   scale_fill_manual(values = colors[c(1, 3)])

# extract only traits related to high salinity ----

# high salinity traits
traitdf_high_sal <- traitdf_high_sal[traitdf_high_sal$RDA1 > 0,]

# subset raw data
ecor_cwm_new <- as.data.frame(ecor_cwm_new)

# add high salinity traits
trait_sal_output <-
  data.frame(ecor_cwm_new[, names(ecor_cwm_new) %in% (traitdf_high_sal$traits)])

# add life cycle duration as well > 1
# trait_edi_lookup[ID_trait_name %in% "Life cycle duration"]
trait_sal_output <-
  cbind(trait_sal_output, ecor_cwm_new$X2.2)

# merge environmental and meta variables
trait_sal_output <-
  cbind(trait_sal_output, ecor_R[, c("site", "cond", "newdat", "salinisati", "year")])
trait_sal_output$newdat <- factor(trait_sal_output$newdat)

# change col name to X2.2
names(trait_sal_output)[5] <- "X2.2"

# bring to long format
trait_sal_output <-
  melt(trait_sal_output, id = c("site", "cond", "newdat", "salinisati", "year"))

# merge with trait_lookup
trait_sal_output <-
  merge(trait_sal_output, trait_ext_lookup, by.x = "variable", by.y = "ID_trait")
trait_sal_output$facet <-
  paste(trait_sal_output$ID_trait_name, trait_sal_output$ID_name, sep = ":")

# remove transition sites
trait_sal_output <-
  trait_sal_output[trait_sal_output$salinisati != "transition",]

# plot trait proportions for each responding trait individually
# ggplot(trait_sal_output, aes(x = salinisati, y = value)) +
#   geom_boxplot(fill = "grey80") +
#   facet_wrap( ~ facet, scales = "free_x") +
#   theme_bw() +
#   labs(y = "Proportion", x = "Position")

# colorized version
# ggplot(trait_sal_output, aes(x = salinisati, y = value, fill = salinisati)) +
#   geom_boxplot() +
#   facet_wrap(~ facet, scales = "free_x") +
#   theme_bw() +
#   labs(y = "Proportion", x = "Position") +
#   scale_fill_manual(values = colors[c(1, 3)])

# ____________________________________________________________________________________________
#### Modelling trait proportions ####
# ____________________________________________________________________________________________
trait_sal_output$salinisati <- factor(trait_sal_output$salinisati)
trait_sal_output$year <- factor(trait_sal_output$year)
trait_sal_output <- droplevels(trait_sal_output)

# Logit transfomation
trait_sal_output$logit_value <- car:::logit(trait_sal_output$value)

# transform to wide format
# trait_c <-
#   dcast(trait_sal_output,
#         site + cond + newdat + salinisati + year ~ variable,
#         value.var = "logit_value")

# plot splitted by year
# ggplot(trait_sal_output, aes(x = salinisati, y = value, fill = year)) +
#   geom_boxplot(colour = "black") +
#   facet_wrap(~ facet, scales = "free_x") +
#   theme_bw() +
#   theme(legend.position = c(.8, .25)) +
#   labs(y = "Proportion", x = "Position", fill = 'Year') +
#   scale_fill_grey(start = 0.3, end = 1)
levels(trait_sal_output$salinisati) <- c('upstream', 'downstream')
ggplot(trait_sal_output, aes(x = salinisati, y = value, fill = year)) +
  geom_boxplot() +
  facet_wrap(~ facet, scales = "free_x") +
  theme_bw() +
#  theme(legend.position = c(0.95, .35)) +
  labs(y = "Proportion", x = "Position", fill = 'Year') +
  scale_fill_brewer(palette = 'Dark2')
# ggsave(filename = file.path(data_out, "Trait_proportion_harmonized.png"),
#        width = 22, height = 14,
#        units = "cm")

# lm ----
lm_results_new <- trait_sal_output %>%
  group_by(variable) %>%
  do(tidy(lm(logit_value ~ salinisati*year, data=.))) 

# filter for significant ones
lm_results_new %>% 
  filter(., p.value <= 0.05) 
# lm_results_new %>% 
#   filter(., p.value <= 0.1) 

# latex table
data.table::dcast(lm_results_new, term ~ variable,
                  value.var = "estimate") %>%
  xtable_wo_rownames(
    .,
    digits = 3,
    caption = "Results of linear models for the five selected traits. Trait proportions were logit transformed prior model building, estimates are on the logit scale.
    Although years were statistically not significant we kept this factor in the model to avoid temporal autocorrelation. Bold values indicate statistically
    significant effects (p < 0.05)."
  )

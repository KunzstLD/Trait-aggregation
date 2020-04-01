# ____________________________________________________________________________________________
#### Trait-responses ####
# Most code taken from Eduard Szöcs and adapted:
# script "do_ecor.R"
# ____________________________________________________________________________________________

# ____________________________________________________________________________________________
#### cwmRDA ####
# ____________________________________________________________________________________________

# Calculate weighted trait-abundances:
# subset taxon pool in ecor_L to taxa in trait_eu_sal
ecor_L <-
  ecor_L[, names(ecor_L)[names(ecor_L) %in% rownames(trait_eu_sal)]]

# weight by square-root transformed abundance
trans <- 0.5
ecor_cwm <-
  prop.table(as.matrix(ecor_L ^ trans), 1) %*% as.matrix(trait_eu_sal)
class <- factor(ecor_R$salinisati)


cwmRDA <- rda(ecor_cwm ~ cond, data = ecor_R)
summary(cwmRDA)
anova(cwmRDA, strata = ecor_R$year, step = 1000)

# 2D plot
class <- factor(ecor_R$salinisati)
scl = 3
colvec <- c("red", "yellow", "green")
plot(cwmRDA, display = "sites", type = "none")
points(
  cwmRDA,
  display = "sites",
  col = colvec[class],
  pch = 16,
  cex = 0.8
)
text(cwmRDA,
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
ordisurf(cwmRDA ~ cond, data = ecor_R, add = TRUE)

# preprae results of traits characterizing down and upstream
traitdf <-
  data.frame(RDA1 = scores(cwmRDA)$species[, 1],
             traits = rownames(scores(cwmRDA)$species))
traitdf <-
  merge(traitdf, trait_ext_lookup, by.x = "traits", by.y = "ID_trait")

# outlier detection (via Mahalanobis-Distance) 
responding <- mahalanobis(matrix(traitdf$RDA1), mean(traitdf$RDA1),
                          cov(matrix(traitdf$RDA1))) > qchisq(0.975, df = 1)

# traits responding to high salinity
traitdf_high_sal <- traitdf[responding,]
traitdf_high_sal[order(traitdf_high_sal$RDA1),]
# Plot scores on constrained axis
# Similiar picture with a few differences compared to Szöcs et al 2014:
# Harmonized traits are those traits responding  the most to the salinity gradient
# Highest scores on the constrained axis: Bi/or multivoltine species
# (in Szöcs et al univoltine)
# Oviposition ovoviviparity
# (in Szöcs et al: clutches, cemented or fixed)
# Lowest scores on the constrained axis: 
# feed_shredder, resp_gil
# (in Szöcs et al: shredder, gil, multivoltine)
# univoltine, aqutic oviposition
# (in Szöcs et al: multivoltine, ovoviviparity)
class <- factor(ecor_R$salinisati)

colors <- c("forestgreen", "steelblue", "red")
par(mar = c(5, 4, 4, 15))
boxplot(
  scores(cwmRDA)$sites[, 1] ~ class,
  ylab = "Scores on constrained axis",
  xlab = "Position",
  col = colors
)
abline(h = 0, lty = "dotted")
rug(traitdf$RDA1, side = 4)
linestack(
  traitdf2$RDA1,
  labels = paste(traitdf2$ID_trait_name, traitdf2$ID_name, sep = ":"),
  at = par("usr")[2],
  add = TRUE,
  hoff = 1
)

traitdf$trait_comb <-
  paste(traitdf$ID_trait_name, traitdf$ID_name, sep = ":")
ggplot(traitdf, aes(x = reorder(traits, RDA1), y = RDA1)) +
  geom_point() +
  coord_flip()

# Plot Trait distribution on first axis
trait_distr <-
  ggplot(traitdf, aes(x = reorder(trait_comb, RDA1), y = RDA1)) +
  geom_point() +
  theme_bw() +
  labs(y = "RDA1", x = "Trait") +
  coord_flip()

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

#### extract only traits related to high salinity ####
traitdf_high_sal <- traitdf_high_sal[traitdf_high_sal$RDA1 < 0,]

# subset raw data
ecor_cwm <- as.data.frame(ecor_cwm)
trait_resp <-
  data.frame(ecor_cwm[, names(ecor_cwm) %in% traitdf_high_sal$traits])
trait_resp <-
  cbind(trait_resp, ecor_R[, c("site", "cond", "newdat", "salinisati", "year")])
trait_resp$newdat <- factor(trait_resp$newdat)

# bring to long format
trait_resp_m <-
  melt(trait_resp, id = c("site", "cond", "newdat", "salinisati", "year"))

# merge with trait_lookup
trait_resp_m <-
  merge(trait_resp_m, trait_ext_lookup, by.x = "variable", by.y = "ID_trait")
trait_resp_m$facet <-
  paste(trait_resp_m$ID_trait_name, trait_resp_m$ID_name, sep = ":")

# remove transition sites
trait_resp_m <-
  trait_resp_m[trait_resp_m$salinisati != "transition",]

# plot
traits_bw <- ggplot(trait_resp_m, aes(x = salinisati, y = value)) +
  geom_boxplot(fill = "grey80") +
  facet_wrap( ~ facet, scales = "free_x") +
  theme_bw() +
  labs(y = "Proportion", x = "Position")

traits_col <-
  ggplot(trait_resp_m, aes(x = salinisati, y = value, fill = salinisati)) +
  geom_boxplot() +
  facet_wrap( ~ facet, scales = "free_x") +
  theme_bw() +
  labs(y = "Proportion", x = "Position") +
  scale_fill_manual(values = colors[c(1, 3)])

# ____________________________________________________________________________________________
#### Modelling ####
# ____________________________________________________________________________________________

trait_resp_m$salinisati <- factor(trait_resp_m$salinisati)
trait_resp_m$year <- factor(trait_resp_m$year)
trait_resp_m <- droplevels(trait_resp_m)

# Logit transfomation
trait_resp_m$logit_value <- car:::logit(trait_resp_m$value)

# transform to wide format
trait_c <-
  dcast(trait_resp_m,
        site + cond + newdat + salinisati + year ~ variable,
        value.var = "logit_value")

# plot splitted by year
ggplot(trait_resp_m, aes(x = salinisati, y = value, fill = year)) +
  geom_boxplot(colour = "black") +
  facet_wrap(~ facet, scales = "free_x") +
  theme_bw() +
  theme(legend.position = c(.8, .25)) +
  labs(y = "Proportion", x = "Position", fill = 'Year') +
  scale_fill_grey(start = 0.3, end = 1)

levels(trait_resp_m$salinisati) <- c('upstream', 'downstream')
ggplot(trait_resp_m, aes(x = salinisati, y = value, fill = year)) +
  geom_boxplot() +
  facet_wrap(~ facet, scales = "free_x") +
  theme_bw() +
  theme(legend.position = c(.8, .25)) +
  labs(y = "Proportion", x = "Position", fill = 'Year') +
  scale_fill_brewer(palette = 'Dark2')

#### independend lm ####
lm_volt_uni <- lm(volt_uni ~ salinisati*year, data = trait_c)
summary(lm_volt_uni)

lm_ovip_aqu <- lm(ovip_aqu ~ salinisati*year, data = trait_c)
summary(lm_volt_uni)

# sink(file = file.path(tabdir, 'tab_sep_lm.tex'))
# stargazer(
#   lm_X2.2,
#   lm_X3.3,
#   lm_X5.1,
#   lm_X8.2,
#   lm_X11.3,
#   title = 'Results of linear models for the five selected traits.
#   Trait proportions were logit transformed prior model building,
#   therefore the results are on the logit scale. Numbers in parentheses give the Standard Errors
#   of the estimates.',
#   label = 'tab:sep_lm',
#   covariate.labels = c(
#     'Intercept (=upstream)',
#     'downstream',
#     'Year 2008',
#     'Year 2009',
#     'downstream x 2008',
#     'downstream x 2009'
#   ),
#   dep.var.labels = c(
#     '\\specialcell{Life cycle duration: \\\\ > 1y}',
#     '\\specialcell{Potential cycles per year: \\\\ >1}',
#     '\\specialcell{Reproduction: \\\\ ovoviviparity}',
#     '\\specialcell{Respiration: \\\\ gill}',
#     '\\specialcell{Feeding habits: \\\\ shredder}'
#   ),
#   intercept.top = TRUE,
#   model.numbers = FALSE,
#   model.names = FALSE,
#   notes.align = 'l',
#   omit.stat = c('adj.rsq', 'ser')
# )
# sink()

# TODO: probably not relevant:check later
# means
# df_means <- plyr::ddply(trait_resp_m, .(salinisati, facet), summarise,
#                   mean = mean(value))
# df_means_c <- dcast(df_means, facet ~ salinisati)
# df_means_c$diff <- df_means_c$down - df_means_c$up
# df_means_c

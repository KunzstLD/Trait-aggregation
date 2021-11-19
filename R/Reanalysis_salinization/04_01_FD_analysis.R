# ____________________________________________________________________________________________
# Analyse functional diversity metrics
# - functional richness: functional space filled (most extreme points of convex hull)
# - functional evenness: evenness/regularity of species abundances in trait space
#  [0 - 1]
#  (decreases if distances between taxa are less regular, less evenly distributed)
# - functional divergence: How is abundance distributed within the trait space volume
#  (2D case: would be high if high number (most abundant species have high trait values,
#  divergence is high)
#  [0 - 1]
# ____________________________________________________________________________________________

# Read in fd data & (for multiple samples corrected) environmental data
fd_res <- readRDS(file = file.path(data_cache,
                                   "Re-analysis_cache", "fd_res.rds"))
ecor_L_high <- readRDS(file = file.path(
  data_cache,
  "Re-analysis_cache",
  "ecor_L_multiple_samp_corr.rds"
))
ecor_R_high <- readRDS(file = file.path(
  data_cache,
  "Re-analysis_cache",
  "ecor_R_multiple_samp_corr.rds"
))
setDT(ecor_R_high)

# Data processing pipeline ----
fd_res <-
  lapply(fd_res, function(y)
    as.data.frame(y[c("FRic", "FEve", "FDiv")]))
fd_communities <- lapply(fd_res, rownames)
fd_res <- mapply(cbind, fd_res, community = fd_communities,
                 SIMPLIFY = FALSE) %>%
  rbindlist(., idcol = "dataset")

# order of datasets
fd_res[, dataset := factor(
  dataset,
  levels = c(
    "original",
    "not_aggregated",
    "direct_median",
    "direct_mean",
    "stepw_median",
    "stepw_mean",
    "weighted"
  )
)]

# Plotting: Overview of trends in FD metrics ----
# FRic
fd_res_lf <- melt(fd_res,
                  id.vars = c("dataset", "community"),
                  variable.name = "fd_indices")
fd_res_lf[, community := as.numeric(community)]

fd_res_lf[fd_indices == "FRic",] %>%
  ggplot(., aes(x = as.factor(community),
                y = value)) +
  geom_point(size = 2) +
  facet_wrap(~ as.factor(dataset)) +
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = "Roboto Mono", size = 11),
    panel.grid = element_blank(),
    plot.margin = margin(5.5, 0, 5.5, 5.5, "pt")
  )

# FEve & FDiv
fd_res_lf[fd_indices %in% c("FEve", "FDiv"), ] %>%
  ggplot(., aes(x = as.factor(community),
                y = value)) +
  geom_point(size = 2) +
  facet_grid(as.factor(fd_indices) ~ as.factor(dataset)) +
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = "Roboto Mono", size = 11),
    panel.grid = element_blank(),
    plot.margin = margin(5.5, 0, 5.5, 5.5, "pt")
  )


# Correlation ----

## Correlate FD metrics with factor year and extract residuals ----
# ...to remove the effect of year
fd_res_lf[ecor_R_high,
          year := i.year,
          on = c(community = "sample_id")]
fd_res_lf[, year := factor(year)]

# helper fun to extract residuals from lm between year and FD value
fun <- function(dt, x, y) {
  mod <- lm(y ~ x, data = dt)
  residuals(mod)
}
fd_res_lf[, residuals := fun(dt = .SD,
                             x = year,
                             y = value),
          by = .(dataset, fd_indices)]

# prepare dataset (residuals original are an additional column)
fd_res_comp <- merge(
  x = fd_res_lf[dataset != "original", .(dataset,
                                         community,
                                         fd_indices,
                                         residuals)],
  y = fd_res_lf[dataset == "original", .(community,
                                         fd_indices,
                                         residuals)],
  by = c("community", "fd_indices"),
  suffixes = c("_other", "_original")
)


## FRic , FEve & FDiv ----
# fit residuals of FD metrics of aggregated and harmonised datasets
# against residuals of the original
# and extract summary
fd_summary <- fd_res_comp[,  lm_fd(dt = .SD,
                                   x = residuals_original,
                                   y = residuals_other),
                          by = .(dataset, fd_indices)]
fd_summary[, r2 := ifelse(r2 < 0.01, round(r2, digits = 3),
                          round(r2, digits = 2))]
fd_summary[, p_value_cutoff := fcase(
  p_value > 0.1,
  "p-value greater 0.1",
  p_value %between% c(0.1, 0.05),
  ".",
  p_value %between% c(0.05, 0.01),
  "*",
  p_value %between% c(0.01, 0.001),
  "**",
  p_value < 0.001,
  "***"
)]

# Plotting
dataset_names <- c(
  "not_aggregated" = "Harmonised; not aggregated",
  "direct_median" = "Direct_agg (median)",
  "direct_mean" = "Direct_agg (mean)",
  "stepw_median" = "Stepwise_agg (median)",
  "stepw_mean" = "Stepwise_agg (mean)",
  "weighted" = "Weighed_agg"
)

# Add coordinates for annotations
fd_summary[fd_indices == "FRic", `:=`(x = rep(-50, 6),
                                      y = rep(100, 6))]

### Plot FRic ----
lm_plot(
  dt = fd_res_comp[fd_indices == "FRic",],
  x_var = residuals_original,
  y_var = residuals_other,
  dt_text = fd_summary[fd_indices == "FRic",],
  xlab = "FRic Szöcs et al. 2014",
  ylab = "FRic aggregated/harmonised datasets"
)
ggsave(
  file.path(data_paper, "Correlation_fd_metrics_FRic.png"),
  width = 25,
  height = 17,
  unit = "cm"
)

### Plot FEve ----
fd_summary[fd_indices == "FEve", `:=`(x = rep(-0.3, 6),
                                      y = rep(0.6, 6))]
lm_plot(
  dt = fd_res_comp[fd_indices == "FEve",],
  x_var = residuals_original,
  y_var = residuals_other,
  dt_text = fd_summary[fd_indices == "FEve",],
  xlab = "FEve Szöcs et al. 2014",
  ylab = "FEve aggregated/harmonised datasets"
)
ggsave(
  file.path(data_paper, "Correlation_fd_metrics_FEve.png"),
  width = 25,
  height = 17,
  unit = "cm"
)

### Plot FDiv ----
fd_summary[fd_indices == "FDiv", `:=`(x = rep(-0.3, 6),
                                      y = rep(0.6, 6))]
lm_plot(
  dt = fd_res_comp[fd_indices == "FDiv",],
  x_var = residuals_original,
  y_var = residuals_other,
  dt_text = fd_summary[fd_indices == "FDiv",],
  xlab = "FDiv Szöcs et al. 2014",
  ylab = "FDiv aggregated/harmonised datasets"
)
ggsave(
  file.path(data_paper, "Correlation_fd_metrics_FDiv.png"),
  width = 25,
  height = 17,
  unit = "cm"
)

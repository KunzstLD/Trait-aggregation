class <- factor(ecor_R$salinisati)
site_scores[, class := rep(class, 7)]

results_high_low_sal[ID_trait_name == "Life cycle duration", ID_name := paste("lcd: ",ID_name)]
results_high_low_sal[ID_trait_name == "Potential number of cycles per year", ID_name := paste("nr.cy:", ID_name)]
results_high_low_sal[grepl("shredder", ID_name), ID_name := sub("(shredder)(.+)", "\\1", ID_name)]
results_high_low_sal[grepl("^gill$", ID_name), ID_name := "gills"]

# Original -----------------------------------------------------------------------------------------
plot_orig <- fun_bxp_tcomp(site_scr = site_scores[id == "original", ],
                           species_scr = species_scores[id == "original", RDA_species_scores],
                           title = "Original")

axis_orig <-
  fun_axis(data_axis = results_high_low_sal[id == "original", RDA_species_scores],
           trait = results_high_low_sal[id == "original", ID_name],
           limits_y = site_scores[id == "original", RDA_site_scores],
           nudge_x = 0.55)
final_orig <- plot_orig|axis_orig


# Not aggregated -----------------------------------------------------------------------------------
plot_not_aggr <-
  fun_bxp_tcomp(site_scr = site_scores[id == "not_aggregated",],
                species_scr = species_scores[id == "not_aggregated", RDA_species_scores],
                title = "Harmonised; not aggregated")

axis_not_aggr <-
  fun_axis(data_axis = results_high_low_sal[id == "not_aggregated", RDA_species_scores],
           trait = results_high_low_sal[id == "not_aggregated", ID_name],
           limits_y = site_scores[id == "not_aggregated", RDA_site_scores])

final_not_aggr <- plot_not_aggr|axis_not_aggr


# Stepwise median ----------------------------------------------------------------------------------
plot_stepw_median <-
  fun_bxp_tcomp(site_scr = site_scores[id == "stepw_median", ],
                species_scr = species_scores[id == "stepw_median", RDA_species_scores],
                title = "Stepwise_agg (median)")

axis_stepw_median <-
  fun_axis(data_axis = results_high_low_sal[id == "stepw_median", RDA_species_scores],
           trait = results_high_low_sal[id == "stepw_median", ID_name],
           limits_y = site_scores[id == "stepw_median", RDA_site_scores])

final_stepw_median <- plot_stepw_median|axis_stepw_median

# Direct mean --------------------------------------------------------------------------------------
plot_dir_mean <-
  fun_bxp_tcomp(site_scr = site_scores[id == "direct_mean", ],
                species_scr = species_scores[id == "direct_mean", RDA_species_scores],
                title = "Direct_agg (mean)")

axis_dir_mean <-
  fun_axis(data_axis = results_high_low_sal[id == "direct_mean", RDA_species_scores],
           trait = results_high_low_sal[id == "direct_mean", ID_name],
           limits_y = site_scores[id == "direct_mean", RDA_site_scores])
final_dir_mean <- plot_dir_mean|axis_dir_mean


scr_combined <- final_orig/final_not_aggr|final_stepw_median/final_dir_mean
for (path in c(data_out, data_paper)) {
  ggplot2::ggsave(
    filename = file.path(path, "boxplot_scores_combined.png"),
    plot = scr_combined,
    width = 27.5,
    height = 22,
    units = "cm"
  )
}

# Direct median ----------------------------------------------------------------------------------
plot_dir_median <-
  fun_bxp_tcomp(site_scr = site_scores[id == "direct_median", ],
                species_scr = species_scores[id == "direct_median", RDA_species_scores],
                title = "Direct_agg (median)")

axis_dir_median <-
  fun_axis(data_axis = results_high_low_sal[id == "direct_median", RDA_species_scores],
           trait = results_high_low_sal[id == "direct_median", ID_name],
           limits_y = site_scores[id == "direct_median", RDA_site_scores])
final_dir_median <- plot_dir_median|axis_dir_median


# Stepwise mean ------------------------------------------------------------------------------------
plot_stepw_mean <-
  fun_bxp_tcomp(site_scr = site_scores[id == "stepw_mean", ],
                species_scr = species_scores[id == "stepw_mean", RDA_species_scores],
                title = "Stepwise_agg (mean)")

axis_stepw_mean <-
  fun_axis(data_axis = results_high_low_sal[id == "stepw_mean", RDA_species_scores],
           trait = results_high_low_sal[id == "stepw_mean", ID_name],
           limits_y = site_scores[id == "stepw_mean", RDA_site_scores])

final_stepw_mean <- plot_stepw_mean|axis_stepw_mean

# Weighted mean ------------------------------------------------------------------------------------
plot_weighted <-
  fun_bxp_tcomp(site_scr = site_scores[id == "weighted", ],
                species_scr = species_scores[id == "weighted", RDA_species_scores],
                title = "Weighted_agg")

axis_weighted <-
  fun_axis(data_axis = results_high_low_sal[id == "weighted", RDA_species_scores],
           trait = results_high_low_sal[id == "weighted", ID_name],
           limits_y = site_scores[id == "weighted", RDA_site_scores])

final_weighted <- plot_weighted|axis_weighted


# Just the remaining 
scr_remaining_SI <- final_dir_median/final_stepw_mean/final_weighted
for (path in c(data_out, data_paper)) {
  ggplot2::ggsave(
    filename = file.path(path, "boxplot_scores_combined_REMAIN_SI.png"),
    plot = scr_remaining_SI,
    width = 35,
    height = 25,
    units = "cm"
  )
}

# All plots - looks a bit ugly in publication
scr_combined_SI <- wrap_plots(
  final_orig,
  final_not_aggr,
  final_dir_median,
  final_dir_mean,
  final_stepw_median,
  final_stepw_mean,
  final_weighted,
  ncol = 2
)
for (path in c(data_out, data_paper)) {
  ggplot2::ggsave(
    filename = file.path(path, "boxplot_scores_combined_SI.png"),
    plot = scr_combined_SI,
    width = 35,
    height = 25,
    units = "cm"
  )
}


# ___________________________________________________
#### Plotting ####
# ___________________________________________________

# How many families per order and variable (== unique families per order)
traitval_noa[, fam_per_order_variable := .N , by = c("order", "variable")]
traitval_noa[deviance_comp_fam != 0, .(family, order, variable, fam_per_order_variable)] %>% 
  View()

# order as factor 
traitval_noa[, order := as.factor(order)]

# change levels for column order 
# traitval_noa <- traitval_noa %>%
#   mutate(
#     order = fct_relevel(
#       order,
#       traitval_noa[!duplicated(order), .(order, fam_per_order_var)] %>% 
#         .[order(fam_per_order_var), order]
#     )
#   ) 
# setDT(traitval_noa)

# plot
plot_list <- list()
for (grf in unique(traitval_noa$grouping_feature)) {
  first_trait <-
    traitval_noa[grouping_feature %in% grf, unique(variable)[1]]
  lvl <- traitval_noa[grouping_feature %in% grf, unique(variable)]

  # create annotations
  anno <- data.frame(
    order = c(8:1),
    deviance_comp_fam = rep(-0.8, 8),
    lab = c(
      paste("N =", traitval_noa[!duplicated(order) &
                                  order %in% "Trichoptera", fam_per_order_variable]),
      paste("N =", traitval_noa[!duplicated(order) &
                                  order %in% "Plecoptera", fam_per_order_variable]),
      paste("N =", traitval_noa[!duplicated(order) &
                                  order %in% "Odonata", fam_per_order_variable]),
      paste("N =", traitval_noa[!duplicated(order) &
                                  order %in% "Megaloptera", fam_per_order_variable]),
      paste("N =", traitval_noa[!duplicated(order) &
                                  order %in% "Hemiptera", fam_per_order_variable]),
      paste("N =", traitval_noa[!duplicated(order) &
                                  order %in% "Ephemeroptera", fam_per_order_variable]),
      paste("N =", traitval_noa[!duplicated(order) &
                                  order %in% "Diptera", fam_per_order_variable]),
      paste("N =", traitval_noa[!duplicated(order) &
                                  order %in% "Coleoptera", fam_per_order_variable])
    ),
    variable = factor(first_trait,
                      levels = lvl)
  )

  set.seed(123)
  plot_list[[grf]] <-
    ggplot(traitval_noa[grouping_feature %in% grf,],
           aes(x = order, y = deviance_comp_fam)) +
    geom_boxplot(
      size = 0.5,
      alpha = 0.9,
      aes(col = order),
      outlier.size = 5
    ) +
    geom_jitter(
      size = 5,
      alpha = 0.4,
      height = 0.0,
      width = 0.2,
      aes(col = order)
    ) +
    geom_text(data = anno, label = anno$lab) +
    geom_hline(aes(yintercept = 0), color = "black", size = 0.6) +
    scale_y_continuous(limits = c(-1.1, 1.1),
                       expand = c(0.005, 0.005)) +
    scale_color_d3() +
    coord_flip() +
    labs(x = "Order",
         y = "Deviance stepwise aggregation and \n trait assignments on family-level") +
    facet_wrap( ~ variable, nrow = 2) +
    theme_light(base_size = 15, base_family = "Poppins") +
    theme(
      legend.position = "none",
      axis.title = element_text(size = 13),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11)
      #    panel.grid = element_blank()
    )
  ggsave(file.path(data_out, paste0("trait_deviations_stepwise_famlvl_", names(plot_list[grf]), ".png")),
         plot_list[[grf]],
         dpi = 400,
         width = 13,
         height = 7)
}
plot_list


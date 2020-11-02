#### Retrieve tax. hierarchy ####
preproc_cp <- copy(preproc_dat)

# tax. hierarchy for each and tax. resolution
lapply(preproc_cp, function(y) retrieve_tax_hierarchy(x = y))

# overview tax. hierarchy
# most frequent case: few (1- ~5) genera
# & few species
# There are some extremes per DB, i.e. few families with
# a lot of different genera and sometimes also many species
# Trend: the more genera per family, more species
# Use frequencies of how many genera per family to
# explain differences (or lack thereof) in aggr. methods
# -> many families with 1 genus -> no difference between
# direct, weighted and stepwise
overview_hierarchy <- lapply(preproc_cp, function(y) {
  y[, .(
    nr_unique_genus,
    entry_on_species,
    family
  )] %>%
    .[!is.na(nr_unique_genus), ] %>%
    .[!duplicated(family), ]
})

# nr_unique_genus vs entry_on_species
overview_hierarchy %>%
  rbindlist(., idcol = "dataset") %>%
  .[dataset == "Trait_freshecol_2020_pp_harmonized", ] %>%
  .[nr_unique_genus <= 50, ] %>%
  ggplot(., aes(x = nr_unique_genus, y = entry_on_species)) +
  geom_point() +
  facet_wrap(~dataset) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 9),
    axis.text.y = element_text(family = "Roboto Mono", size = 9)
  )

#
lapply(overview_hierarchy, function(y) {
  Hmisc::describe(y)
})

# derive frequency unique genus entries
lapply(overview_hierarchy, function(y) {
  Hmisc::describe(y) %>%
    .[["nr_unique_genus"]] %>%
    .[["values"]]
}) %>%
  rbindlist(., idcol = "dataset")

# frequency species entries
lapply(overview_hierarchy, function(y) {
  Hmisc::describe(y) %>%
    .[["entry_on_species"]] %>%
    .[["values"]]
}) %>%
  rbindlist(., idcol = "dataset")

# plot
lapply(preproc_cp, function(y) {
  y[, .(
    nr_unique_genus,
    entry_on_species,
    family
  )] %>%
    .[!is.na(nr_unique_genus), ] %>% # don't regard entries on family-lvl
    .[!duplicated(family), ]
}) %>%
  rbindlist(., idcol = "dataset") %>%
  melt(., measure.vars = c(
    "nr_unique_genus",
    "entry_on_species"
  )) %>%
  ggplot(.) +
  geom_violin(aes(x = as.factor(dataset), y = value)) +
  facet_wrap(~variable) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 9),
    axis.text.y = element_text(family = "Roboto Mono", size = 9),
  )

# bind
overview_bind <- overview_hierarchy %>%
  rbindlist(., idcol = "dataset")


#### Plot taxonomic hierarchy ####

tax_hierarchy_plot <- function(plotting_data,
                               label_unique_genera = 7,
                               y_lim_max = 110,
                               annotate_region = NULL) {
  plotting_data %>%
    ggplot(.) +
    geom_point(aes(x = 1,
                   y = nr_unique_genus),
               size = 1.1,
               color = "black") +
    geom_point(aes(x = 2,
                   y = entry_on_species),
               size = 1.1,
               color = "black") +
    geom_segment(
      aes(
        x = 1,
        xend = 2,
        y = nr_unique_genus,
        yend = entry_on_species
      ),
      size = .5,
      color = "black",
      alpha = .5
    ) +
    geom_text_repel(
      data = ~ .x[nr_unique_genus > label_unique_genera, ],
      mapping = aes(
        x = 1,
        y = nr_unique_genus,
        label = as.factor(family)
      ),
      hjust = 1.2,
      size = 3,
      nudge_x = -0.02,
      direction = "y",
      segment.size  = 0.1,
      segment.color = "gray20"
    ) +
    geom_vline(xintercept = 1,
               linetype = "dashed",
               size = .1) +
    geom_vline(xintercept = 2,
               linetype = "dashed",
               size = .1) +
    annotate(
      "rect",
      xmin = 1,
      xmax = 2,
      ymin = 0,
      ymax = 5,
      alpha = .2,
      color = "steelblue",
      size = 1.1
    ) +
    annotate(
      "text",
      x = 2.25,
      y = 2.5,
      family = "Poppins",
      size = 2.8,
      color = "gray20",
      label = paste(perc_gen, "% of families have \n 5 or less genera.")
    ) +
    annotate(
      "text",
      label = paste("Genera per family \n", annotate_region),
      x = 0.75,
      y = y_lim_max,
      size = 4,
      family = "Poppins"
    ) +
    annotate(
      "text",
      label = paste("Species per genus \n", annotate_region),
      x = 2.25,
      y = y_lim_max,
      size = 4,
      family = "Poppins"
    ) +
    xlim(0.5, 2.5) +
    ylim(0, y_lim_max) +
    labs(x = "", y = "Number of taxa") +
    theme_classic() +
    theme(
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(family = "Roboto Mono", size = 11)
    )
}

# AUS
dat_AUS <- overview_hierarchy$Trait_AUS_harmonized
perc_gen <- dat_AUS[nr_unique_genus <= 5,
                    round(.N / nrow(dat_AUS) * 100,
                          digits = 2)]
tax_hierarchy_plot(plotting_data = dat_AUS,
                   label_unique_genera = 7, 
                   annotate_region = "AUS") 
for(link in c(data_out, data_paper)) {
  ggplot2::ggsave(
    filename = file.path(link, "taxonomic_hierarchy_AUS.png"),
    width = 22,
    height = 12,
    units = "cm"
  )
}

# EU
dat_EU <- overview_hierarchy$Trait_freshecol_2020_pp_harmonized
perc_gen <- dat_EU[nr_unique_genus <= 5,
                    round(.N / nrow(dat_EU) * 100,
                          digits = 2)]
tax_hierarchy_plot(plotting_data = dat_EU,
                   label_unique_genera = 10, 
                   y_lim_max = 400, 
                   annotate_region = "EU")
for(link in c(data_out, data_paper)) {
  ggplot2::ggsave(
    filename = file.path(link, "taxonomic_hierarchy_EU.png"),
    width = 22,
    height = 12,
    units = "cm"
  )
}

# NOA
dat_NOA <- overview_hierarchy$Traits_US_LauraT_pp_harmonized
perc_gen <- dat_NOA[nr_unique_genus <= 5,
                   round(.N / nrow(dat_NOA) * 100,
                         digits = 2)]
tax_hierarchy_plot(plotting_data = dat_NOA,
                   label_unique_genera = 15, 
                   y_lim_max = 210, 
                   annotate_region = "NOA")
for(link in c(data_out, data_paper)) {
  ggplot2::ggsave(
    filename = file.path(link, "taxonomic_hierarchy_NOA.png"),
    width = 22,
    height = 12,
    units = "cm"
  )
}

# NZ
dat_NZ <- overview_hierarchy$Trait_NZ_pp_harmonized
perc_gen <- dat_NZ[nr_unique_genus <= 5,
                    round(.N / nrow(dat_NZ) * 100,
                          digits = 2)]
tax_hierarchy_plot(plotting_data = dat_NZ,
                   label_unique_genera = 5, 
                   y_lim_max = 100,
                   annotate_region = "NZ")
for(link in c(data_out, data_paper)) {
  ggplot2::ggsave(
    filename = file.path(link, "taxonomic_hierarchy_NZ.png"),
    width = 22,
    height = 12,
    units = "cm"
  )
}







  
  
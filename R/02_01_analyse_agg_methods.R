# _____________________________________________________________________________
#### Analysis aggregation procedures ####
# _____________________________________________________________________________

# _____________________________________________________________________________
#### How many taxa end up with different trait values after aggregation?(%) ####
# _____________________________________________________________________________
taxa_diff <- lapply(results_agg, function(y) {
  y[, .(deviance, total = .N)] %>%
    .[deviance != 0, .(.N / total * 100,
                       total)] %>%
    .[!duplicated(total),]
}) %>% rbindlist(., idcol = "file")

setnames(taxa_diff, 
         old = c("file", 
                 "V1",
                 "total"), 
         new = c("Database",
                 "Deviating cases [%]",
                 "Number of cases"))
taxa_diff[, `Deviating cases [%]` := round(`Deviating cases [%]`, digits = 2)]
taxa_diff$Database <- c("Australia", "Europe", "New Zealand", "North America")
# latex output
xtable_wo_rownames(x = taxa_diff, auto = TRUE)

# _____________________________________________________________________________
#### Range of deviance #### 
# _____________________________________________________________________________
lapply(results_agg, function(y)
  y[deviance %between% c(-0.2, 0.2) & deviance != 0, .N] /
    y[deviance %between% c(-1, 1) & deviance != 0, .N])
lapply(results_agg, function(y)
  y[, Hmisc::describe(deviance)])

# if direct agg differed, how much? (%)
# does not include cases where value_genus_fam_agg was zero
# and value_direct_agg was different from zero
lapply(results_agg, function(y)
  y[deviance != 0 & value_genus_fam_agg != 0, 
    .(family, order, variable, perc_diff = abs(deviance)/value_genus_fam_agg)] %>% 
    .[perc_diff <= 0.2, ])


# _____________________________________________________________________________
#### For which families did the aggregation methods yield different trait values? ###
# _____________________________________________________________________________
lapply(results_agg, function(y)
  y[deviance != 0, .(family, order)] %>%
    .[!duplicated(family),])
# lapply(results_agg, function(y) y[deviance != 0 & family %in% "Baetidae", ])


# families per order
lapply(results_agg, function(y) y[deviance != 0, .(family, variable, order)] %>% 
         .[, .(family, variable, .N), by = "order"] %>% 
         .[order(-N)]) 
# lapply(results_agg, function(y) y[deviance > 0, ])
# lapply(results_agg, function(y) y[deviance < 0, ])

# _____________________________________________________________________________
#### Are there families where trait values diverge that occur in several/all datasets? ####
# _____________________________________________________________________________
lapply(results_agg, function(y) y[deviance != 0, .(family)]) %>%
  rbindlist(., idcol = "file") %>%
  dcast(., formula = family ~ file) %>%
  .[Trait_AUS_harmonized != 0 & Traits_US_LauraT_pp_harmonized != 0 &
      Trait_EU_pp_harmonized != 0, ]
# Trait_NZ_pp_harmonized != 0 & 

# _____________________________________________________________________________
#### Which traits were differently classified according to the aggregation methods? ####
# _____________________________________________________________________________
lapply(results_agg, function(y) y[, .N, by = "variable"])

lapply(results_agg, function(y) y[deviance != 0, .(.N, family),
                                  by = "variable"] %>% 
         .[order(-N), ])

lapply(results_agg, function(y) y[deviance != 0, .(family, variable)]) %>%
  rbindlist(., idcol = "file") %>%
  dcast(., formula = variable ~ file) %>% 
  .[order(Trait_AUS_harmonized), ]

lapply(results_agg, function(y) y[deviance != 0, .(family, variable)]) %>%
  rbindlist(., idcol = "file") %>% 
  .[family %in% "Leptophlebiidae", ]

results_agg[["Trait_AUS_harmonized"]][family %in% "Leptophlebiidae", ]

traitval_noa[, .N, by = "variable"] %>% .[, N]
traitval_noa[deviance_dir_fam != 0, .(.N),
             by = c("variable")] %>%
  .[order(-N), N / total]


# _____________________________________________________________________________
#### Specific traits that diverge in all databases? ####
# _____________________________________________________________________________
lapply(results_agg, function(y) y[deviance != 0, .(family, variable)]) %>%
  rbindlist(., idcol = "file") %>%
  dcast(., formula = variable ~ file) %>%
  .[Trait_AUS_harmonized != 0 & Traits_US_LauraT_pp_harmonized != 0 &
      Trait_NZ_pp_harmonized != 0 & Trait_EU_pp_harmonized != 0, ]

lapply(results_agg, function(y) y[deviance != 0 & variable %like% "feed.*gatherer", 
                                  .(family, order, variable)])


#### For which taxa are deviances greater than 0.5? ####
lapply(results_agg, function(y) Hmisc::describe(y$deviance))
lapply(results_agg, function(y) y[deviance >= 0.5, .(family, variable, 
                                                     deviance)])

# _____________________________________________________________________________
#### Plotting deviance data ####
# _____________________________________________________________________________

# Plot of deviance per family
# Example Australia
results_aus <- results_agg[["Trait_EU_pp_harmonized"]]

#results_aus[order %in% c("Trichoptera") & variable %like% "feed.*|resp.*|size.*|volt.*", ] %>%
results_aus[deviance != 0, ] %>%
  ggplot(., aes(
    x = as.factor(variable), y = deviance,
    label = deviance
  )) +
  geom_point(stat = "identity", aes(col = family), size = 12) +
  geom_segment(aes(y = 0, 
                   x = variable, 
                   yend = deviance, 
                   xend = variable,
                   col = family))+ 
  geom_text(color = "white", size = 3) +
  labs(
    title = "Comparison complex aggregation vs direct aggregation",
    y = "Deviance",
    x = "Traits"
  ) +
  ylim(-1.5, 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  facet_wrap(~family) +
  theme_light(base_size = 15) + #,base_family = "Poppins"
  theme(
    legend.position = "none", 
    axis.title = element_text(size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 10),
    axis.text.y = element_text(family = "Roboto Mono", size = 10)
  )

results_aus[deviance <= 0.1 & deviance > 0, ]

# save
ggsave(
  filename = "Trait_agg.png", plot = last_plot(),
  path = file.path(data_out),
  dpi = 400
)

# summary plot: 
rbindlist(results_agg, idcol = "file") %>%
  .[deviance != 0,] %>%
  ggplot(., aes(as.factor(variable), y = deviance,
                label = deviance)) +
  geom_point(stat = "identity", aes(col = family), size = 12) +
  geom_segment(aes(
    y = 0,
    x = variable,
    yend = deviance,
    xend = variable,
    col = family
  )) +
  geom_text(color = "white", size = 3)+
  coord_flip()+
  facet_wrap(~file)+
  theme(legend.position = "none")

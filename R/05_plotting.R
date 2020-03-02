# ___________________________________________________
#### Plotting ####
# ___________________________________________________

# freq of deviating assessments per order & grouping feature
freq_diff <- traitval_noa_aqi[, .(
  n_complete = .N, family, variable,
  order, grouping_feature, deviance_comp_fam
), by = c("order", "grouping_feature")] %>%
  .[deviance_comp_fam != 0, .(
    n_deviating = .N,
    n_complete, family, variable, order,
    deviance_comp_fam
  ), by = c("order","grouping_feature")] %>% 
  .[, .(freq = (n_deviating/n_complete)*100, 
        order, 
        grouping_feature)] 

#
freq_trait <- freq_diff[grouping_feature %in% "size", ] %>%
  .[!duplicated(order), ] 
freq_trait$order <- with(freq_trait, reorder(order, freq))

p_freq <- ggplot(freq_trait) +
  geom_bar(
    aes(x = as.factor(order), y = freq),
    stat = 'identity',
    width = 0.35,
    fill = "steelblue"
  ) +
  coord_flip() +
  labs(#title = "Frequency of differntly classified cases for the grouping feature feeding mode",
    #x = "Order",
    y = "Frequency in [%]") +
  theme_light(base_size = 15, base_family = "Poppins") +
  theme(
    legend.position = "none",
    title = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_blank(),
    #axis.text.x = element_text(family = "Roboto Mono", size = 11),
    axis.text.y = element_text(size = 11)#,
    #    panel.grid = element_blank()
  )


set.seed(123)
# change levels for column order according to freq plot
traitval_noa_aqi[, order := as.factor(order)]
traitval_noa_aqi <- traitval_noa_aqi %>%
  mutate(
    order = fct_relevel(
      order,
      "Megaloptera",
      "Odonata",
      "Hemiptera",
      "Diptera",
      "Ephemeroptera",
      "Trichoptera",
      "Coleoptera",
      "Plecoptera"
    )
  ) 
setDT(traitval_noa_aqi)

# freq plot
p_sum <- ggplot(traitval_noa_aqi[grouping_feature %in% "size",],
                     aes(x = order, y = deviance_comp_fam)) +
  geom_jitter(size = 4,
              alpha = 0.15,
              height = 0.05,
              width = 0.2,
              aes(col = order)) +
  geom_point(size = 6, alpha = 0.9, aes(col = order)) +
  geom_hline(aes(yintercept = 0), color = "black", size = 0.6) +
  scale_y_continuous(limits = c(-1.1, 1.1), expand = c(0.005, 0.005)) +
  scale_color_brewer(palette = "Set1")+
  coord_flip() +
  labs(x = "Order",
       y = "Deviance complex aggregation \n and assignments on family level")+
  facet_wrap( ~ variable, nrow = 2) +
  theme_light(base_size = 15, base_family = "Poppins") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 13),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11)
    #    panel.grid = element_blank()
  )
ggdraw(p_sum) + draw_plot(
  p_freq,
  x = 0.47,
  y = -0.25,
  width = 0.65,
  height = 1,
  scale = 0.5
)


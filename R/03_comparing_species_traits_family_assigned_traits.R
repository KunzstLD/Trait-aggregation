# _____________________________________________________________________________
# Here two trait aggregation methods are compared
# to traits assigned on family level for the Australian trait databbase.
# Reference traits on family level are from Chessman
# TODO Source for Chessman traits 
# ?For later, include respiration information:
# functional spiracles -> resp_spi
# air respiration   -> resp_spi
# gills -> resp_gil
# TODO check if there are rows with just zeros
# _____________________________________________________________________________

#### Data processing ####
chessman_raw <- read_excel(
  path = file.path(
    ".",
    "Data",
    "Chessman part 2.xlsx"
  ),
  skip = 1
)
setDT(chessman_raw)

# subset to feeding information, taxonomical information & size
chessman_raw <- chessman_raw[, .SD,
  .SDcols = names(chessman_raw) %like% "Order|Family|.*feeding|.*length"
]

# Classify continuous varibale length as size
chessman_raw[, `:=`(
  size_small = ifelse(`Maximum length (mm)` < 9,
    1, 0
  ),
  size_medium = ifelse(
    `Maximum length (mm)` >= 9 &
      `Maximum length (mm)` <= 16,
    1,
    0
  ),
  size_large = ifelse(`Maximum length (mm)` > 16,
    1, 0
  )
)]

# change col names so that data can be merged
# with results from other aggregations
setnames(chessman_raw,
  old = c(
    "Order",
    "Family",
    "Shredder (proportion of feeding)",
    "Scraper (proportion of feeding)",
    "Predator (proportion of feeding)",
    "Gatherer (proportion of feeding)",
    "Filterer (proportion of feeding)"
  ),
  new = c(
    "order",
    "family",
    "feed_shredder",
    "feed_herbivore",
    "feed_predator",
    "feed_gatherer",
    "feed_filter"
  )
)

# del maximum length variable
chessman_raw[, `Maximum length (mm)` := NULL]

# rm missing entries
chessman_raw <- na.omit(chessman_raw)

# transform to lf
chessman_raw <- melt(chessman_raw, id.vars = c("family", "order"))

# correct taxnomoy: "Veneroida" to "Venerida"
chessman_raw[order %in% "Veneroida", order := "Venerida"]

# combine with results from other aggregation methods
# see Script 02_comparing_agg_methods
traitval_aus <- merge(
  results_agg[["Trait_AUS_harmonized"]],
  chessman_raw,
  by = c("family", "variable", "order")
)
setnames(traitval_aus,
  old = c(
    "deviance",
    "value"
  ),
  new = c(
    "deviance_complex_direct_agg",
    "value_famlvl"
  )
)

# create grouping feature column
traitval_aus[, grouping_feature := sub("(.+)(\\_)(.+)", "\\1", variable)]

# create subset restricted to certain orders
# No Dipterans in data 
traitval_aus_aqi <- traitval_aus[order %in% c(
  "Ephemeroptera",
  "Hemiptera",
  "Odonata",
  "Trichoptera",
  "Coleoptera",
  "Plecoptera",
  "Diptera",
  "Lepidoptera",
  "Megaloptera",
  "Neuroptera"
  ), ]

# _____________________________________________________________________________
#### Analysis
# _____________________________________________________________________________

traitval_aus[, `:=`(
  deviance_dir_fam = value_direct_agg - value_famlvl,
  deviance_comp_fam = value_genus_fam_agg - value_famlvl
)]

# How many cases overall have been evaluated differently by Chessman?
# 41.1 % for direct aggregation method
# 39.2 % for complex aggr
nrow(traitval_aus[deviance_dir_fam != 0, ]) / nrow(traitval_aus)
nrow(traitval_aus[deviance_comp_fam != 0, ]) / nrow(traitval_aus)

# Which traits mostly differently classified?
traitval_aus[deviance_comp_fam != 0, .(.N), by = c("variable")] %>% 
  .[order(-N),]
traitval_aus[deviance_comp_fam != 0 & variable %in% "feed_herbivore", ] %>%
  .[, .(.N), by = order] %>% 
  .[order(-N),]

# Regarding deviating classification (trait_val compl_agg > fam_assignment)
# and vice versa: no tendency, almost equal. Also regarding orders
traitval_aus[deviance_comp_fam > 0, .N, by = "order"] %>%
  .[order(-N), ]
traitval_aus[deviance_comp_fam < 0, .N, by = "order"] %>%
  .[order(-N), ]

# Which orders?
# separate for grouping features
traitval_aus[deviance_comp_fam != 0, .(.N), by = c("order", 
                                                   "grouping_feature"), ] %>%
  .[order(grouping_feature, -N), ]


# How many taxa/cases are classified differntly?
# Overall, about 50 % of Diptera differently classified regarding
nrow(traitval_aus[deviance_comp_fam != 0 &
  order %in% "Diptera", ]) / nrow(traitval_aus[order %in% "Diptera", ])
# 42 % of Trichoptera differently classified
nrow(traitval_aus[deviance_comp_fam != 0 &
  order %in% "Trichoptera", ]) / nrow(traitval_aus[order %in% "Trichoptera", ])
# 29 % of Coleoptera differntly classified
nrow(traitval_aus[deviance_comp_fam != 0 &
  order %in% "Coleoptera", ]) / nrow(traitval_aus[order %in% "Coleoptera", ])

# freq of deviating assessments
freq_diff <- traitval_aus_aqi[, .(
  n_complete = .N, family, variable,
  order, grouping_feature, deviance_comp_fam
), by = "order"] %>%
  .[deviance_comp_fam != 0, .(
    n_deviating = .N,
    n_complete, family, variable, order,
    deviance_comp_fam
  ), by = c("order","grouping_feature")] %>% 
  .[, .(freq = (n_deviating/n_complete)*100, 
        order, 
        grouping_feature)] 

# re both traits: feeding mode 
# Odonata trait assignments do not deviate from trait aggregation values
p_freq_feed <- freq_diff[grouping_feature %in% "feed", ] %>%
  .[!duplicated(order),] %>% 
  ggplot(.) +
  geom_bar(aes(x = as.factor(order), y = freq), stat = 'identity',
           width = 0.35, fill = "steelblue")+
  coord_flip()+
  labs(#title = "Frequency of differntly classified cases for the grouping feature feeding mode",
       #x = "Order",
       y = "Frequency in [%]")+
  theme_light(base_size = 15, base_family = "Poppins") +
  theme(
    legend.position = "none",
    title = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_blank(),
    #axis.text.x = element_text(family = "Roboto Mono", size = 11),
    axis.text.y = element_text(family = "Roboto Mono", size = 11)#,
#    panel.grid = element_blank()
  )
# size:
p_freq_size <- freq_diff[grouping_feature %in% "size", ] %>%
  .[!duplicated(order),] %>% 
  ggplot(.) +
  geom_bar(aes(x = as.factor(order), y = freq), stat = 'identity',
           width = 0.35, fill = "steelblue")+
  coord_flip()+
  labs(#title = "Frequency of differntly classified cases for the grouping feature feeding mode",
       #x = "Order",
       y = "Frequency in [%]")+
  theme_light(base_size = 15, base_family = "Poppins") +
  theme(
    legend.position = "none",
    title = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_blank(),
#    axis.text.x = element_text(family = "Roboto Mono", size = 11),
    axis.text.y = element_text(family = "Roboto Mono", size = 11)#,
#    panel.grid = element_blank()
  )

# Is it possible to have a meaningul summary plot? How would it look like?
# Deviation of aggr trait values compared to family assignments per order and trait
set.seed(123)

# feeding mode:
p_sum_feed <- ggplot(traitval_aus_aqi[grouping_feature %in% "feed",],
       aes(x = as.factor(order), y = deviance_comp_fam)) +
  geom_jitter(size = 6,
              alpha = 0.35,
              width = 0.15,
              aes(col = order)) +
  geom_hline(aes(yintercept = 0), color = "gray70", size = 0.6) +
  scale_y_continuous(limits = c(-1.1, 1.1), expand = c(0.005, 0.005)) +
  scale_color_d3()+
  coord_flip() +
  labs(x = "Order",
       y = "Deviance complex aggregation \n and assignments on family level")+
  facet_wrap( ~ variable) +
  theme_light(base_size = 15, base_family = "Poppins") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 13),
    axis.text.x = element_text(family = "Roboto Mono", size = 11),
    axis.text.y = element_text(family = "Roboto Mono", size = 11)
    #    panel.grid = element_blank()
  )
ggdraw(p_sum_feed) + draw_plot(p_freq_feed, x = 0.58, y = -0.25, width = 0.52, height = 1,
                            scale = 0.5)

# size:
p_sum_size <- ggplot(traitval_aus_aqi[grouping_feature %in% "size",],
       aes(x = as.factor(order), y = deviance_comp_fam)) +
  geom_jitter(size = 6,
              alpha = 0.35,
              width = 0.15,
              aes(col = order)) +
  geom_hline(aes(yintercept = 0), color = "gray70", size = 0.6)+
  scale_y_continuous(limits = c(-1.1, 1.1), expand = c(0.005, 0.005)) +
  coord_flip() +
  labs(x = "Order",
       y = "Deviance complex aggregation \n and assignments on family level")+
  facet_wrap( ~ variable, nrow = 2) +
  theme_light(base_size = 15, base_family = "Poppins") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 13),
    axis.text.x = element_text(family = "Roboto Mono", size = 11),
    axis.text.y = element_text(family = "Roboto Mono", size = 11)
    #    panel.grid = element_blank()
  )
ggdraw(p_sum_size) + draw_plot(p_freq_size, x = 0.38, y = -0.25, width = 0.8, height = 1,
                               scale = 0.5)
# TODO:Fix x-axis title 

# plot only families where there is an extreme deviance?
test <- traitval_aus_aqi %>%
.[deviance_comp_fam < -0.5 | deviance_comp_fam > 0.5, ]

melt(test,
    id.vars = c("family", "order", "variable", "N", "grouping_feature"),
    variable.name = "value_type"
  ) %>%
.[value_type %in% c("value_genus_fam_agg", "value_famlvl"), ] %>%
 ggplot(
    data = .,
    aes(x = as.factor(variable), y = value)
  ) +
  geom_point(aes(col = value_type),size = 4)+
  coord_flip()+
  facet_wrap(~order)+
  theme_light(base_size = 15, base_family = "Poppins") +
  theme(
    #legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 10)#,
#    panel.grid = element_blank()
  )
# _____________________________________________________________________________
# Here two trait aggregation methods are compared
# to traits assigned on family level for the North American trait databbase
# TODO check if there are rows with just zeros
# North American trait data from Matt Pyne (?)
# TODO: Source
# _____________________________________________________________________________

#### Data preprocessing ####
noa_trait_matrix <-
  read_excel(
    path = file.path(".", "Data", "American Trait Matrix_upgraded(2014).xlsx"),
    sheet = 2
  ) %>%
  setDT(.) %>%
  .[, -("...1")] %>%
  .[, .(Order, Family, Volt, Size, Habt, Trop, Resp)]

# check traits per grouping feature
# describe(noa_trait_matrix)

# rm NAs
noa_trait_matrix <- na.omit(noa_trait_matrix)

# convert categories to presence-absence of trait
noa_trait_matrix_lf <-
  melt(noa_trait_matrix, id.vars = c("Family", "Order"))
noa_trait_matrix <- dcast(
  noa_trait_matrix_lf,
  Order + Family ~ value,
  value.var = "value",
  fun.aggregate = length
)

# change col names
setnames(
  noa_trait_matrix,
  old = c(
    "Habt.Burrow",
    "Habt.Climb",
    "Habt.Cling",
    "Habt.Skate",
    "Habt.Sprawl",
    "Habt.Swim",
    "Resp.Air",
    "Resp.Gills",
    "Resp.Tegument",
    "Size.Large",
    "Size.Medium",
    "Size.Small",
    "Trop.CollectorFilterer",
    "Trop.CollectorGatherer",
    "Trop.Herbivore",
    "Trop.Predator",
    "Trop.Shredder",
    "Volt.MultiVoltine",
    "Volt.SemiVoltine",
    "Volt.UniVoltine",
    "Family",
    "Order"
  ),
  new = c(
    "locom_burrow",
    "locom_climb",
    "locom_cling",
    "locom_skate",
    "locom_sprawl",
    "locom_swim",
    "resp_pls_spi",
    "resp_gil",
    "resp_teg",
    "size_large",
    "size_medium",
    "size_small",
    "feed_filter",
    "feed_gatherer",
    "feed_herbivore",
    "feed_predator",
    "feed_shredder",
    "volt_bi_multi",
    "volt_semi",
    "volt_uni",
    "family",
    "order"
  )
)

# harmonize locomotion so that this grouping features matches with the harmonized trait set
# sessil does not exist as category
noa_trait_matrix[, locom_swim := apply(.SD, 1, max),
                 .SDcols = c("locom_swim", "locom_skate")]
noa_trait_matrix[, locom_crawl := apply(.SD, 1, max),
                 .SDcols = c("locom_sprawl", "locom_climb", "locom_cling")]
# rm unnecessary cols
noa_trait_matrix[, c("locom_skate", "locom_sprawl", "locom_climb", "locom_cling") := NULL]

# convert back to lf
# NOA data just contain aquatic insects
noa_trait_matrix <-
  melt(noa_trait_matrix, id.vars = c("order", "family"))

#### Check:
# distinctive families
noa_trait_matrix$family %>% Hmisc::describe()

# Overlap between noa_trait_matrix and aggregated results
unique(noa_trait_matrix$family) %in% unique(results_agg[["Traits_US_LauraT_pp_harmonized"]]$family) %>%
  sum()

#### Merge with values from trait aggregation
traitval_noa <-
  merge(results_agg[["Traits_US_LauraT_pp_harmonized"]],
        noa_trait_matrix,
        by = c("family", "variable", "order"))

setnames(
  traitval_noa,
  old = c("deviance",
          "value"),
  new = c("deviance_complex_direct_agg",
          "value_famlvl")
)

# create grouping feature column
traitval_noa[, grouping_feature := sub("(\\w)(\\_)(.+)", "\\1", variable)]

# _______________________________________________________________________
#### Analysis ####
# _______________________________________________________________________

# calc deviances to family assignments
traitval_noa[, `:=`(
  deviance_dir_fam = value_direct_agg - value_famlvl,
  deviance_comp_fam = value_genus_fam_agg - value_famlvl
)]

#### How many cases overall have been evaluated differently by Pyne?
# 22 % and 21 % respectively
traitval_noa[deviance_dir_fam != 0, .N] / traitval_noa[, .N]
traitval_noa[deviance_comp_fam != 0, .N] / traitval_noa[, .N]

# Which traits?
total <- traitval_noa[, .N, by = "variable"] %>% .[, N]
traitval_noa[deviance_dir_fam != 0, .(.N),
             by = c("variable")] %>%
  .[order(-N), N / total]

# Latex output:
# direct_fam
traitval_noa[deviance_dir_fam != 0, .(.N),
             by = c("variable")] %>%
  .[order(-N), .(
    Trait = variable,
    `Families differently evaluated [%]` =
      round(N / total * 100, digits = 2)
  )] %>%
  xtable_wo_rownames(
    .,
    caption = "Percentage of families differently evaluated by
    direct aggreation and Pyne et al.
    for all traits were deviating evaluations
    occurred.",
    label = "tab:SI_perc_dir_agg_expert_NOA"
  )

# comp_fam
traitval_noa[deviance_comp_fam != 0, .(.N),
             by = c("variable")] %>%
  .[order(-N), .(
    Trait = variable,
    `Families differently evaluated [%]` =
      round(N / total * 100, digits = 2)
  )] %>%
  xtable_wo_rownames(
    .,
    caption = "Percentage of families differently evaluated by
    stepwise aggreation and Pyne et al. for all traits were deviating evaluations
    occurred.",
    label = "tab:SI_perc_stepwise_agg_expert_NOA"
  )

#### Which orders?
# Ephmeroptera, Diptera
total <- traitval_noa[, .N, by = c("order")] %>% .[order(-N),]

# latex output
traitval_noa[deviance_dir_fam != 0, .(variable, .N), by = "order"] %>%
  merge(x = . , y = total, by = "order") %>%
  .[, .(
    Order = order,
    `Families differently evaluated direct_agg [%]` = round((N.x / N.y) * 100, digits = 2)
  )] %>%
  .[!duplicated(Order), ] %>%
  .[order(-`Families differently evaluated direct_agg [%]`), ] %>%
  xtable_wo_rownames(.,
                     caption = "Percentage of families differently evaluated
                     by  and Pyne et al. for all compared orders",
                     label = "tab:SI_perc_dir_agg_expert_family_NOA")

# stepwise agg latex output
traitval_noa[deviance_comp_fam != 0, .(variable, .N), by = "order"] %>%
  merge(x = . , y = total, by = "order") %>%
  .[, .(
    Order = order,
    `Families differently evaluated stepwise_agg [%]` = round((N.x /
                                                                 N.y) * 100, digits = 2)
  )] %>%
  .[!duplicated(Order),] %>%
  .[order(-`Families differently evaluated stepwise_agg [%]`),] %>%
  xtable_wo_rownames(.,
                     caption = "Percentage of families differently evaluated
                     by  and Pyne et al. for all compared orders",
                     label = "tab:SI_perc_stepwise_agg_expert_family_NOA")

# SD in deviance dir_fam
traitval_noa[, sd_dir_fam := sd(deviance_dir_fam), by = c("order", "variable")]
traitval_noa[order(-sd_dir_fam), .(family,
                                   order,
                                   variable,
                                   sd_dir_fam,
                                   value_direct_agg,
                                   value_famlvl)] %>% View()

# Regarding deviating classification (trait_val compl_agg > fam_assignment)
# and vice versa: no tendency, almost equal
traitval_noa[deviance_comp_fam > 0, .(.N, variable), by = "order"] %>%
  .[order(-N),]
traitval_noa[deviance_comp_fam < 0, .N, by = "order"] %>%
  .[order(-N),]

#### How big are the deviances actually?
# direct aggreation with at family-level assigned traits
total <- traitval_noa[deviance_dir_fam != 0, .N]

# dev 0.25
traitval_noa[deviance_dir_fam != 0, .(family,
                                      order,
                                      variable,
                                      value_direct_agg,
                                      value_famlvl,
                                      deviance_dir_fam)] %>%
  .[abs(deviance_dir_fam) <= 0.25,]


# dev. 0.5
traitval_noa[deviance_dir_fam != 0, .(family,
                                      order,
                                      variable,
                                      value_direct_agg,
                                      value_famlvl,
                                      deviance_dir_fam)] %>%
  .[abs(deviance_dir_fam) %between% c(0.26, 0.5), .N / total]
traitval_noa[deviance_dir_fam != 0, .(family,
                                      order,
                                      variable,
                                      value_direct_agg,
                                      value_famlvl,
                                      deviance_dir_fam)] %>%
  .[abs(deviance_dir_fam) %between% c(0.26, 0.5),]


# dev 0.75
traitval_noa[deviance_dir_fam != 0, .(family,
                                      order,
                                      variable,
                                      value_direct_agg,
                                      value_famlvl,
                                      deviance_dir_fam)] %>%
  .[abs(deviance_dir_fam) == 0.75,]

# dev 1
traitval_noa[deviance_dir_fam != 0, .(
  family,
  order,
  variable,
  grouping_feature,
  value_direct_agg,
  value_famlvl,
  deviance_dir_fam
)] %>%
  .[abs(deviance_dir_fam) == 1,] %>%
  Hmisc::describe()

# stepwise aggregation with at family-level assigned traits
total_comp_agg <- traitval_noa[deviance_comp_fam != 0, .N]

traitval_noa[deviance_comp_fam != 0, .(
  family,
  order,
  variable,
  grouping_feature,
  value_direct_agg,
  value_famlvl,
  deviance_comp_fam
)] %>%
  .[abs(deviance_comp_fam) == 1,] %>%
  Hmisc::describe()

# How many taxa/cases are classified differntly per order?
nrow(traitval_noa[deviance_comp_fam != 0 &
                    order %in% "Ephemeroptera",]) / nrow(traitval_noa[order %in% "Ephemeroptera",])
nrow(traitval_noa[deviance_comp_fam != 0 &
                    order %in% "Diptera",]) / nrow(traitval_noa[order %in% "Diptera",])
nrow(traitval_noa[deviance_comp_fam != 0 &
                    order %in% "Trichoptera",]) / nrow(traitval_noa[order %in% "Trichoptera",])
nrow(traitval_noa[deviance_comp_fam != 0 &
                    order %in% "Coleoptera",]) / nrow(traitval_noa[order %in% "Coleoptera",])

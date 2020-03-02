# _________________________________________________________________________
#### basic analysis of trait datasets ####
# Information on taxonomical coverage
# Completeness of trait data 
# _________________________________________________________________________

# NOA_fc not used
trait_dat_subs <- trait_dat[c(
  "Trait_AUS_harmonized.rds",
  "Trait_EU_pp_harmonized.rds",
  "Trait_NZ_pp_harmonized.rds",
  "Traits_US_LauraT_pp_harmonized.rds"
)]

# _________________________________________________________________________
# completeness of trait data
# _________________________________________________________________________
completeness <- trait_dat_subs %>%
  lapply(., function(y) {
    y[, .SD,
      .SDcols = names(y) %like% "species|genus|family|order|feed.+|locom.+|size.+|volt.+|resp.+|bf.+"
      ] %>% 
      normalize_by_rowSum(.,
                          non_trait_cols = c(
                            "species",
                            "genus",
                            "family",
                            "order"
                          )
      ) %>% 
      completeness_trait_data(x = ., 
                              non_trait_cols = c("species", 
                                                 "genus",
                                                 "family",
                                                 "order"))}
  )
completeness <- lapply(completeness, as.data.table) %>%
  rbindlist(., idcol = "file") 

# change colnames
setnames(completeness,
         old = c("file",
                 "V1",
                 "V2"),
         new = c("Database",
                 "Trait covered [%]",
                 "Grouping feature"))

# create look up tables 
look_up_db <- data.table(
  Key_col = c(
    "Trait_AUS_harmonized.rds",
    "Trait_EU_pp_harmonized.rds",
    "Trait_NZ_pp_harmonized.rds",
    "Traits_US_LauraT_pp_harmonized.rds"
  ),
  Database = c("Australia",
               "Europe",
               "New Zealand",
               "North America")
)
look_up_grf <- data.table(
  Key_col = c("^bf",
              "^locom",
              "^size",
              "^volt",
              "^feed",
              "^resp"),
  Grouping_feature = c(
    "Body form",
    "Locomotion",
    "Size",
    "Voltinism",
    "Feeding mode",
    "Respiration"
  )
)

# merge
completeness[look_up_db,
     `:=`(Database = i.Database), 
     on = c(Database = "Key_col")]

completeness[look_up_grf,
     `:=`(`Grouping feature` = i.Grouping_feature),
     on = c(`Grouping feature` = "Key_col")]

# order by % trait covered
completeness[, `Trait covered [%]` := as.numeric(`Trait covered [%]`)]
completeness <- completeness[order(Database, `Grouping feature`), ]

# change col order 
setcolorder(completeness,
            neworder = c("Database", "Grouping feature", "Trait covered [%]"))

# latex output
xtable_wo_rownames(x = completeness,
                   caption = "Table shows how well the individual
                   grouping features are covered in the trait databases",
                   label = "tab:trait_coverage")

# _________________________________________________________________________
#### Taxonomical coverage ####
# _________________________________________________________________________

# distinctive entries
lapply(trait_dat_subs, nrow)

# how many entries per taxonomical level?
lapply(trait_dat_subs, function(y) Hmisc::describe(y))

# how many distinct orders?
lapply(trait_dat_subs, function(y) y[!is.na(order), length(unique(order))])

# how many distinct families?
lapply(trait_dat_subs, function(y) y[!is.na(family), length(unique(family))])

# how many taxa on species level?
lapply(trait_dat_subs, function(y) y[!is.na(species), .N])

# how many taxa solely on genus-level?
lapply(trait_dat_subs, function(y) y[is.na(species) & !is.na(genus), 
                                length(unique(genus))])

# how many taxa solely on family-level?
lapply(trait_dat_subs, function(y) y[is.na(species) & is.na(genus) & !is.na(family), 
                                length(unique(family))])

#________________________________________________________________________
#### Ratio initial nr of families and after selecting only compl TP ####
# How many families end up in the final
# preprocessed trait datasets compared to the initial
# trait datasets?
# This gives us insight how much and how consistent
# information is covered in these trait databases
# Notice: trait information gets not lost during aggregation
# TODO: identify difference between NOA_fc & NOA_bin
# -> Why Neuroptera in NOA_fc but not in NOA_bin?

#________________________________________________________________________

# families per order initial data
cov_init <- lapply(trait_dat, function(y) {
  y[!duplicated(family), .(family, .N),
    by = order]
})

# family per order preproc data
cov_preproc <- lapply(preproc_dat, function(y) {
  y[!duplicated(family), .(family, .N),by = order]
})

# Nr of orders per DB init
lapply(cov_init, function(y){
  y[!is.na(order), .N, by = "order"] %>% nrow()
})

# Nr of orders per DB preproc
lapply(cov_preproc, function(y){
  y[!is.na(order), .N, by = "order"] %>% nrow()
})

# How much % of aquatic insects per Database?
aq_ins <- c(
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
)

lapply(cov_init, function(y) {
  y[!is.na(order), .N, by = "order"] %>%
    .[order(N), .(total_sum = sum(N), N, order)] %>%
    .[order %in% aq_ins, .(order, 
                           aq_ins = round(sum(N) / total_sum, digits = 2))]
})


# orders not included
# NZ:
order_vec <- unique(cov_preproc$Trait_NZ_pp_harmonized$order)
cov_init$Trait_NZ_pp_harmonized.rds[!order %in% order_vec, ]

# AUS:
order_vec <- unique(cov_preproc$Trait_AUS_harmonized$order)
cov_init$Trait_AUS_harmonized.rds[!order %in% order_vec, unique(order)]



# families not covered in preproc. dataset?
# mapply(function(x,y) x[!family %in% y$family, ],
#        cov_init,
#        cov_preproc,
#        SIMPLIFY = FALSE)

# bind init & preproc together 
cov_res <-
  mapply(function(x, y)
    merge(x, y, by = c("family", "order")),
    cov_init,
    cov_preproc,
    SIMPLIFY = FALSE)

#  change col names
cov_res <- lapply(cov_res, function(y)
  setnames(
    y,
    old = c("N.x", "N.y"),
    new = c("N_init", "N_preproc")
  ))

# calculate % preproc/initial
cov_res <- lapply(cov_res, function(y) {
  y[, coverage := ((N_preproc / N_init) * 100)]
})
# lapply(cov_res, function(y){
#   y[order(order), ]
# })

output_tbl <- lapply(cov_res, function(y)
  y[!duplicated(order), ]) %>%
  rbindlist(., idcol = "file") %>%
  .[!is.na(order) , .(order, coverage = round(coverage, digits = 2), file)] %>%
  dcast(., order ~ file, value.var = "coverage")  

setnames(
  output_tbl,
  old = c(
    "order",
    "Trait_AUS_harmonized.rds",
    "Trait_EU_pp_harmonized.rds",
    "Trait_NZ_pp_harmonized.rds",
    "Traits_US_LauraT_pp_harmonized.rds",
    "Traits_US_LauraT_pp_harmonized_fc.rds"
  ),
  new = c("Order",
          "Australia",
          "Europe",
          "New Zealand", 
          "North America",
          "North America fuzzy coded")
)

# latex output
print(
  xtable(
    output_tbl,
    caption = "Proportion of families per order that remain
    after only complete trait profiles have been selected from the total
    number of all families in the databases.",
    auto = TRUE
  ),
  include.rownames = FALSE
)

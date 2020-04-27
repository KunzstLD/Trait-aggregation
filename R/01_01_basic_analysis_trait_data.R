# _________________________________________________________________________
#### basic analysis of trait datasets ####
# Completeness of trait data 
# Information on taxonomical coverage
# _________________________________________________________________________

# _________________________________________________________________________
# completeness of trait data
# _________________________________________________________________________
completeness <- trait_dat %>%
  lapply(., function(y) {
    y[, .SD,
      .SDcols = names(y) %like% "species|genus|family|order|feed.+|locom.+|size.+|volt.+|resp.+|bf.+|ovip.+"
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
              "^resp", 
              "^ovip"),
  Grouping_feature = c(
    "Body form",
    "Locomotion",
    "Size",
    "Voltinism",
    "Feeding mode",
    "Respiration",
    "Oviposition"
  )
)

# merge
completeness[look_up_db,
     `:=`(Database = i.Database), 
     on = c(Database = "Key_col")]
completeness[look_up_grf,
     `:=`(`Grouping feature` = i.Grouping_feature),
     on = c(`Grouping feature` = "Key_col")]

# convert Trait covered to numeric
completeness[, `Trait covered [%]` := as.numeric(`Trait covered [%]`)]

# which grf and which db has highest "cumulative" completeness in % 
completeness[, base::sum(`Trait covered [%]`), by = "Grouping feature"] %>%
  .[order(V1), `Grouping feature`]
target <-
  completeness[, base::sum(`Trait covered [%]`), by = "Database"] %>%
  .[order(V1), Database]

# wide format
completeness_output <- dcast(data = completeness,
                      Database ~  `Grouping feature`,
                      value.var = "Trait covered [%]") %>%
  .[match(target, Database),]


# change col order 
setcolorder(
  completeness_output,
  neworder = c(
    "Database",
    "Body form",
    "Oviposition",
    "Voltinism",
    "Locomotion",
    "Size",
    "Respiration",
    "Feeding mode"
  )
)

# latex output
xtable_wo_rownames(x = completeness_output,
                   caption = "Table shows [%] of entries that have 
                   information for the individual grouping features
                   per trait databases",
                   label = "tab:trait_coverage",
                   digits = 0)


# _________________________________________________________________________
#### Taxonomic coverage ####
# removed taxa on a higher level
# _________________________________________________________________________

# overview
# lapply(trait_dat, function(y) Hmisc::describe(y))

# What we need: 
# - taxa pool 
TaxaPool <- lapply(trait_dat, function(y)
  y[, .N]) %>% 
  lapply(., as.data.table) %>%
  rbindlist(.,
            idcol = "id")

# How many resolved on species, genus and family-level?
TaxLvl <- lapply(trait_dat, taxonomic_lvl) %>% 
  rbindlist(., idcol = "id")

# % of aquatic insects
# How much % of aquatic insects per Database?
orders_aq_ins <- c(
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

AqIns <- lapply(trait_dat, function(y)
  y[order %in% orders_aq_ins, .N]) %>% 
  lapply(. , as.data.table) %>% 
  rbindlist(.,
            idcol = "id")
setnames(AqIns, "V1", "nr_aq_taxa")

# combine 
TaxCoverage <- Reduce(merge, list(TaxaPool, 
                   TaxLvl,
                   AqIns))

# calculate relative frequencies
cols <- c("species", "genus", "family", "nr_aq_taxa")
TaxCoverage[, c("species_rf",
                "genus_rf",
                "family_rf",
                "aq_taxa_rf") := lapply(.SD, function(y)
                  round(y / V1 * 100, digits = 2)),
            .SDcols = cols]

# create char vectors by pasting together results
TaxCoverage[, `:=`(
  species = paste0(species, " (", species_rf, ")"),
  genus = paste0(genus, " (", genus_rf, ")"),
  family = paste0(family, " (", family_rf, ")"),
  nr_aq_taxa = paste0(nr_aq_taxa, " (", aq_taxa_rf, ")")
)]

# change colnames
setnames(TaxCoverage,
         old = c("id", 
                 "V1", 
                 "species", 
                 "genus",
                 "family",
                 "nr_aq_taxa"),
         new = c("Database", 
                 "Nr. of taxa",
                 "Species",
                 "Genus",
                 "Family",
                 "Nr. aquatic taxa"))

# change database names
TaxCoverage[look_up_db,
            `:=`(Database = i.Database),
            on = c(Database = "Key_col")]

# change row order
TaxCoverage <- TaxCoverage[match(c("Europe",
                                   "North America",
                                   "Australia",
                                   "New Zealand"),
                                 Database),]

# rm not used columns
TaxCoverage[, c("species_rf", 
                "genus_rf",
                "family_rf",
                "aq_taxa_rf") := NULL]

# latex output 
xtable_wo_rownames(x = TaxCoverage,
                   caption = "",
                   label = "tab:tax_coverage",
                   digits = 2)

# TODO: Following code maybe not needed, think about it
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

# # families per order initial data
# cov_init <- lapply(trait_dat, function(y) {
#   y[!duplicated(family), .(family, .N),
#     by = order]
# })
# 
# # family per order preproc data
# cov_preproc <- lapply(preproc_dat, function(y) {
#   y[!duplicated(family), .(family, .N),by = order]
# })
# 
# # Nr of orders per DB init
# lapply(cov_init, function(y){
#   y[!is.na(order), .N, by = "order"] %>% nrow()
# })
# 
# # Nr of orders per DB preproc
# lapply(cov_preproc, function(y){
#   y[!is.na(order), .N, by = "order"] %>% nrow()
# })
# 
# # orders not included
# # NZ:
# order_vec <- unique(cov_preproc$Trait_NZ_pp_harmonized$order)
# cov_init$Trait_NZ_pp_harmonized.rds[!order %in% order_vec, ]
# 
# # AUS:
# order_vec <- unique(cov_preproc$Trait_AUS_harmonized$order)
# cov_init$Trait_AUS_harmonized.rds[!order %in% order_vec, unique(order)]
# 
# 
# # families not covered in preproc. dataset?
# # mapply(function(x,y) x[!family %in% y$family, ],
# #        cov_init,
# #        cov_preproc,
# #        SIMPLIFY = FALSE)
# 
# # bind init & preproc together 
# cov_res <-
#   mapply(function(x, y)
#     merge(x, y, by = c("family", "order")),
#     cov_init,
#     cov_preproc,
#     SIMPLIFY = FALSE)
# 
# #  change col names
# cov_res <- lapply(cov_res, function(y)
#   setnames(
#     y,
#     old = c("N.x", "N.y"),
#     new = c("N_init", "N_preproc")
#   ))
# 
# # calculate % preproc/initial
# cov_res <- lapply(cov_res, function(y) {
#   y[, coverage := ((N_preproc / N_init) * 100)]
# })
# # lapply(cov_res, function(y){
# #   y[order(order), ]
# # })
# 
# output_tbl <- lapply(cov_res, function(y)
#   y[!duplicated(order), ]) %>%
#   rbindlist(., idcol = "file") %>%
#   .[!is.na(order) , .(order, coverage = round(coverage, digits = 2), file)] %>%
#   dcast(., order ~ file, value.var = "coverage")  
# 
# setnames(
#   output_tbl,
#   old = c(
#     "order",
#     "Trait_AUS_harmonized.rds",
#     "Trait_EU_pp_harmonized.rds",
#     "Trait_NZ_pp_harmonized.rds",
#     "Traits_US_LauraT_pp_harmonized.rds",
#     "Traits_US_LauraT_pp_harmonized_fc.rds"
#   ),
#   new = c("Order",
#           "Australia",
#           "Europe",
#           "New Zealand", 
#           "North America",
#           "North America fuzzy coded")
# )
# 
# # latex output
# # for now, exclude NOA_fc 
# print(
#   xtable(
#     output_tbl[, .(Order, 
#                    Australia,
#                    `New Zealand`,
#                    Europe,
#                    `North America`)],
#     caption = "Proportion of families per order that remain
#     after only complete trait profiles have been selected from the total
#     number of all families in the databases.",
#     auto = TRUE
#   ),
#   include.rownames = FALSE
# )
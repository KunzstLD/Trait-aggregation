#_____________________________________________________________________
# How many families end up in the final
# preprocessed trait datasets compared to the initial
# trait datasets?
# This gives us insight how much and how consistent
# information is covered in these trait databases
# Notice: trait information gets not lost during aggregation
# TODO: identify difference between NOA_fc & NOA_bin
# -> Why Neuroptera in NOA_fc but not in NOA_bin?

#_____________________________________________________________________

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
  y[!is.na(order), .N, by = "order"] #%>% nrow()
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


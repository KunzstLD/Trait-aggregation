# ____________________________________________________________________________________________
# Fourth - Corner analysis
# TODO repeat this with "full data"?
# ____________________________________________________________________________________________

# Load "reduced" trait data (with original), abundance, and cond. data ----
# "reduced" dataset (i.e. highest abundances of multiple obs. per year):
agg_data_high <- readRDS(file = file.path(
  data_cache,
  "Re-analysis_cache",
  "agg_data_multiple_samp_corr.rds"
))
ecor_L_high <- readRDS(file = file.path(
  data_cache,
  "Re-analysis_cache",
  "ecor_L_multiple_samp_corr.rds"
))
ecor_R_high <- readRDS(file = file.path(
  data_cache,
  "Re-analysis_cache",
  "ecor_R_multiple_samp_corr.rds"
))

# Load "full" trait data (with original), abundance, and cond. data ----
agg_data <- readRDS(file = file.path(data_cache,
                                     "Re-analysis_cache",
                                     "agg_data.rds"))
ecor_L <- readRDS(file = file.path(data_cache,
                                   "Re-analysis_cache",
                                   "ecor_L_corr.rds"))
ecor_R <- readRDS(file = file.path(data_cache,
                                   "Re-analysis_cache",
                                   "ecor_R_corr.rds"))

# lookup table for trait data
trait_edi_lookup <-
  fread(file.path("./Data/Edi_salinity_study/data/trait_lookup.csv"),
        sep = ";")
trait_edi_lookup[, ID_trait := paste0("X", ID_trait)]

# ? keep year in the model (according to the paper)
ecor_R_high$year <- factor(ecor_R_high$year)


# Conductivity gradient ----

# "reduced" data
desired_order_high <- ecor_R_high[order(salinisati), sample_id]

ecor_R_high[, sample_id := as.character(sample_id)]
ecor_R_high[, sample_id := factor(sample_id, levels = desired_order_high)]

ggplot(ecor_R_high, aes(
  x = sample_id,
  y = cond,
  color = as.factor(salinisati)
)) +
  geom_point(size = 2) +
  labs(x = "Sampled ID (site year combination)",
       y = "Conductivity",
       color = "Site location") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16),
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = "Roboto Mono", size = 14),
    legend.text = element_text(family = "Roboto Mono", size = 14),
    legend.title = element_text(family = "Roboto Mono", size = 16)
  )
ggsave(
  file.path(data_paper, "Conductivity_gradient_reduced.png"),
  width = 25,
  height = 17,
  unit = "cm"
)

# "full" data
setDT(ecor_R)
desired_order <- ecor_R[order(salinisati), sample_id]

ecor_R[, sample_id := as.character(sample_id)]
ecor_R[, sample_id := factor(sample_id, levels = desired_order)]

ggplot(ecor_R, aes(
  x = sample_id,
  y = cond,
  color = as.factor(salinisati)
)) +
  geom_point(size = 2) +
  labs(x = "Sampled ID (site year combination)",
       y = "Conductivity",
       color = "Site location") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16),
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = "Roboto Mono", size = 14),
    legend.text = element_text(family = "Roboto Mono", size = 14),
    legend.title = element_text(family = "Roboto Mono", size = 16)
  )
ggsave(
  file.path(data_paper, "Conductivity_gradient_full.png"),
  width = 25,
  height = 17,
  unit = "cm"
)


# 4th corner with all traits ----

## "reduced" dataset ----
fc_salinity_rd <- lapply(agg_data_high, function(y)
  fourthcorner(
    ecor_R_high[, c("cond"), drop = FALSE],
    ecor_L_high,
    y,
    modeltype = 6,
    nrepet = 999
  ))
saveRDS(
  fc_salinity_rd,
  file.path(
    data_cache,
    "Re-analysis_cache",
    "fc_salinity_all_traits_reduced.rds"
  )
)

# check p values
lapply(fc_salinity_rd, function(y)
  which(y$tabD2$adj.pvalue <= 0.1))
fc_salinity_rd$not_aggregated$tabD2[15]


## "full" dataset ----
fc_salinity_full <- lapply(agg_data, function(y)
  fourthcorner(ecor_R[, c("cond"), drop = FALSE],
               ecor_L,
               y,
               modeltype = 6,
               nrepet = 999))
saveRDS(
  fc_salinity_full,
  file.path(
    data_cache,
    "Re-analysis_cache",
    "fc_salinity_all_traits_full.rds"
  )
)

lapply(fc_salinity_full, function(y)
  which(y$tabD2$adj.pvalue <= 0.1))


# 4th corner with pre-selected traits ----
# take those from Szöcs et al. 2014:
#   Potential nr. of life cycles: 1
#   Reproduction: clutches, cemented or fixed
#   Life cycle duration <= 1 year
#   Feeding habits shredder
#   Life cycle duration >= 1 year
#   Respiration gill
#   Potential nr. of cycles per year > 1
#   Reproduction ovoviviparity
selected_traits <-
  c(
    trait_edi_lookup[ID_name %like% c("^1$|clutches, cemented or fixed|1 year|shredder|gill|^> 1$|ovo.*"),
                     ID_trait],
    "volt_uni",
    "ovip_aqu",
    "feed_shredder",
    "resp_gil",
    "volt_bi_multi",
    "ovip_ovo"
  )

## "reduced" dataset ----
res <- list()
for (i in names(agg_data_high)) {
  trait_dt <- agg_data_high[[i]]
  cols <- intersect(names(trait_dt), selected_traits)
  
  res[[i]] <- fourthcorner(ecor_R_high[, c("cond"), drop = FALSE],
                           ecor_L_high,
                           trait_dt[, cols],
                           modeltype = 6,
                           nrepet = 999)
}
saveRDS(
  res,
  file.path(
    data_cache,
    "Re-analysis_cache",
    "fc_salinity_selected_traits_reduced.rds"
  )
)
res
trait_edi_lookup[ID_name %like% c("^1$|clutches, cemented or fixed|1 year|shredder|gill|^> 1$|ovo.*"), ]

## "full" dataset ----
res_full <- list()
for (i in names(agg_data)) {
  trait_dt <- agg_data[[i]]
  cols <- intersect(names(trait_dt), selected_traits)
  
  res_full[[i]] <- fourthcorner(ecor_R[, c("cond"), drop = FALSE],
                                ecor_L,
                                trait_dt[, cols],
                                modeltype = 6,
                                nrepet = 999)
}
saveRDS(
  res_full,
  file.path(
    data_cache,
    "Re-analysis_cache",
    "fc_salinity_selected_traits_full.rds"
  )
)

# Save 4th corner output as .html tables ----
file <- c(
  "fc_salinity_all_traits_reduced",
  "fc_salinity_all_traits_full",
  "fc_salinity_selected_traits_full",
  "fc_salinity_selected_traits_reduced"
)

sapply(file, function(x) {
  rmarkdown::render(
    input = file.path(
      path_src,
      "Reanalysis_salinization",
      "output_fth_corner.Rmd"
    ),
    output_file = sprintf("fc_%s.html", x),
    output_dir = "/home/kunzst/Dokumente/Projects/Trait_DB/Trait-aggregation/Output/",
    params = list(file = x)
  )
})

# Tables for latex ----

# Results for datasets with selected traits:
res_sel_full <-
  readRDS(
    file.path(
      data_cache,
      "Re-analysis_cache",
      "fc_salinity_selected_traits_full.rds"
    )
  )
res_sel_red <- readRDS(
  file.path(
    data_cache,
    "Re-analysis_cache",
    "fc_salinity_selected_traits_reduced.rds"
  )
)

# Extract fc results and get latex output
fc_tbl_rd <- extract_fc_results(x = res_sel_red)
fc_tbl_rd <- rbindlist(fc_tbl_rd, idcol = "Dataset")
fc_tbl_full <- extract_fc_results(x = res_sel_full)
fc_tbl_full <- rbindlist(fc_tbl_full, idcol = "Dataset")

# Get real trait names
search <- fc_tbl_rd[grep("X.*", Test), sub("cond \\/ ", "", Test)]
search <- unique(search)

for (i in seq_along(search)) {
  subs <-
    trait_edi_lookup[ID_trait == search[[i]], paste(ID_trait_name, ID_name)]
  fc_tbl_rd[, Test := sub(search[[i]], subs, Test)]
  fc_tbl_full[, Test := sub(search[[i]], subs, Test)]
}

fc_tbl_rd[,
          Pvalue.adj := fcase(
            Pvalue.adj <= 0.1 & Pvalue.adj > 0.05,
            paste(Pvalue.adj, "."),
            Pvalue.adj <= 0.05 & Pvalue.adj > 0.01,
            paste(Pvalue.adj, "*"),
            Pvalue.adj <= 0.01 & Pvalue.adj > 0.001,
            paste(Pvalue.adj, "**"),
            Pvalue.adj <= 0.001,
            paste(Pvalue.adj, "***"),
            Pvalue.adj > 0.1,
            as.character(Pvalue.adj)
          )]

fc_tbl_full[,
            Pvalue.adj := fcase(
              Pvalue.adj <= 0.1 & Pvalue.adj > 0.05,
              paste(Pvalue.adj, "."),
              Pvalue.adj <= 0.05 & Pvalue.adj > 0.01,
              paste(Pvalue.adj, "*"),
              Pvalue.adj <= 0.01 &
                Pvalue.adj > 0.001,
              paste(Pvalue.adj, "**"),
              Pvalue.adj <= 0.001,
              paste(Pvalue.adj, "***"),
              Pvalue.adj > 0.1,
              as.character(Pvalue.adj)
            )]

# Latex output
print(xtable::xtable(x = fc_tbl_rd[, -"Alter"]),
      include.rownames = FALSE)
print(xtable::xtable(x = fc_tbl_full[, -"Alter"]),
      include.rownames = FALSE)

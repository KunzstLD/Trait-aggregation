# ____________________________________________________________________________________________
# Calculate functional diversity metrics 
# following Villeger et al. 2008
# - functional richness: functional space filled (most extreme points of convex hull)
# - functional evenness: evenness/regularity of species abundances in trait space 
#  [0 - 1]
#  (decreases if distances between taxa are less regular, less evenly distributed)
# - functional divergence: How is abundance distributed within the trait space volume 
#  (2D case: would be high if high number (most abundant species have high trait values, 
#  divergence is high)
#  [0 - 1]
# ____________________________________________________________________________________________

# Data processing ----

## Load aggregated trait datasets ----
agg_data <- readRDS(file.path(data_cache, "Re-analysis_cache", "agg_data.rds"))
ecor_L <- readRDS(file.path(data_cache, "Re-analysis_cache", "ecor_L_corr.rds"))
ecor_R <- readRDS(file.path(data_cache, "Re-analysis_cache", "ecor_R_corr.rds"))
setDT(ecor_R)

## Data that need to be pooled or removed ----
# Combination of years and sites that need to be pooled/or data with highest abundance
# needs to be picked (we have the same site sampled multiple times in a year)
comb_siteyear <- ecor_R[, .N, by = .(site, year)] %>%
  .[N > 1,]

# Sample ids with one observation within a year
keep_ids <- ecor_R[, .(sample_id, .N), by = .(site, year)] %>%
  .[N == 1, sample_id]
  
# merge ecor_R site & year
ecor_L$sample_id <- rownames(ecor_L)
ecor_L <- merge(ecor_L,
                ecor_R[, .(site, year, sample_id)],
                by = "sample_id")
setDT(ecor_L)

# Extract sample IDs (per year and site) with highest abundances
ids <- list()
for(i in 1:nrow(comb_siteyear)) {
  st <- comb_siteyear[i, site]
  yr <- comb_siteyear[i, year]
  
  ids[[i]] <- ecor_L[site == st & year == yr, ] %>%
    melt(
      .,
      id.vars = c("sample_id", "site", "year"),
      variable.name = "taxon",
      value.name = "abundance"
    ) %>%
    .[, .(site, year, abund_total = sum(abundance)), by = sample_id] %>%
    unique(.) %>%
    .[abund_total == max(abund_total), sample_id]
}
keep_ids <- c(keep_ids, as.numeric(unlist(ids)))

# subset ecor_L & ecor_R
ecor_R_high <- ecor_R[sample_id %in% keep_ids, ]
ecor_L_high <- ecor_L[sample_id %in% keep_ids, ]
setDF(ecor_L_high)
rownames(ecor_L_high) <- ecor_L_high$sample_id
ecor_L_high$sample_id <- NULL
ecor_L_high$year <- NULL
ecor_L_high$site <- NULL

## Identification of taxa with zero abundance ----

# Taxa with zero abundances from ecor_L and trait data
rm_taxa <- melt(ecor_L[sample_id %in% keep_ids,], id.vars = c("site", "year", "sample_id")) %>%
  .[, sum(value), by = variable] %>%
  .[V1 == 0, variable]

# rm
ecor_L_high <- ecor_L_high[, !names(ecor_L_high) %in% rm_taxa]
agg_data <- lapply(agg_data, function(y) y[!rownames(y) %in% rm_taxa, ])

# save
saveRDS(ecor_L_high, 
        file = file.path(data_cache, "Re-analysis_cache", "ecor_L_multiple_samp_corr.rds"))
saveRDS(ecor_R_high, 
        file = file.path(data_cache, "Re-analysis_cache", "ecor_R_multiple_samp_corr.rds"))
saveRDS(agg_data, 
        file = file.path(data_cache, "Re-analysis_cache", "agg_data_multiple_samp_corr.rds"))

# Calculate FRic, FEve & FDiv ----
fd_res <- lapply(agg_data, function(y) dbFD(x = y, 
                                            a = ecor_L_high, 
                                            corr = "cailliez"))
saveRDS(fd_res,
        file = file.path(data_cache, "Re-analysis_cache", "fd_res.rds"))





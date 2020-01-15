# _____________________________________________________________________________
#### Aggregation ####
# Here two trait aggregation methods are compared 
# to traits assigned on family level
# _____________________________________________________________________________

# how many entries per family get aggregated
AST_subset[, .N, by = .(genus, family)]

# First aggregation step -> Median

# create name pattern to choose traits
pat_traitname <- paste(trait_col, collapse = "|")

# subset so that no NA values occur in Species data (otherwise all NA entries are viewed as a group &
# aggregated as well)
AST_subset_genus <- AST_subset[!is.na(species), lapply(.SD, median),
                               .SDcols = names(AST_subset) %like% pat_traitname,
                               by = genus]

# merge family information back
AST_subset_genus[AST_subset[!is.na(species),],
                 `:=`(family = i.family,
                      order = i.order,
                      unique_id = i.unique_id),
                 on = "genus"]

# aggregate on family level
AST_subset_fam <- AST_subset_genus[, c(lapply(.SD, function(y) {
  if (length(unique(y)) == length(y) & length(y) > 1) {
    max(y)
    # e.g. in case (0,0,3)
  } else if (Mode(y) == 0 & !all((y) == 0)) {
    Mode(y[y != 0])
  }
  else {
    Mode(y)
  }
})),
.SDcols = names(AST_subset_genus) %like% pat_traitname,
by = "family"]


# compare to data resolved on family level -> check original data
# load preprocessed but not harmonized dataset
Trait_AST_preproc <- readRDS(
  file = file.path(
    data_in,
    "Australian",
    "Trait_analysis",
    "Cache_preproc",
    "Trait_AST_preproc.rds"
  )
)

# just use Chessman traits for the beginning 
vec <- Trait_AST_preproc[is.na(Species) &
                           is.na(Genus) & Family %in% candidates, .SD,
                         .SDcols = names(Trait_AST_preproc) %like% paste("fam.*Chessman|Family")] %>%
  .[, apply(.SD, 1, function(y)
    sum(y) > 0),
    .SDcols = !(names(.) %like% "Family")]

test_AST <- Trait_AST_preproc[is.na(Species) &
                                is.na(Genus) &
                                Family %in% candidates, .SD,
                              .SDcols = names(Trait_AST_preproc) %like% paste("fam.*Chessman|Family")] %>%
  .[vec,]


# shredder
# scraper
# predator
# gatherer
# filter 
# functional spiracles -> resp_spi
# air respiration   -> resp_spi
# gills -> resp_gil
# small, medium, large

# _____________________________________________________________________________
#### Analysis
# _____________________________________________________________________________

# pattern
pattern <- "(?i)shredder|scraper|predator|gatherer|filter|small|medium|large"

# introduce key column
setnames(AST_subset_fam,
         old = "feed_filter",
         new = "feed_filterer")

AST_subset_fam <- AST_subset_fam %>%
  melt(., id.vars = c("family")) %>%
  .[variable %like% pattern,] %>%
  .[, key := tolower(sub("(.*)(\\_)(.*)", "\\3", variable))]

setnames(test_AST,
         old = c("Family",
                 "size_small_fam_Chessman",
                 "size_medium_fam_Chessman",
                 "size_large_fam_Chessman"),
         new = c("family",
                 "small_fam_Chessman",
                 "medium_fam_Chessman",
                 "large_fam_Chessman"),
         skip_absent = TRUE)

test_AST <- test_AST %>% melt(.) %>%
  .[variable %like% pattern,] %>%
  .[, key := tolower(sub("(?=\\_)(\\w+)", "", variable, perl = TRUE))]

# merge both & calc deviance between trait values
AST_subset_fam[test_AST,
               `:=`(dev_trait = value - i.value,
                    value_fam_level = i.value),
               on = c("family", "key")]

# prepare & plot deviance data
AST_subset_fam[, dev_trait := round(dev_trait, 2)] %>%
  ggplot(., aes(x = as.factor(variable), y = dev_trait, label = dev_trait)) +
  geom_point(stat = "identity", aes(col = family), size = 8) +
  geom_text(color = "white", size = 3) +
  labs(
    title = "Comparison aggregated traits with at family-level assigned traits",
    y = "Deviance in trait values",
    x = "Trait states"
  ) +
  ylim(-1.5, 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  facet_wrap(~family) +
  theme(
    axis.title = element_text(size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 9), #### For altering USGS trait databases
    ## goal: summarize trait information into one line per genus
    ## data is trimmed to only genera found in the Grand Lake Meadows

    axis.text.y = element_text(family = "Roboto Mono", size = 9),
    legend.text = element_text(size = 11),
    legend.title = element_blank()
  )
# save
ggsave(
  filename = "Trait_agg.png", plot = last_plot(),
  path = file.path(data_out),
  dpi = 400
)
# implement different Aggr. functions
# search for further candidates
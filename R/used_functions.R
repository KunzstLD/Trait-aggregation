# _____________________________________________________________
#### Data preparation ####
# _____________________________________________________________

# load data from input path and store in list
# and assigns name (according to filename)
load_data <- function(path, pattern) {
  files <- list.files(path = path, pattern = pattern)
  data <- lapply(files, function(y) readRDS(file = file.path(path, y)))
  data <- setNames(data, files)
  data
}

# wrapper around xtable to print latex code without row.names! 
xtable_wo_rownames <-
  function(x,
           caption = NULL,
           label = NULL,
           align = NULL,
           digits = NULL,
           display = NULL,
           auto = FALSE,
           ...) {
    print(
      xtable(
        x = x,
        caption = caption,
        label = label,
        align = align,
        digits = digits,
        display = display,
        auto = auto,
        ...
      ),
      include.rownames = FALSE
    )
  }

# Google search 
query_google <- function(x) {
  lapply(
    x,
    function(y) {
      utils::browseURL(url = paste0("https://google.com/search?q=", y))
    }
  ) %>%
    invisible()
}

# Capitalize the first letter #
simple_cap <- function(x) {
  s <- tolower(x)
  paste0(toupper(substring(s, 1, 1)), substring(s, 2))
}

# ___________________________________________________________________________________
#### create individual pattern of trait name (not category!) ####
# i.e. feed_herbivore, feed_shredder -> feed
# TODO: Add unit test
# ___________________________________________________________________________________
create_pattern_ind <- function(x, non_trait_cols) {
  pat <- paste0(non_trait_cols, collapse = "|")
  # get trait names & create pattern for subset
  trait_names_pattern <-
    grep(pat, names(x), value = TRUE, invert = TRUE) %>%
    sub("\\_.*|\\..*", "", .) %>%
    unique() %>%
    paste0("^", .)
  return(trait_names_pattern)
}

# ___________________________________________________________________________________
#### Normalization of trait scores ####
# All trait states of one trait are divided by their row sum
# Hence, trait affinities are represented as "%" or ratios
# ___________________________________________________________________________________
normalize_by_rowSum <- function(x, non_trait_cols) {

  # get trait names & create pattern for subset
  trait_names_pattern <- create_pattern_ind(
    x = x,
    non_trait_cols = non_trait_cols
  )

  # loop for normalization (trait categories for each trait sum up to 1)
  for (cols in trait_names_pattern) {
    # get row sum for a specific trait
    x[, rowSum := apply(.SD, 1, sum), .SDcols = names(x) %like% cols]

    # get column names for assignment
    col_name <- names(x)[names(x) %like% cols]

    # divide values for each trait state by
    # the sum of trait state values
    x[, (col_name) := lapply(.SD, function(y) {
      y / rowSum
    }),
    .SDcols = names(x) %like% cols
    ]
  }

  # del rowSum column
  x[, rowSum := NULL]
  x
}

# _________________________________________________________________
#### check for completeness of trait dataset ####
# not used anymore
# _________________________________________________________________
completeness_trait_data <- function(x, non_trait_cols) {
  trait_names_pattern <- create_pattern_ind(
    x = x,
    non_trait_cols = non_trait_cols
  )

  # test how complete trait sets are
  output <- matrix(ncol = 2, nrow = length(trait_names_pattern))
  for (i in seq_along(trait_names_pattern)) {
    # vector containing either 0 (no NAs) or a number (> 0) meaning that all
    # entries for this trait contained NA
    vec <-
      x[, apply(.SD, 1, function(y) {
        base::sum(is.na(y))
      }),
      .SDcols = names(x) %like% trait_names_pattern[[i]]
      ]

    # How complete is the dataset for each individual trait?
    output[i, ] <-
      c(
        (length(vec[vec == 0]) / nrow(x)) %>% `*`(100) %>% round(),
        trait_names_pattern[[i]]
      )
  }
  return(output)
}

# _______________________________________________________________________
#### Return nr of taxa on taxonomic levels species, genus and family ####
# _______________________________________________________________________
taxonomic_lvl <- function(data) {
  if(!is.data.table(data))
    setDT(data)
  spec <- data[!is.na(species), .N]
  gen <- data[is.na(species) & !is.na(genus), .N]
  fam <-
    data[is.na(species) & is.na(genus) & !is.na(family), .N]
  
  output <- data.table(species = spec, 
                       genus = gen, 
                       family = fam)
  return(output)
}

# ___________________________________________________________________
#### Trait Aggregation ####
# ___________________________________________________________________

# Mode
# when there are no duplicate values, mode returns the first value!
Mode <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# aggregation function used 
# in spec_genus_agg
agg_fun_mean <- function(y, na.rm = FALSE) {
  # just one value return that value
  if (length(y) == 1)
    y
  # mode for cases with duplicates for one value, like:
  # c(1,1,1,3,0)
  else if (length(names(table(y)[table(y) > 1])) == 1)
    Mode(y, na.rm = na.rm)
  # median for cases with multiple duplicates of the same length
  else if (length(names(table(y)[table(y) > 1])) > 1 &
           length(names(table(y)[table(y) == 1])) == 0)
    mean(y, na.rm = na.rm)
  # mean for cases with multiple duplicates
  # and distinct values, like
  # c(1,1,2,2,3)
  else if (length(names(table(y)[table(y) > 1])) > 1 &
           length(names(table(y)[table(y) == 1])) > 0)
    mean(y, na.rm = na.rm)
  # for distinct values 
  else if (length(y) == length(unique(y)))
    mean(y, na.rm = na.rm)
}

# slightly altered in comparison to agg_fun
# uses median instead of mean
agg_fun_median <- function(y, na.rm = FALSE) {
  # just one value return that value
  if (length(y) == 1)
    y
  # mode for cases with duplicates for one value, like:
  # c(1,1,1,3,0)
  else if (length(names(table(y)[table(y) > 1])) == 1)
    Mode(y, na.rm = na.rm)
  # median for cases with multiple duplicates of the same length
  else if (length(names(table(y)[table(y) > 1])) > 1 &
           length(names(table(y)[table(y) == 1])) == 0)
    median(y, na.rm = na.rm)
  # median for cases with multiple duplicates
  # and distinct values, like
  # c(1,1,2,2,3)
  else if (length(names(table(y)[table(y) > 1])) > 1 &
           length(names(table(y)[table(y) == 1])) > 0)
    median(y, na.rm = na.rm)
  # median for distinct values 
  else if (length(y) == length(unique(y)))
    median(y, na.rm = na.rm)
}

# Stepwise aggregation to family-level by allocating species entries
# to genus level using the median, then aggregating to family-level
# using a method decided by the user as well.

# spec_genus aggegation using improved agg_function
spec_genus_agg_alt <- function(trait_data,
                           non_trait_cols, 
                           method,
                           na.rm = TRUE) {
  # get names of trait columns
  pat <- paste0(non_trait_cols, collapse = "|")
  trait_col <- grep(pat, names(trait_data), value = TRUE, invert = TRUE)
  
  # aggregate to genus level via median
  # subset so that no NA values occur in species data
  # (otherwise all NA entries are viewed as a group &
  # aggregated as well)
  trait_data_genus <- trait_data[!is.na(genus),
                           lapply(.SD, method, na.rm = na.rm),
                           .SDcols = trait_col,
                           by = "genus"]
  
  # merge family information back
  # !is.na(species)
  trait_data_genus[trait_data[!is.na(genus)],
                   `:=`(family = i.family,
                        order = i.order),
                   on = "genus"]
  
  # bind with data resolved on family level
  trait_data_genus <-
    rbind(trait_data_genus,
          trait_data[is.na(species) & is.na(genus) & !is.na(family), ],
          fill = TRUE)
  
  # aggregate to family level
  agg_data <- trait_data_genus[, c(lapply(.SD, method, na.rm = na.rm)),
  .SDcols = trait_col,
  by = "family"]
  
  # merge information on order back
  agg_data[trait_data,
           `:=`(order = i.order),
           on = "family"]
}

# direct aggregation to family level
direct_agg <- function(trait_data,
                       non_trait_cols, 
                       method,
                       na.rm = TRUE) {
  # get names of trait columns
  pat <- paste0(non_trait_cols, collapse = "|")
  trait_col <- grep(pat, names(trait_data), value = TRUE, invert = TRUE)

  # aggregate to genus level via median
  # subset so that no NA values occur in species data
  # (otherwise all NA entries are viewed as a group &
  # aggregated as well)
  agg_data <- trait_data[, lapply(.SD, method, na.rm = na.rm),
    .SDcols = trait_col,
    by = "family"
  ]

  # merge information on order back
  agg_data[trait_data,
    `:=`(order = i.order),
    on = "family"
  ]
  agg_data
}

# weighted aggregation
# TODO testing
# Calculates a weighted average based on the number of 
# species entries per genera 
weighted_agg <- function(trait_data,
                         non_trait_cols,
                         na.rm = TRUE) {
  # get names of trait columns
  pat <- paste0(non_trait_cols, collapse = "|")
  trait_col <- grep(pat, names(trait_data), value = TRUE, invert = TRUE)
  
  # create copy
  data_cp <- copy(trait_data)
  
  # calc weightes
  data_cp[!is.na(species), coeff := .N,
          by = "genus"]
  # assign 1 to coeff entries with NA
  data_cp[is.na(coeff), coeff := 1]
  
  # calculate weighted mean
  data_cp <- data_cp[, lapply(.SD, function(y)
    weighted.mean(y, coeff, na.rm = na.rm)),
    .SDcols = trait_col,
    by = "family"]
  
  # merge back information on order
  data_cp[trait_data, 
          order := i.order, 
          on = "family"]
  
  return(data_cp)
}

# TODO: Monitor subset data (how many entries in species/genus)
# Aggregation for EU data (includes complementation from tachet)
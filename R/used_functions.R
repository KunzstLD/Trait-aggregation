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

# _______________________________________________________________________
#### Get only complete trait data ####
# from a dataset with traits and taxa information
# get those traits that are complete (meaning at least one value different
# from zero)
# Gives a list with as many datasets as traits are presented
# Can be merged together using Reduce!
# works atm without considering NAs
# non trait column need to be manually defined by user
# _______________________________________________________________________
get_complete_trait_data <- function(trait_data, non_trait_col) {

  # pattern of non trait col names
  non_trait_col_pat <- paste0("?(i)", paste0(non_trait_col, collapse = "|"))

  # create name vector
  name_vec <-
    grep(
      non_trait_col_pat,
      names(trait_data),
      invert = TRUE,
      value = TRUE
    ) %>%
    sub("\\_.*", "", .) %>%
    unique()

  # create output matrix
  output <- matrix(ncol = 2, nrow = length(name_vec))

  data <- list()
  for (i in seq_along(name_vec)) {
    row <- trait_data[, base::sum(.SD) > 0,
      .SDcols = names(trait_data) %like% name_vec[i],
      by = 1:nrow(trait_data)
    ]$V1

    #
    data[[i]] <- trait_data[row, .SD,
      .SDcols = names(trait_data) %like%
        paste(c(
          name_vec[i],
          non_trait_col
        ),
        collapse = "|"
        )
    ]
    names(data)[[i]] <- name_vec[[i]]
  }
  return(data)
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
      round(y / rowSum, digits = 2)
    }),
    .SDcols = names(x) %like% cols
    ]
  }

  # del rowSum column
  x[, rowSum := NULL]
  return(x)
}

# ___________________________________________________________________________________
#### check for completeness of trait dataset ####
# ___________________________________________________________________________________
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

# Aggregation to family level by allocating species entries
# to genus level using the median.
# Aggregating to family level is done by mode or median
# if no duplicate values occur
spec_genus_agg <- function(trait_data,
                           non_trait_cols) {

  # get names of trait columns
  pat <- paste0(non_trait_cols, collapse = "|")
  trait_col <- grep(pat, names(trait_data), value = TRUE, invert = TRUE)

  # aggregate to genus level via median
  # subset so that no NA values occur in species data
  # (otherwise all NA entries are viewed as a group &
  # aggregated as well)
  trait_data_genus <- trait_data[!is.na(species),
    lapply(.SD, median, na.rm = TRUE),
    .SDcols = trait_col,
    by = genus
  ]

  # merge family information back
  trait_data_genus[trait_data[!is.na(species), ],
    `:=`(
      family = i.family,
      order = i.order
    ),
    on = "genus"
  ]

  # bind with data resolved on genus level or family level
  # TODO: Change this in scripts for Convergence of trait profile groups!
  trait_data_genus <-
    rbind(trait_data_genus, trait_data[is.na(species) & !is.na(family), ],
      fill = TRUE
    )

  # aggregate to family level
  agg_data <- trait_data_genus[, c(lapply(.SD, function(y) {
    # for cases like c(0,1), c(0,0,1,1) and c(0,1,0.5)
    if (length(unique(y)) == length(y) |
      sum(duplicated(y)) == sum(!duplicated(y))) {
      median(y, na.rm = TRUE)
      # e.g. in case (0,0,3)
      # } else if (Mode(y, na.rm = TRUE) == 0 & !all((y) == 0)) {
      #   Mode(y[y != 0], na.rm = TRUE)
    }
    else {
      Mode(y, na.rm = TRUE)
    }
  }), .N),
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

# direct aggregation to family level
direct_agg <- function(trait_data,
                       non_trait_cols) {
  # get names of trait columns
  pat <- paste0(non_trait_cols, collapse = "|")
  trait_col <- grep(pat, names(trait_data), value = TRUE, invert = TRUE)

  # aggregate to genus level via median
  # subset so that no NA values occur in species data
  # (otherwise all NA entries are viewed as a group &
  # aggregated as well)
  agg_data <- trait_data[, lapply(.SD, median, na.rm = TRUE),
    .SDcols = trait_col,
    by = family
  ]

  # merge information on order back
  agg_data[trait_data,
    `:=`(order = i.order),
    on = "family"
  ]
  agg_data
}

# TODO: Monitor subset data (how many entries in species/genus)
# TODO: Weigthing?
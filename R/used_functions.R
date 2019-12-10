
# ___________________________________________________________________________________
# Get only complete trait data 
# from a dataset with traits and taxa information
# get those traits that are complete (meaning at least one value different 
# from zero)
# Gives a list with as many datasets as traits are presented
# Can be merged together using Reduce!
# works atm without considering NAs 
# non trait column need to be manually defined by user
# ___________________________________________________________________________________
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
                      by = 1:nrow(trait_data)]$V1
    
    #
    data[[i]] <- trait_data[row, .SD,
                            .SDcols = names(trait_data) %like%
                              paste(c(
                                name_vec[i],
                                non_trait_col
                              ),
                              collapse = "|")]
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
  trait_names_pattern <- create_pattern_ind(x = x,
                                            non_trait_cols = non_trait_cols)
  
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
    .SDcols = names(x) %like% cols]
  }
  
  # del rowSum column
  x[, rowSum := NULL]
  return(x)
}

# ___________________________________________________________________________________
#### check for completeness of trait dataset ####
# ___________________________________________________________________________________
completeness_trait_data <- function(x, non_trait_cols) {
  
  trait_names_pattern <- create_pattern_ind(x = x, 
                                            non_trait_cols = non_trait_cols)
  
  # test how complete trait sets are
  output <- matrix(ncol = 2, nrow = length(trait_names_pattern))
  for (i in seq_along(trait_names_pattern)) {
    # vector containing either 0 (no NAs) or a number (> 0) meaning that all
    # entries for this trait contained NA
    vec <-
      x[, apply(.SD, 1, function(y)
        base::sum(is.na(y))),
        .SDcols = names(x) %like% trait_names_pattern[[i]]]
    
    # How complete is the dataset for each individual trait?
    output[i,] <-
      c((length(vec[vec == 0]) / nrow(x))  %>% `*` (100) %>% round(),
        trait_names_pattern[[i]])
  }
  return(output)
}

# ___________________________________________________________________________________
#### Trait Aggregation ####
# Mode
# when there are no duplicate values, mode returns the first value!
# ___________________________________________________________________________________
Mode <- function(x, na.rm = FALSE) {
  if (na.rm)
    x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
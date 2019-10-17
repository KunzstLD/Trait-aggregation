# Get only complete trait data -------------------------------------------------------
# from a dataset with traits and taxa information
# get those traits that are complete (meaning at least one value different 
# from zero)
# Gives a list with as many datasets as traits are presented
# Can be merged together using Reduce!
# works atm without considering NAs 
# non trait column need to be manually defined by user
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

# Trait Aggregation -------------------------------------------------------
# Mode
# when there are no duplicate values, mode returns the first value!
Mode <- function(x, na.rm = FALSE) {
  if (na.rm)
    x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

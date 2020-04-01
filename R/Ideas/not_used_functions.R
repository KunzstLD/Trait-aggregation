# Geometric mean
geom_mean <- function(x, na.rm = TRUE) {
  if(na.rm)
    x <- x[!is.na(x)]
  prod(x) ^ (1 / length(x))
}

# agg fun with geom_mean
# only useful when no zeros occur
agg_fun <- function(y, na.rm = TRUE) {
  y <- y[y != 0]
  # if just zeros
  if(identical(y, numeric(0)))
    0
  # if just NA return NA
  # else (is.na(y))
  # just one value return that value
  else if (length(y) == 1)
    y
  # mode for cases with duplicates for one value, like:
  # c(1,1,1,3,0)
  else if (length(names(table(y)[table(y) > 1])) == 1)
    Mode(y, na.rm = na.rm)
  # median for cases with multiple duplicates
  else if (length(names(table(y)[table(y) > 1])) > 1 &
           length(names(table(y)[table(y) == 1]))  == 0)
    median(y, na.rm = na.rm)
  # geometric mean for cases with multiple duplicates and
  # distinct values, like:
  # c(1,1,2,2,3)
  else if (length(names(table(y)[table(y) > 1])) > 1 &
           length(names(table(y)[table(y) == 1]))  > 0)
    geom_mean(y, na.rm = na.rm)
  # geometric mean for distinct values
  else if (length(y) == length(unique(y)))
    geom_mean(y, na.rm = na.rm)
}

# species genus agg function using agg_fun with geometric mean
spec_genus_agg_2 <- function(data,
                             non_trait_cols) {
  # get names of trait columns
  pat <- paste0(non_trait_cols, collapse = "|")
  trait_col <- grep(pat, names(data), value = TRUE, invert = TRUE)
  
  # aggregate to genus level via median
  # subset so that no NA values occur in species data
  # (otherwise all NA entries are viewed as a group &
  # aggregated as well)
  trait_data_genus <- data[!is.na(species),
                           lapply(.SD, median, na.rm = TRUE),
                           .SDcols = trait_col,
                           by = genus]
  
  # merge family information back
  trait_data_genus[data[!is.na(species),],
                   `:=`(family = i.family,
                        order = i.order),
                   on = "genus"]
  
  # bind with data resolved on genus level or family level
  # TODO: Change to is.na(genus) and add taxa res. on family-level afterwards
  trait_data_genus <-
    rbind(trait_data_genus,
          data[is.na(species) & !is.na(genus), ] %>%
            .[!genus %in% trait_data_genus$genus, ],
          fill = TRUE)
  
  # # aggregate to family level
  agg_data <- trait_data_genus[, c(lapply(.SD, agg_fun)),
                               .SDcols = trait_col,
                               by = "family"]
  
  # merge information on order back
  agg_data[data,
           `:=`(order = i.order),
           on = "family"]
  
  # bind data on family-level
  if (nrow(data[is.na(species) & is.na(genus), ]) == 0)
    agg_data <- agg_data
  else
    agg_data <-
    rbind(agg_data, data[is.na(species) & is.na(genus), ]  %>%
            .[!family %in% agg_data$family, -c("species", "genus")] %>%
            .[!duplicated(family), ])
}
# Investigate cases where trait aggregation procedures
# yielded different results
# Baetidae -> feeding mode -> Australia
# Glossomatidae
test <- preproc_dat[["Trait_AUS_harmonized"]]

test <- test[family %in% "Hydroptilidae", .SD,
             .SDcols = names(test) %like% "^resp.*|^feed.*|species|genus|family|order"]

# 1) complex aggregation:
pat <- paste0(c("species",
                "genus",
                "family", 
                "order"),
              collapse = "|")
trait_col <- grep(pat, names(test), value = TRUE, invert = TRUE)

# aggregate to genus level via median
# Baetidae example: just one entry
trait_data_genus <- test[!is.na(genus),
                         lapply(.SD, median, na.rm = TRUE),
                         .SDcols = trait_col,
                         by = genus]

# merge family information back
trait_data_genus[test[!is.na(genus),],
                 `:=`(family = i.family,
                      order = i.order),
                 on = "genus"]

# original
trait_data_genus <-
  rbind(trait_data_genus,
        test[is.na(species) & !is.na(genus), ] %>%
          .[!genus %in% trait_data_genus$genus, ],
        fill = TRUE)

# bind with data resolved on family-level
# instead of genus-level 
trait_data_genus <-
  rbind(trait_data_genus,
        test[is.na(species) & is.na(genus) & !is.na(family), ],
        fill = TRUE)

# # aggregate to family level
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
})),
.SDcols = trait_col,
by = "family"]

# merge information on order back
agg_data[test,
         `:=`(order = i.order),
         on = "family"]

# bind data on family-level
agg_data <-
  rbind(agg_data, test[is.na(species) & is.na(genus), ]  %>%
          .[!family %in% agg_data$family, -c("species", "genus")] %>%
          .[!duplicated(family), ])



# direct aggrgeation
# aggregate to genus level via median
agg_data <- test[, lapply(.SD, median, na.rm = TRUE),
                         .SDcols = trait_col,
                         by = family
                         ]
  
# merge information on order back
agg_data[test,
           `:=`(order = i.order),
           on = "family"
           ]
agg_data




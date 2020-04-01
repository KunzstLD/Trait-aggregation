# Investigate cases where trait aggregation procedures
# yielded different results:
# Leptophlebiidae
test <- preproc_dat[["Trait_AUS_harmonized"]]

test <- test[family %in% "Leptophlebiidae", .SD,
             .SDcols = names(test) %like% "^resp.*|^feed.*|^locom.*|^size.*|species|genus|family|order"]

# 1) complex aggregation:
pat <- paste0(c("species",
                "genus",
                "family", 
                "order"),
              collapse = "|")
trait_col <- grep(pat, names(test), value = TRUE, invert = TRUE)

# aggregate to genus level via median
trait_data_genus <- test[!is.na(genus),
                         c(lapply(.SD, median, na.rm = TRUE), .N),
                         .SDcols = trait_col,
                         by = genus]

# merge family information back
trait_data_genus[test[!is.na(genus),],
                 `:=`(family = i.family,
                      order = i.order),
                 on = "genus"]

# bind with data resolved on family-level
# instead of genus-level 
trait_data_genus <-
  rbind(trait_data_genus,
        test[is.na(species) & is.na(genus) & !is.na(family), ],
        fill = TRUE)

## aggregate to family level
agg_data <- trait_data_genus[, c(lapply(.SD, agg_fun_median), .N),
                             .SDcols = trait_col,
                             by = "family"]

#### direct aggrgeation ####
# aggregate to genus level via median
test[, lapply(.SD, median, na.rm = TRUE),
                         .SDcols = trait_col,
                         by = family
                         ]

# Testing "aggregation to family - level" part of the
# "spec_genus_agg" function
fun <- function(x) {
    # for cases like c(0,1), c(0,0,1,1) and c(0,1,0.5)
    if (length(unique(x)) == length(x) |
        sum(duplicated(x)) == sum(!duplicated(x))) {
        median(x)
    } else {
        Mode(x = x)
    }
}

test_that(
    "test aggregating several vectors by mode if duplicates exist, otherwise maximum",
    {
        expect_equal(fun(x = 0), 0)
        expect_equal(fun(x = 1), 1)
        expect_equal(fun(x = c(0, 1)), 0.5)
        expect_equal(fun(x = c(0, 1, 1)), 1)
        expect_equal(fun(x = c(0, 1, 0.5)), 0.5)
        expect_equal(fun(x = c(0, 0, 0)), 0)
        expect_equal(fun(x = c(1, 1, 1)), 1)
        expect_equal(fun(x = c(0.5, 0.5, 1, 1)), 0.75)
        expect_equal(fun(x = c(0, 0, 0, 0.5, 1)), 0)
    }
)

# Test case:
# Aggregation to family level
# test <- AUS_subset[!is.na(species), c(
#     "volt_bi_multi",
#     "volt_semi",
#     "volt_uni"
# ) := lapply(.SD, Mode, na.rm = TRUE),
# .SDcols = names(AUS_subset) %like% "^volt",
# by = "genus"
# ] %>%
#     .[family %in% "Dytiscidae", .(
#         species, genus, family,
#         volt_bi_multi, volt_uni, volt_semi
#     )]

# test[, c(lapply(.SD, function(y) {
#     # take mode if duplicates exist, otherwise maximum
#       if (length(unique(y)) == length(y) & length(y) > 1) {
#           max(y)
#       } else {
#           Mode(x = y)
#       }
#   }), .N),
#   .SDcols = names(test) %like% "^volt",
#   by = "family"
#   ]

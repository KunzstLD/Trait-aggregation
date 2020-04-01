#_______________________________________________________________________
#### testing agg_fun ####
# mean and median versions are tested
#_______________________________________________________________________

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


test_that(
  "test aggregating several vectors by mode if duplicates exist, median for cases
  with several duplicates, mean for distinct values & duplicates with distinct values",
  {
    expect_equal(agg_fun(y = 0), 0)
    expect_equal(agg_fun(y = 1), 1)
    expect_equal(agg_fun(y = c(0, 1)), 0.5)
    expect_equal(agg_fun(y = c(0, 1, 1)), 1)
    expect_equal(agg_fun(y = c(0, 1, 0.5)), 0.5)
    expect_equal(agg_fun(y = c(0, 0, 0)), 0)
    expect_equal(agg_fun(y = c(1, 1, 1)), 1)
    expect_equal(agg_fun(y = c(0.5, 0.5, 1, 1)), 0.75)
    expect_equal(agg_fun(y = c(0, 0, 0, 0.5, 1)), 0)
    expect_equal(agg_fun(y = c(0,0,1,1,2)), 0.8)
    expect_equal(agg_fun(y = c(0,0,1,1,10)), 2.4)
  }
)

test_that(
  "test aggregating several vectors by mode if duplicates exist, median for cases
  with several duplicates, median for distinct values & duplicates with distinct values",
  {
    expect_equal(agg_fun_median(y = 0), 0)
    expect_equal(agg_fun_median(y = 1), 1)
    expect_equal(agg_fun_median(y = c(0, 1)), 0.5)
    expect_equal(agg_fun_median(y = c(0, 1, 1)), 1)
    expect_equal(agg_fun_median(y = c(0, 1, 0.5)), 0.5)
    expect_equal(agg_fun_median(y = c(0, 0, 0)), 0)
    expect_equal(agg_fun_median(y = c(1, 1, 1)), 1)
    expect_equal(agg_fun_median(y = c(0.5, 0.5, 1, 1)), 0.75)
    expect_equal(agg_fun_median(y = c(0, 0, 0, 0.5, 1)), 0)
    expect_equal(agg_fun_median(y = c(0,0,1,1,2)), 1)
    expect_equal(agg_fun_median(y = c(0,0,1,1,10)), 1)
  }
)



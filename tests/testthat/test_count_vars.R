context("count_vars")
library(datafinder)

longdata <- data.frame(factor = factor(rep(1:10, 1001)),
                       numeric = rep(1:10, 1001),
                       logical = rep(c(TRUE, FALSE), 5005))

## Check if count_vars returns an object of class "data.frame"
test_that("count_vars: class data.frame", {
    expect_equal(class(count_vars(longdata)), "data.frame")
})

## Check if no input returns error
test_that("count_vars: count the number of missing dataframe", {
    expect_error(count_vars())
})

## Check if single data object returns data frame of length one with correct
## info
ref_single <- data.frame(
    Dataframe = "longdata",
    factor = 1,
    integer = 1,
    logical = 1,
    total_vars = 3,
    sample_size = 10010
)
test_that("count_vars: count the number of variables in a single dataframe", {
    expect_equal(count_vars(longdata), ref_single)
})

## Check if multiple data objects returns data frame with correct length and
## info
smallerdata <- longdata[1:100, ]
ref_multiple <- data.frame(
    Dataframe = c("longdata", "smallerdata"),
    factor = c(1, 1),
    integer = c(1, 1),
    logical = c(1, 1),
    total_vars = c(3, 3),
    sample_size = c(10010, 100)
)
test_that("count_vars: count the number of variables in multiple dataframes", {
              expect_equal(count_vars(c(longdata, smallerdata)), ref_multiple)
})

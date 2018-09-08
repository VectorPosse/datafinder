context("count_vars")
library(datafinder)

## Check if count_vars returns an object of class "data.frame"
test_that("count_vars: class data.frame", {
    expect_equal(class(count_vars("CO2")), "data.frame")
})

## Check if no input returns error
test_that("count_vars: count the number of missing dataframe", {
    expect_error(count_vars())
})

## Check if single data object returns data frame of length one with correct
## info
ref_single <- data.frame(
    Dataframe = "CO2",
    factor = 2,
    numeric = 2,
    ordered = 1,
    total_vars = 5
)
test_that("count_vars: count the number of variables in a single dataframe", {
    expect_equal(count_vars("CO2"), ref_single)
    expect_equal(count_vars(CO2), count_vars("CO2"))
})

## Check if multiple data objects returns data frame with correct length and
## info
ref_multiple <- data.frame(
        Dataframe = c("CO2", "mtcars"),
        factor = c(2, 0),
        numeric = c(2, 11),
        ordered = c(1, 0),
        total_vars = c(5, 11)
)
test_that("count_vars: count the number of variables in multiple dataframes", {
              expect_equal(count_vars(c("CO2", "mtcars")), ref_multiple)
              expect_equal(count_vars(c(CO2, mtcars)), count_vars(c("CO2", "mtcars")))
})
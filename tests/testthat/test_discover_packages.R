context("discover_packages")
library(datafinder)

# Test output class
test_that("discover_packages returns data.frame", {
    expect_equal(class(discover_packages()), "data.frame")
})

# Test for NAs
test_that("No NA values in discover_packages", {
    expect_equal(sum(is.na(discover_packages())), 0)
})
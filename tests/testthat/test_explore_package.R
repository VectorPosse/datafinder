context("explore_package")
library(datafinder)

library(datasets)

# Test output class
test_that("explore_packages returns data.frame", {
    expect_equal(class(explore_package("datasets")), "data.frame")
})

# Test for NAs
test_that("explore_packages returns data.frame", {
    expect_equal(sum(is.na(explore_package("datasets"))), 0)
})
context("explore_package")
library(datafinder)

library(datasets)

# Test output class
test_that("explore_packages returns data.frame", {
    expect_equal(class(explore_package("datasets")), "data.frame")
})

# Test for NAs
test_that("explore_packages returns no NAs", {
    expect_equal(sum(is.na(explore_package("datasets"))), 0)
})

# Test that we can pass package quoted or unquoted
test_that("explore_packages accepts pkgs as string or not", {
    expect_equal(explore_package(datasets), explore_package("datasets"))
})

# Test that dataframes that require data() command works
test_that("explore_packages works with dataframes that need data()", {
    expect_equal(class(explore_package(bcp)), "data.frame")
})

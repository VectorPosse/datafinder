context("visualize_dataframe")
library(datafinder)

# Make a long data frame
longdata <- as.data.frame(rbind(EuStockMarkets, EuStockMarkets, EuStockMarkets,
                                EuStockMarkets, EuStockMarkets, EuStockMarkets))

# Load in all possible permuations
reasonable <- visualize_dataframe(CO2)
reasonable_override <- visualize_dataframe(CO2, override = TRUE)
expect_warning(wide <- visualize_dataframe(mtcars))
wide_override <- visualize_dataframe(mtcars, override = TRUE)
expect_warning(long <- visualize_dataframe(longdata))
expect_warning(long_override <- visualize_dataframe(longdata, override = TRUE))

# Test that we can pass dataframes quoted or not
test_that("visualize_dataframe: pass dataframe as string or not", {
    expect_silent(visualize_dataframe(women))
    expect_silent(visualize_dataframe("women"))
})

# Test that wide and long dataframes giv warnings
test_that("visualize_dataframe: warnings work as expected", {
    expect_warning(wide <- visualize_dataframe(mtcars))
    expect_warning(long <- visualize_dataframe(longdata))
    expect_warning(long_override <- visualize_dataframe(longdata,
                                                        override = TRUE))
})

# Test output classes
test_that("visualize_dataframe: class gg, ggmatrix", {
    expect_equal(class(reasonable), c("gg", "ggmatrix"))
    expect_equal(class(reasonable_override), c("gg", "ggmatrix"))
    expect_equal(class(wide), c("gg", "ggmatrix"))
    expect_equal(class(wide_override), c("gg", "ggmatrix"))
    expect_equal(class(long), c("gg", "ggmatrix"))
    expect_equal(class(long_override), c("gg", "ggmatrix"))
})

# Test resonably sized data frames
test_that("visualize_dataframe: resonably sized works", {
    expect_equal(reasonable$nrow, ncol(CO2))
})

# Test long data frames
test_that("visualize_dataframe: wide truncates output to 10 cols", {
    expect_equal(wide$nrow, 10)
})

# Test wide data frames
test_that("visualize_dataframe: long no variables are lost", {
    expect_equal(long$nrow, ncol(longdata))
})

# Test that override makes a plot the same length as columns
test_that("visualize_dataframe: override function plots correctly", {
    expect_equal(reasonable_override$nrow, ncol(CO2))
    expect_equal(reasonable_override$ncol, ncol(CO2))
    expect_equal(wide_override$nrow, ncol(mtcars))
    expect_equal(wide_override$ncol, ncol(mtcars))
    expect_equal(long_override$nrow, ncol(longdata))
    expect_equal(long_override$ncol, ncol(longdata))
})

# Test high cardinality plot
test_that("visualize_dataframe: high cardinality plots work", {
    a <- data.frame(factor(1:100))
    expect_warning(expect_error(visualize_dataframe(a)))
    a$b <- factor(rep(1:10, 10))
    expect_warning(visualize_dataframe(a))
})

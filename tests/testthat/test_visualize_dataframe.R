context("visualize_dataframe")
library(datafinder)

# Make a long data frame
longdata <- data.frame(factor = factor(rep(1:10, 1001)),
                       numeric = rep(1:10, 1001),
                       logical = rep(c(TRUE, FALSE), 5005))

# Load in all possible permuations
reasonable <- visualize_dataframe(longdata[1:1000, ])
reasonable_override <- visualize_dataframe(longdata[1:1000, ], override = TRUE)
expect_warning(wide <- visualize_dataframe(longdata[1:1000 , c(1:3, 1:3, 1:3, 1:3)]))
wide_override <- visualize_dataframe(longdata[1:1000 , c(1:3, 1:3, 1:3, 1:3)], override = TRUE)
expect_warning(long <- visualize_dataframe(longdata))
expect_warning(long_override <- visualize_dataframe(longdata, override = TRUE))

# Test that we can pass dataframes quoted or not
test_that("visualize_dataframe: pass dataframe as string or not", {
    expect_silent(visualize_dataframe(women))
    expect_silent(visualize_dataframe("women"))
})

# Test that wide and long dataframes giv warnings
test_that("visualize_dataframe: warnings work as expected", {
    expect_warning(wide <- visualize_dataframe(longdata[1:1000 , c(1:3, 1:3, 1:3, 1:3)]))
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
    expect_equal(reasonable$nrow, ncol(longdata[1:1000, ]))
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
    expect_equal(reasonable_override$nrow, ncol(longdata[1:1000, ]))
    expect_equal(reasonable_override$ncol, ncol(longdata[1:1000, ]))
    expect_equal(wide_override$nrow, ncol(longdata[1:1000 , c(1:3, 1:3, 1:3, 1:3)]))
    expect_equal(wide_override$ncol, ncol(longdata[1:1000 , c(1:3, 1:3, 1:3, 1:3)]))
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

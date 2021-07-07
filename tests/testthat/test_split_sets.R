context("split_sets")
library(helprs)

test_that("returned list is of correct length", {
    expect_equal(2, length(split_sets(mtcars)))
    expect_equal(3, length(split_sets(mtcars, validation=TRUE)))
})

test_that("df names are correct", {
    expect_equal("train", names(split_sets(mtcars))[1])
    expect_equal("train", names(split_sets(mtcars, validation=TRUE))[1])

    expect_equal("test", names(split_sets(mtcars))[2])
    expect_equal("test", names(split_sets(mtcars, validation=TRUE))[2])

    expect_equal("validation", names(split_sets(mtcars, validation=TRUE))[3])

    expect_equal(NA, as.logical(names(split_sets(mtcars))[3]))
})


test_that("props sums to 1", {
    expect_equal(1, sum(c(0.7, 0.3)))
})

test_that("too-small dataframes cannot be parsed", {
    expect_error(
        split_sets(mtcars[1:15,1:11]),
        "Dataframe must have at least 20 observations.")
})

test_that("incorrect props argument is caught", {
    expect_error(
        split_sets(mtcars, props=c(0.6,0.2,0.1,0.1)),
        "The props argument needs to be a numeric vector of length 2 or 3.")
    expect_error(
        split_sets(mtcars, props=c(1)),
        "The props argument needs to be a numeric vector of length 2 or 3.")
    expect_error(
        split_sets(mtcars, props=c(1)),
        "The props argument needs to be a numeric vector of length 2 or 3.")
    expect_error(
        split_sets(mtcars, props=c("a")),
        "The props argument needs to be a numeric vector of length 2 or 3.")
})


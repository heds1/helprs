context("check_na")
library(helprs)

df <- data.frame(x = c('a','b',NA,''), y = c(1,NA,2,NA))

test_that("NAs are summed correctly", {
    expected_result <- c(1,2); names(expected_result) <- c('x','y')
    expect_equal(expected_result, check_na(df))
})

test_that("Empty strings are summed correctly", {
    expected_result <- c(2,2); names(expected_result) <- c('x','y')
    expect_equal(expected_result, check_na(df, empty_str = TRUE))
})

test_that("False positives are not returned", {
    df2 <- data.frame(x = c('a','b','c',''), y = c(1,3,2,2)) 
    expected_result <- c(0,0); names(expected_result) <- c('x','y')
    expect_equal(expected_result, check_na(df2))
})

test_that("False positive spaces are not returned", {
    df2 <- data.frame(x = c('a','b','c',' '), y = c(1,3,2,2)) 
    expected_result <- c(0,0); names(expected_result) <- c('x','y')
    expect_equal(expected_result, check_na(df2))
})

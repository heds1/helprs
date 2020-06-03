context("drop_var")
library(helprs)

test_that("var is dropped", {
    expect_equal(dim(mtcars)[2]-1, dim(drop_var(mtcars, "mpg"))[2])
})

test_that("error is returned", {
    expect_error(drop_var(mtcars,mpg), "object 'mpg' not found")
})


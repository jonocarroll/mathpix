library(mathpix)
context("Connect to API")

skip_on_cran()

test_that("eq1 returns correct LaTeX", {
    expect_equal(mathpix(system.file("eq_no_01.png", package = "mathpix")),
                 "$$\n \\int \\frac { 4 x } { \\sqrt { x ^ { 2 } + 1 } } d x  \n$$")
})


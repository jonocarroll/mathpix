library(mathpix)
context("Connect to API")

test_that("eq1 returns correct LaTeX", {
    skip_on_cran()
    expect_equal(mathpix(system.file("eq_no_01.png", package = "mathpix")),
                 "$$\n \\int \\frac { 4x } { \\sqrt { x ^ { 2} + 1} } d x \n$$")
})

test_that("Travis successfully uses API key", {
    skip_on_cran()
    expect_message(mathpix(system.file("eq_no_01.png", package = "mathpix")),
                   "Using Mathpix APP_ID=jcarroll\n")
})

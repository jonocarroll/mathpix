library(mathpix)
context("Connect to API")

test_that("eq1 returns correct LaTeX", {
    skip_on_cran()
    expect_equal(mathpix(system.file("extdata", "eq_no_01.png", package = "mathpix"), insert = FALSE),
                 "$$\n \\int \\frac { 4 x } { \\sqrt { x ^ { 2 } + 1 } } d x \n$$")
})

test_that("Travis successfully uses API key", {
    skip_on_cran()
    expect_message(mathpix(system.file("extdata", "eq_no_01.png", package = "mathpix"), insert = FALSE),
                   "Using Mathpix APP_ID = jcarroll2\n")
})

test_that("Retrying image processing can be successful", {
    skip_on_cran()
    ## this one no longer fails
    expect_equal(gsub(" ", "", suppressWarnings(mathpix(system.file("extdata", "eq_no_05_screencapfixes.jpg", package = "mathpix"), insert = FALSE, retry = FALSE))),
                 gsub(" ", "", "$$
 p _ { i } ( \\theta ) = c _ { i } + \\frac { 1- c _ { i } } { 1+ e ^ { - a _ { i } ( \\theta - b _ { i } ) } }
$$"))
    ## this one should work
    expect_equal(gsub(" ", "", suppressWarnings(mathpix(system.file("extdata", "eq_no_05_screencapfixes.jpg", package = "mathpix"), insert = FALSE, retry = TRUE))),
                 gsub(" ", "", "$$\n p _ { i } ( \\theta ) = c _ { i } + \\frac { 1- c _ { i } } { 1+ e ^ { - a _ { i } (\\theta - b _ { i } ) } } \n$$"))
})

context("File processing")

test_that("Missing image produces an error", {
    expect_error(mathpix("not_a_file.jpg"))
})

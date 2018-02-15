context("Test get_ppi")

library(washdata)
devtools::install_github("validmeasures/ppitables")

x <- get_ppi(index = 65, ccode = "BGD")

test_that("x is data.frame", {
  expect_is(x, "data.frame")
})


x <- get_ppi(index = NA, ccode = "BGD")

test_that("x is NA", {
  expect_equal(x, NA)
})

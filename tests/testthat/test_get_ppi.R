context("Test get_ppi")

library(washdata)
#devtools::install_github("validmeasures/ppitables")

x <- get_ppi(index = 65, ccode = "BGD")

test_that("x is data.frame", {
  expect_is(x, "data.frame")
})

test_that("error message", {
  expect_error(get_ppi(index = NA, ccode = "BGD"), "PPI score required. Try again.")
})

test_that("error message", {
  expect_error(get_ppi(index = 65, ccode = NA), "Country code required. Try again.")
})

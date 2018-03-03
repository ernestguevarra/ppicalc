context("Test get_ppi")

library(washdata)
library(ppitables)
#devtools::install_github("validmeasures/ppitables")

x <- get_ppi(index = 65, ppiTable = ppiBGD2013)

test_that("x is data.frame", {
  expect_is(x, "data.frame")
})

test_that("error message", {
  expect_error(get_ppi(index = NULL, ppiTable = ppiBGD2013), "PPI score required. Try again.")
})

test_that("error message", {
  expect_error(get_ppi(index = 65, ppiTable = NULL), "Country PPI table required. Try again.")
})

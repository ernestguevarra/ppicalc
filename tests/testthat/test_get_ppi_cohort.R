context("Test get_ppi")

library(washdata)
library(ppitables)

x <- get_ppi_cohort(index = indicatorsDataBGD$ppi,
                    ppiTable = ppiBGD2013)

test_that("x is data.frame", {
  expect_is(x, "data.frame")
})


test_that("error message", {
  expect_error(get_ppi_cohort(index = "ppi", ppiTable = ppiBGD2013), "Index is not numeric. If data is not provided, index must be numeric. Try again.")
})

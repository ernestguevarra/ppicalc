context("Test get_ppi")

library(washdata)
#devtools::install_github("validmeasures/ppitables")

x <- get_ppi_cohort(index = indicatorsDataBGD$ppi,
                    ccode = "BGD")

test_that("x is data.frame", {
  expect_is(x, "data.frame")
})

test_that("error message", {
  expect_error(get_ppi_cohort(data = indicatorsDataBGD,
                              index = indicatorsDataBGD$ppi,
                              ccode = "BGD"), "If data is provided, index must be a character value indicating the variable name for PPI score in data. Try again.")
})


x <- get_ppi_cohort(data = indicatorsDataBGD,
                    index = "ppi",
                    ccode = "BGD")

test_that("x is data.frame", {
  expect_is(x, "data.frame")
})

test_that("error message", {
  expect_error(get_ppi_cohort(index = "ppi", ccode = "BGD"), "Index is not numeric. If data is not provided, index must be numeric. Try again.")
})

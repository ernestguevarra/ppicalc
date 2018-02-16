context("Test get_ppi")

library(washdata)
#devtools::install_github("validmeasures/ppitables")

x <- get_ppi_cohort(index = indicatorsDataBGD$ppi,
                    ccode = "BGD")

test_that("x is data.frame", {
  expect_is(x, "data.frame")
})

################################################################################
#
#' quintile_ppi_cohort
#'
#' Group population by quintiles based on their multiple poverty probabilities
#'
#' @param ppi A data frame of poverty probabilities or likelihoods.
#' @param probs A numeric vector between 0 to 1 of quantile probabilities.
#'     Default values are a vector of equal probabilities (0.2, 0.4, 0.6, 0.8, 1).
#' @return A data frame of multiple quintile classifications of between 1 to 5
#'     of increasing poverty probabilities.
#'
#' @examples
#' # ppi for all poverty definitions using Bangladesh data from
#' # washdata package and PPI lookup table from ppitables package
#' # install.packages("washdata"); library(washdata)
#' ppi <- get_ppi_cohort(index = washdata::indicatorsDataBGD$ppi,
#'                       ppiTable = ppitables::ppiBGD2013)
#' quintile_ppi_cohort(ppi = ppi)
#'
#' @export
#'
#
################################################################################

quintile_ppi_cohort <- function(ppi, probs = c(0.2, 0.4, 0.6, 0.8, 1)) {
  #
  #
  #
  if(class(probs) != "numeric") {
    stop("Quantile probabilities must be a numeric vector. Try again.")
  }

  if(any(probs > 1) | any(probs < 0)) {
    stop("Quantile probabilities must be between 0 and 1. Try again.")
  }
  #
  # Get quintiles of each poverty probabilities in the data frame
  #
  quintileDF <- mapply(FUN = quintile_ppi, ppi = ppi, probs = probs)
  #
  # return output
  #
  return(quintileDF)
}

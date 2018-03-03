################################################################################
#
#' get_ppi
#'
#' Get a household's poverty probability based on their Poverty Probability
#' Index (PPI) score. This function uses PPI lookup tables for 60 countries
#' available via the \code{ppitables} package.
#'
#' @param index The household's PPI score
#' @param ppiTable Name of country PPI table to use from the \code{ppitables}
#'     package
#'
#' @return A vector of household's poverty probabilities based on various poverty
#'     metrics
#'
#' @examples
#' # Apply function on a household in Bangladesh (BGD) with a PPI score of 65
#' #\dontrun{
#' get_ppi(index = 65, ppiTable = ppitables::ppiBGD2013)
#' #}
#'
#' @export
#'
#
################################################################################

get_ppi <- function(index, ppiTable) {

  if(is.null(index)) {
    stop("PPI score required. Try again.", call. = TRUE)
  }

  if(is.null(ppiTable)) {
    stop("Country PPI table required. Try again.", call. = TRUE)
  }

  ppi <- ppiTable[ppiTable[["score"]] == index, ]
  return(ppi)
}

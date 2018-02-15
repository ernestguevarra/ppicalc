################################################################################
#
#' get_ppi
#'
#' Get a household's poverty probability based on their Poverty Probability
#' Index (PPI) score. This function uses PPI lookup tables for 60 countries
#' available via the \code{ppitables} package.
#'
#' @param index The household's PPI score
#' @param ccode Three letter ISO country code
#'
#' @return A vector of household's poverty probabilities based on various poverty
#'     metrics
#'
#' @examples
#' # Apply function on a household in Bangladesh (BGD) with a PPI score of 65
#' \dontrun{
#' get_ppi(index = 65, ccode = "BGD")
#' }
#'
#' @export
#'
#
################################################################################

get_ppi <- function(index, ccode) {

  if(is.na(index) | is.na(ccode)) {
    cat("PPI score and/or country code is NA. Returning NA")
    return(NA)
  }

  ppiData <- get(paste("ppiMatrix", ccode, sep = ""))

  ppi <- subset(ppiData, score == index)

  return(ppi)
}

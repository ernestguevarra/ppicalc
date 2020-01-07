################################################################################
#
#' Get a cohort or sample of households' poverty probability based on each
#' household's Poverty Probability Index (PPI) score. This function uses PPI
#' lookup tables for 60 countries available via the \code{ppitables} package.
#'
#' @param index The households' PPI score of class numeric.
#' @param ppiTable Name of country PPI table to use from the \code{ppitables}
#'     package
#'
#' @return A data frame of households' poverty probabilities based on various
#'     poverty metrics
#'
#' @examples
#' # Apply function on a dataset from Bangladesh that includes PPI scores from
#' # the washdata package
#' \dontrun{
#' get_ppi_cohort(data = washdata::indicatorsDataBGD,
#'                index = "ppi",
#'                ppiTable = ppiBGD2013)
#' }
#'
#' # Apply function on a dataset from Bangladesh but specifying vector for PPI
#' # and keeping data as NULL
#' #\dontrun{
#' get_ppi_cohort(index = washdata::indicatorsDataBGD$ppi,
#'                ppiTable = ppitables::ppiBGD2013)
#' #}
#'
#' @export
#'
#
################################################################################

get_ppi_cohort <- function(index, ppiTable) {

  if(class(index) != "numeric") {
    stop("Index is not numeric. If data is not provided, index must be numeric. Try again.", call. = TRUE)
  }
  #
  # Create concatenating object
  #
  ppiDF <- NULL
  #
  # Cycle through each row of index
  #
  for(i in 1:length(index)) {
    #
    # get_ppi for current index
    #
    ppi <- get_ppi(index = index[i], ppiTable = ppiTable)
    #
    # concatenate ppi
    #
    ppiDF <- data.frame(rbind(ppiDF, ppi))
  }
  #
  # Rename rows
  #
  row.names(ppiDF) <- 1:nrow(ppiDF)
  #
  # return output
  #
  return(ppiDF)
}


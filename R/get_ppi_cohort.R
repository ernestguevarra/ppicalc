################################################################################
#
#' get_ppi_cohort
#'
#' Get a cohort or sample of households' poverty probability based on each
#' household's Poverty Probability Index (PPI) score. This function uses PPI
#' lookup tables for 60 countries available via the \code{ppitables} package.
#'
#' @param data A data frame that contains data on PPI scores.
#' @param index The households' PPI score of class numeric (default). If \code{data}
#'     parameter is provided, then \code{index} should be a character value
#'     indicating the variable name in \code{data} containing the households'
#'     PPI score.
#' @param ccode Three letter ISO country code
#'
#' @return A data frame of households' poverty probabilities based on various poverty
#'     metrics
#'
#' @examples
#' # Apply function on a dataset from Bangladesh that includes PPI scores from
#' # the washdata package
#' \dontrun{
#' get_ppi_cohort(data = indicatorsDataBGD,
#'                index = "ppi",
#'                ccode = "BGD")
#' }
#'
#' # Apply function on a dataset from Bangladesh but specifying vector for PPI
#' # and keeping data as NULL
#' \dontrun{
#' get_ppi_cohort(index = indicatorsDataBGD$ppi,
#'                ccode = "BGD")
#' }
#'
#' @export
#'
#
################################################################################

get_ppi_cohort <- function(data = NULL, index, ccode) {

  if(is.null(data)) {

    if(class(index) != "numeric") {
      stop("Index is not numeric. If data is not provided, index must be numeric. Try again.", call. = TRUE)
    }

    ppi <- mapply(FUN = get_ppi,
                  index = index,
                  ccode = ccode)

    ppiDF <- data.frame(matrix(data = unlist(ppi),
                        nrow = length(index),
                        ncol = ncol(get(paste("ppiMatrix", ccode, sep = ""))),
                        byrow = TRUE))

    names(ppiDF) <- names(get(paste("ppiMatrix", ccode, sep = "")))
  }

  if(!is.null(data)) {

    if(class(index) != "character") {
      stop("If data is provided, index must be a character value indicating the variable name for PPI score in data. Try again.", call. = TRUE)
    }

    ppi <- mapply(FUN = get_ppi,
                  index = data[ , index],
                  ccode = ccode)

    ppiDF <- data.frame(matrix(data = unlist(ppi),
                               nrow = nrow(data),
                               ncol = ncol(get(paste("ppiMatrix", ccode, sep = ""))),
                               byrow = TRUE))

    names(ppiDF) <- names(get(paste("ppiMatrix", ccode, sep = "")))
  }
  return(ppiDF)
}


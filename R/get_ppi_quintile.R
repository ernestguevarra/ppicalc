################################################################################
#
#' get_ppi_quintile
#'
#' Group population by quintiles based on their poverty probability
#'
#' @param ppi A numeric vector of poverty probabilities or likelihoods
#' @param probs A numeric vector between 0 to 1 of quantile probabilities.
#'     Default values are a vector of equal probabilities (0.2, 0.4, 0.6, 0.8, 1).
#' @return A numeric vector between 1 to 5 classifying data into quintiles of
#'     increasing poverty probabilities.
#'
#' @examples
#' #
#'
#' @export
#'
#
################################################################################

get_ppi_quintile <- function(ppi, probs = c(0.2, 0.4, 0.6, 0.8, 1)) {

  if(class(probs) != "numeric") {
    stop("Quantile probabilities must be a numeric vector. Try again.")
  }

  if(length(probs) < 5 | length(probs) > 5) {
    stop("Quantile probabilities must be a vector of lenght 5. Try again.")
  }

  if(probs > 1 | probs < 0) {
    stop("Quantile probabilities must be between 0 and 1. Try again.")
  }
  #
  # Get quintile cutoffs
  #
  qCutOff <- quantile(x = ppi, probs = probs)
  #
  # Classify households by wealth quintile
  #
  pQuintile <- ifelse(ppi <= qCutOff[1], 1,
                 ifelse(ppi > qCutOff[1] & ppi <= qCutOff[2], 2,
                   ifelse(ppi > qCutOff[2] & ppi <= qCutOff[3], 3,
                     ifelse(ppi > qCutOff[3] & ppi <= qCutOff[4], 4, 5))))
  #
  # return output
  #
  return(pQuintle)
}

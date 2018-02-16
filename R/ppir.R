################################################################################
#
#' ppir
#'
#' The Poverty Probability Index (PPI) is a poverty measurement tool
#' for organizations and businesses with a mission to serve the poor. The PPI
#' is statistically-sound, yet simple to use: the answers to 10 questions about
#' a household’s characteristics and asset ownership are scored and then given
#' these PPI scores, the likelihood that the household is living below the poverty
#' line can be determined using this package. This package depends on country-specific
#' lookup tables developed for the PPI available via the \code{ppitables} package.
#'
#' @docType package
#' @name ppir
#' @importFrom utils menu
#'
#'
#
################################################################################


## quiets concerns of R CMD check re: the psus and THRESHOLD that appear in bbw
if(getRversion() >= "2.15.1")  utils::globalVariables(c("score"))

################################################################################
#
#' Get poverty classification labels given a selected country to obtain PPI for
#'
#' @param ccode A character value for three letter ISO code for country to
#'   calculate PPI for. Currently, 12 countries are supported by this function:
#'   1) Colombia (COL); 2) Ivory Coast (CIV); 3) Dominican Republic (DOM);
#'   4) Ghana (GHA); 5) Kenya (KEN); 6) Mozambique (MOZ); 7) Myanmar (MMR);
#'   8) Peru (PER); 9) Philippines (PHL); 10) Rwanda (RWA); 11) Senegal (SEN);
#'   12) Togo (TGO)
#'
#' @return A vector of names/labels of poverty classifications
#'
#' @examples
#'
#' get_poverty_class(ccode = "COL")
#'
#' @export
#'
#
################################################################################

get_poverty_class <- function(ccode) {
  ## Colombia
  if(ccode == "COL") {
    x <- names(ppitables::ppiCOL2018)[2:ncol(ppitables::ppiCOL2018)]
  }
  ## Ivory Coast
  if(ccode == "CIV") {
    x <- names(ppitables::ppiCIV2018)[2:ncol(ppitables::ppiCIV2018)]
  }
  ## Dominican Republic
  if(ccode == "DOM") {
    x <- names(ppitables::ppiDOM2018)[2:ncol(ppitables::ppiDOM2018)]
  }
  ## Ghana
  if(ccode == "GHA") {
    x <- names(ppitables::ppiGHA2019)[2:ncol(ppitables::ppiGHA2019)]
  }
  ## Kenya
  if(ccode == "KEN") {
    x <- names(ppitables::ppiKEN2018)[2:ncol(ppitables::ppiKEN2018)]
  }
  ## Mozambique
  if(ccode == "MOZ") {
    x <- names(ppitables::ppiMOZ2019)[2:ncol(ppitables::ppiMOZ2019)]
  }
  ## Myanmar
  if(ccode == "MMR") {
    x <- names(ppitables::ppiMMR2019)[2:ncol(ppitables::ppiMMR2019)]
  }
  ## Peru
  if(ccode == "PER") {
    x <- names(ppitables::ppiPER2018)[2:ncol(ppitables::ppiPER2018)]
  }
  ## Philippines
  if(ccode == "PHL") {
    x <- names(ppitables::ppiPHL2018)[2:ncol(ppitables::ppiPHL2018)]
  }
  ## Rwanda
  if(ccode == "CIV") {
    names(ppitables::ppiCIV2018)[2:ncol(ppitables::ppiCIV2018)]
  }
  ## Senegal
  if(ccode == "SEN") {
    x <- names(ppitables::ppiSEN2018)[2:ncol(ppitables::ppiSEN2018)]
  }
  ## Togo
  if(ccode == "TGO") {
    x <- names(ppitables::ppiTGO2018)[2:ncol(ppitables::ppiTGO2018)]
  }
  ##
  return(x)
}

################################################################################
#
#' Function to calculate Poverty Probability Index (PPI) for Kenya
#'
#' @param path Directory path to IPA's country-specific PPI data analysis and
#'   collection XLSX file
#' @param ccode A character value for three letter ISO code for country to
#'   calculate PPI for. Currently, 12 countries are supported by this function:
#'   1) Colombia (COL); 2) Ivory Coast (CIV); 3) Dominican Republic (DOM);
#'   4) Ghana (GHA); 5) Kenya (KEN); 6) Mozambique (MOZ); 7) Myanmar (MMR);
#'   8) Peru (PER); 9) Philippines (PHL); 10) Rwanda (RWA); 11) Senegal (SEN);
#'   12) Togo (TGO)
#' @param category Poverty classifications. This defaults to all the poverty
#'   classifications available for the selected country
#'
#' @return A dataframe of PPI for each household in \code{df}.
#'
#' @examples
#' calculate_ppi(path = paste(system.file("ppi", package = "ppicalc"),
#'                            "colombia.xlsx", sep = "/"),
#'               ccode = "COL")
#'
#' @export
#'
#
################################################################################

calculate_ppi <- function(path,
                          ccode,
                          category = get_poverty_class(ccode = ccode)) {
  ## Colombia
  if(ccode == "COL") {
    scoreDF <- ppi_col_ipa(path = path, category = category)
  }
  ## Return results
  return(scoreDF)
}

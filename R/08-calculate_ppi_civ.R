################################################################################
#
#' Get Ivory Coast PPI data from IPA's data analysis and collection tool
#'
#' @param path Directory path to IPA's Ivory Coast PPI data analysis and
#'   collection XLSX file
#'
#' @return A dataframe of household data based on IPA's Ivory Coast PPI data
#'   analysis and collection tool
#'
#' @examples
#' get_data_civ(path = paste(system.file("ppi", package = "ppicalc"), "ivoryCoast.xlsx", sep = "/"))
#'
#' @export
#'
#
################################################################################

get_data_civ <- function(path) {
  df <- openxlsx::read.xlsx(xlsxFile = path,
                            sheet = "Data",
                            startRow = 8,
                            cols = 1:47)
  ## Return data.frame
  return(df)
}


################################################################################
#
#' Function to calculate Poverty Probability Index (PPI) for Ivory Coast using
#' IPA's new PPI methodology
#'
#' @param path Directory path to IPA's Ivory Coast PPI data analysis and
#'   collection XLSX file
#' @param category Poverty classifications that can be calculated for
#'   Ivory Coast using IPA's new PPI methodology. By default, all 15 poverty
#'   classifications for Ivory Coast are specified
#'
#' @return A dataframe of PPI for each household in \code{df}.
#'
#' @examples
#' \dontrun{
#'   ppi_col_ipa(path = paste(system.file("ppi", package = "ppicalc"),
#'                            "ivoryCoast.xlsx", sep = "/"))
#' }
#'
#' @export
#'
#
################################################################################

ppi_civ_ipa <- function(path,
                        category = names(ppitables::ppiCIV2018)[2:ncol(ppitables::ppiCIV2018)]) {

  df <- get_data_col(path = path)

  scoreDF <- data.frame(matrix(data = NA, nrow = nrow(df), ncol = length(category) * 2))
  names(scoreDF) <- c(paste(category, "score", sep = "_"),
                      paste(category, "likelihood", sep = "_"))
  ## Recode data
  ## q1: District?
  q1a <- bbw::recode(var = df[["1A"]],
                     recodes = "NA=NA;else=1")
  q1b <- bbw::recode(var = df[["1B"]],
                     recodes = "NA=NA;else=2")
  q1c <- bbw::recode(var = df[["1C"]],
                     recodes = "NA=NA;else=3")
  q1d <- bbw::recode(var = df[["1D"]],
                     recodes = "NA=NA;else=4")
  q1e <- bbw::recode(var = df[["1E"]],
                     recodes = "NA=NA;else=5")
  q1f <- bbw::recode(var = df[["1F"]],
                     recodes = "NA=NA;else=6")
  q1g <- bbw::recode(var = df[["1G"]],
                     recodes = "NA=NA;else=7")
  q1h <- bbw::recode(var = df[["1H"]],
                     recodes = "NA=NA;else=8")
  q1i <- bbw::recode(var = df[["1I"]],
                     recodes = "NA=NA;else=9")
  q1j <- bbw::recode(var = df[["1J"]],
                     recodes = "NA=NA;else=10")
  q1k <- bbw::recode(var = df[["1K"]],
                     recodes = "NA=NA;else=11")
  q1l <- bbw::recode(var = df[["1L"]],
                     recodes = "NA=NA;else=12")
  q1m <- bbw::recode(var = df[["1M"]],
                     recodes = "NA=NA;else=13")
  q1n <- bbw::recode(var = df[["1N"]],
                     recodes = "NA=NA;else=2")
  q1 <- rowSums(cbind(q1a, q1b, q1c, q1d,
                      q1e, q1f, q1g, q1h,
                      q1i, q1j, q1k, q1l,
                      q1m, q1n), na.rm = TRUE)
  ## q2: Household members
  q2a <- bbw::recode(var = df[["2A"]],
                     recodes = "NA=NA;else=1")
  q2b <- bbw::recode(var = df[["2B"]],
                     recodes = "NA=NA;else=2")
  q2 <- rowSums(cbind(q2a, q2b), na.rm = TRUE)
  ## q3: Highest educational level?
  q3a <- bbw::recode(var = df[["3A"]],
                     recodes = "NA=NA;else=1")
  q3b <- bbw::recode(var = df[["3B"]],
                     recodes = "NA=NA;else=2")
  q3c <- bbw::recode(var = df[["3C"]],
                     recodes = "NA=NA;else=3")
  q3d <- bbw::recode(var = df[["3D"]],
                     recodes = "NA=NA;else=4")
  q3 <- rowSums(cbind(q3a, q3b, q3c, q3d), na.rm = TRUE)
  ## q4: children 6-16 attend school?
  q4a <- bbw::recode(var = df[["4A"]],
                     recodes = "NA=NA;else=1")
  q4b <- bbw::recode(var = df[["4B"]],
                     recodes = "NA=NA;else=2")
  q4c <- bbw::recode(var = df[["4C"]],
                     recodes = "NA=NA;else=3")
  q4 <- rowSums(cbind(q4a, q4b, q4c), na.rm = TRUE)
  ## q5: Mode of water supply?
  q5a <- bbw::recode(var = df[["5A"]],
                     recodes = "NA=NA;else=1")
  q5b <- bbw::recode(var = df[["5B"]],
                     recodes = "NA=NA;else=2")
  q5c <- bbw::recode(var = df[["5C"]],
                     recodes = "NA=NA;else=3")
  q5d <- bbw::recode(var = df[["5D"]],
                     recodes = "NA=NA;else=4")
  q5e <- bbw::recode(var = df[["5E"]],
                     recodes = "NA=NA;else=5")
  q5f <- bbw::recode(var = df[["5F"]],
                     recodes = "NA=NA;else=6")
  q5g <- bbw::recode(var = df[["5G"]],
                     recodes = "NA=NA;else=7")
  q5 <- rowSums(cbind(q5a, q5b, q5c, q5d, q5e, q5f, q5g), na.rm = TRUE)
  ## q6: type of toilet?
  q6a <- bbw::recode(var = df[["6A"]],
                     recodes = "NA=NA;else=1")
  q6b <- bbw::recode(var = df[["6B"]],
                     recodes = "NA=NA;else=2")
  q6c <- bbw::recode(var = df[["6C"]],
                     recodes = "NA=NA;else=3")
  q6d <- bbw::recode(var = df[["6D"]],
                     recodes = "NA=NA;else=4")
  q6e <- bbw::recode(var = df[["6E"]],
                     recodes = "NA=NA;else=5")
  q6 <- rowSums(cbind(q6a, q6b, q6c, q6d, q6e), na.rm = TRUE)
  ## q7: where do you take your shower?
  q7a <- bbw::recode(var = df[["7A"]],
                     recodes = "NA=NA;else=1")
  q7b <- bbw::recode(var = df[["7B"]],
                     recodes = "NA=NA;else=2")
  q7c <- bbw::recode(var = df[["7C"]],
                     recodes = "NA=NA;else=3")
  q7d <- bbw::recode(var = df[["7D"]],
                     recodes = "NA=NA;else=4")
  q7 <- rowSums(cbind(q7a, q7b, q7c, q7d), na.rm = TRUE)
  ## q8: Onw moped, car or van?
  q8a <- bbw::recode(var = df[["8A"]],
                     recodes = "NA=NA;else=1")
  q8b <- bbw::recode(var = df[["8B"]],
                     recodes = "NA=NA;else=2")
  q8c <- bbw::recode(var = df[["8C"]],
                     recodes = "NA=NA;else=3")
  q8 <- rowSums(cbind(q8a, q8b, q8c), na.rm = TRUE)
  ## q9: own a fan?
  q9 <- ifelse(!is.na(df[["9A"]]), 1, 0)
  ## q10: own a bed?
  q10 <- ifelse(!is.na(df[["10A"]]), 1, 0)
  ## Create ppiDF object
  ppiDF <- data.frame(hhid = df$HHID, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10)
  ## National poverty line
  if("nl100" %in% category) {
    ## District where household resides?
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=7;2=5;3=9;4=4;5=0;6=3;7=3;8=2;9=5;10=0;11=2;12=2;13=4;14=4")
    ## Number of household members?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=17;2=0")
    ## Highest educational leve?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=0;2=4;3=5;4=12")
    ## Children 6-16 attend school?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=11;2=7;3=0")
    ## Mode of water supply?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=10;2=4;3=4;4=1;5=2;6=2;7=0")
    ## Type of toilet?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=7;2=6;3=5;4=5;5=0")
    ## Where shower is taken?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=0;2=3;3=9;4=1")
    ## Moped or care or van?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=15;2=9;3=0")
    ## fan?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=6;0=0")
    ## bed
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=4;2=0")
    ## Add score to scoreDF
    scoreDF[ , "nl100_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ## Add likelihood to scoreDF
    scoreDF[ , "nl100_likelihood"] <- ppitables::ppiCIV2018[scoreDF[ , "nl100_score"] + 1, "nl100"]
  }
  ## National poverty line - 150%
  if("nl150" %in% category) {
    ## District where household resides?
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=7;2=6;3=6;4=3;5=0;6=3;7=2;8=3;9=5;10=0;11=0;12=1;13=2;14=3")
    ## Number of household members?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=19;2=0")
    ## Highest educational leve?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=0;2=4;3=5;4=11")
    ## Children 6-16 attend school?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=13;2=7;3=0")
    ## Mode of water supply?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=10;2=4;3=2;4=2;5=2;6=2;7=0")
    ## Type of toilet?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=8;2=7;3=5;4=6;5=0")
    ## Where shower is taken?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=0;2=3;3=9;4=2")
    ## Moped or care or van?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=14;2=7;3=0")
    ## fan?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=6;0=0")
    ## bed
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=4;2=0")
    ## Add score to scoreDF
    scoreDF[ , "nl150_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ## Add likelihood to scoreDF
    scoreDF[ , "nl150_likelihood"] <- ppitables::ppiCIV2018[scoreDF[ , "nl150_score"] + 1, "nl150"]
  }
  ## National poverty line - 200%
  if("nl200" %in% category) {
    ## District where household resides?
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=6;2=5;3=7;4=3;5=0;6=4;7=2;8=4;9=7;10=1;11=0;12=1;13=2;14=3")
    ## Number of household members?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=18;2=0")
    ## Highest educational leve?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=0;2=4;3=5;4=10")
    ## Children 6-16 attend school?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=13;2=6;3=0")
    ## Mode of water supply?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=10;2=5;3=2;4=3;5=2;6=3;7=0")
    ## Type of toilet?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=8;2=7;3=6;4=6;5=0")
    ## Where shower is taken?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=0;2=3;3=6;4=1")
    ## Moped or care or van?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=17;2=8;3=0")
    ## fan?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=6;0=0")
    ## bed
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=5;2=0")
    ##
    scoreDF[ , "nl200_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ## Add likelihood to scoreDF
    scoreDF[ , "nl200_likelihood"] <- ppitables::ppiCIV2018[scoreDF[ , "nl200_score"] + 1, "nl200"]
  }
  ## 2011 Purchasing power parity $1.00/day
  if("ppp100" %in% category) {
    ## District where household resides?
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=5;2=6;3=6;4=6;5=0;6=0;7=11;8=0;9=3;10=3;11=3;12=4;13=3;14=8")
    ## Number of household members?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=13;2=0")
    ## Highest educational leve?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=0;2=5;3=7;4=14")
    ## Children 6-16 attend school?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=9;2=7;3=0")
    ## Mode of water supply?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=9;2=8;3=10;4=3;5=3;6=2;7=0")
    ## Type of toilet?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=8;2=6;3=4;4=6;5=0")
    ## Where shower is taken?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=0;2=7;3=13;4=5")
    ## Moped or care or van?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=3;2=10;3=0")
    ## fan?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=6;0=0")
    ## bed
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=7;2=0")
    ##
    scoreDF[ , "ppp190_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ## Add likelihood to scoreDF
    scoreDF[ , "ppp190_likelihood"] <- ppitables::ppiCIV2018[scoreDF[ , "ppp190_score"] + 1, "ppp190"]
  }
  ## 2011 Purchasing power parity $1.90/day
  if("ppp190" %in% category) {
    ## District where household resides?
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=7;2=6;3=10;4=3;5=0;6=2;7=6;8=0;9=5;10=0;11=2;12=5;13=5;14=5")
    ## Number of household members?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=18;2=0")
    ## Highest educational leve?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=0;2=4;3=5;4=11")
    ## Children 6-16 attend school?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=11;2=8;3=0")
    ## Mode of water supply?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=9;2=4;3=6;4=1;5=1;6=1;7=0")
    ## Type of toilet?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=9;2=7;3=5;4=6;5=0")
    ## Where shower is taken?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=0;2=5;3=10;4=2")
    ## Moped or care or van?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=9;2=11;3=0")
    ## fan?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=7;0=0")
    ## bed
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=4;2=0")
    ##
    scoreDF[ , "ppp190_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ## Add likelihood to scoreDF
    scoreDF[ , "ppp190_likelihood"] <- ppitables::ppiCIV2018[scoreDF[ , "ppp190_score"] + 1, "ppp190"]
  }
  ## 2011 Purchasing power parity $3.20
  if("ppp320" %in% category) {
    ## District where household resides?
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=7;2=5;3=9;4=5;5=0;6=3;7=3;8=3;9=5;10=0;11=1;12=3;13=3;14=3")
    ## Number of household members?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=17;2=0")
    ## Highest educational leve?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=0;2=5;3=5;4=11")
    ## Children 6-16 attend school?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=11;2=7;3=0")
    ## Mode of water supply?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=10;2=4;3=4;4=2;5=3;6=3;7=0")
    ## Type of toilet?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=7;2=6;3=5;4=5;5=0")
    ## Where shower is taken?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=0;2=3;3=8;4=2")
    ## Moped or care or van?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=16;2=8;3=0")
    ## fan?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=6;0=0")
    ## bed
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=4;2=0")
    ##
    scoreDF[ , "ppp320_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ## Add likelihood to scoreDF
    scoreDF[ , "ppp320_likelihood"] <- ppitables::ppiCIV2018[scoreDF[ , "ppp320_score"] + 1, "ppp320"]
  }
  ## 2011 Purchasing power parity $5.50
  if("ppp550" %in% category) {
    ## District where household resides?
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=7;2=6;3=7;4=3;5=0;6=3;7=2;8=4;9=6;10=1;11=0;12=1;13=2;14=3")
    ## Number of household members?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=19;2=0")
    ## Highest educational leve?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=0;2=4;3=5;4=10")
    ## Children 6-16 attend school?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=13;2=6;3=0")
    ## Mode of water supply?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=9;2=4;3=1;4=2;5=2;6=3;7=0")
    ## Type of toilet?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=9;2=8;3=6;4=6;5=0")
    ## Where shower is taken?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=0;2=3;3=7;4=1")
    ## Moped or care or van?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=16;2=7;3=0")
    ## fan?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=6;0=0")
    ## bed
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=4;2=0")
    ##
    scoreDF[ , "ppp550_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ## Add likelihood to scoreDF
    scoreDF[ , "ppp550_likelihood"] <- ppitables::ppiCIV2018[scoreDF[ , "ppp550_score"] + 1, "ppp550"]
  }
  ## 2005 Purchasing power parity $1.25
  if("ppp125" %in% category) {
    ## District where household resides?
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=9;2=6;3=10;4=4;5=0;6=2;7=5;8=1;9=4;10=0;11=1;12=4;13=5;14=4")
    ## Number of household members?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=18;2=0")
    ## Highest educational leve?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=0;2=4;3=4;4=12")
    ## Children 6-16 attend school?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=10;2=8;3=0")
    ## Mode of water supply?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=9;2=3;3=5;4=1;5=2;6=1;7=0")
    ## Type of toilet?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=7;2=6;3=5;4=5;5=0")
    ## Where shower is taken?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=0;2=4;3=10;4=3")
    ## Moped or care or van?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=11;2=10;3=0")
    ## fan?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=8;0=0")
    ## bed
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=4;2=0")
    ##
    scoreDF[ , "ppp125_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ##
    scoreDF[ , "ppp125_likelihood"] <- ppitables::ppiCIV2018[scoreDF[ , "ppp125_score"] + 1, "ppp125"]
  }
  ## 2005 Purchasing power parity $2.50
  if("ppp250" %in% category) {
    ## District where household resides?
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=7;2=6;3=7;4=3;5=0;6=4;7=3;8=3;9=5;10=0;11=0;12=2;13=2;14=3")
    ## Number of household members?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=19;2=0")
    ## Highest educational leve?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=0;2=4;3=5;4=11")
    ## Children 6-16 attend school?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=13;2=7;3=0")
    ## Mode of water supply?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=9;2=4;3=3;4=2;5=2;6=2;7=0")
    ## Type of toilet?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=8;2=7;3=6;4=7;5=0")
    ## Where shower is taken?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=0;2=3;3=9;4=2")
    ## Moped or care or van?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=14;2=8;3=0")
    ## fan?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=6;0=0")
    ## bed
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=4;2=0")
    ##
    scoreDF[ , "ppp250_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ##
    scoreDF[ , "ppp250_likelihood"] <- ppitables::ppiCIV2018[scoreDF[ , "ppp250_score"] + 1, "ppp250"]
  }
  ## 2005 Purchasing power parity $5.00
  if("ppp500" %in% category) {
    ## District where household resides?
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=6;2=3;3=6;4=3;5=0;6=3;7=2;8=5;9=7;10=1;11=0;12=2;13=0;14=2")
    ## Number of household members?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=20;2=0")
    ## Highest educational leve?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=0;2=3;3=5;4=10")
    ## Children 6-16 attend school?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=12;2=3;3=0")
    ## Mode of water supply?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=8;2=4;3=0;4=3;5=2;6=1;7=0")
    ## Type of toilet?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=8;2=5;3=4;4=5;5=0")
    ## Where shower is taken?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=0;2=3;3=5;4=1")
    ## Moped or care or van?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=21;2=5;3=0")
    ## fan?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=6;0=0")
    ## bed
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=4;2=0")
    ##
    scoreDF[ , "ppp500_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ##
    scoreDF[ , "ppp500_likelihood"] <- ppitables::ppiCIV2018[scoreDF[ , "ppp500_score"] + 1, "ppp500"]
  }
  ## Bottom 20th percentile
  if("percentile20" %in% category) {
    ## District where household resides?
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=9;2=6;3=10;4=4;5=0;6=2;7=5;8=1;9=4;10=0;11=1;12=4;13=5;14=4")
    ## Number of household members?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=18;2=0")
    ## Highest educational leve?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=0;2=4;3=4;4=12")
    ## Children 6-16 attend school?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=10;2=7;3=0")
    ## Mode of water supply?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=9;2=3;3=4;4=1;5=1;6=1;7=0")
    ## Type of toilet?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=8;2=6;3=5;4=5;5=0")
    ## Where shower is taken?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=0;2=4;3=10;4=3")
    ## Moped or care or van?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=11;2=10;3=0")
    ## fan?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=7;0=0")
    ## bed
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=3;2=0")
    ##
    scoreDF[ , "percentile20_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ##
    scoreDF[ , "percentile20_likelihood"] <- ppitables::ppiCIV2018[scoreDF[ , "percentile20_score"] + 1, "percentile20"]
  }
  ## Bottom 40th percentile
  if("percentile40" %in% category) {
    ## District where household resides?
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=8;2=5;3=9;4=5;5=0;6=4;7=3;8=3;9=5;10=0;11=2;12=2;13=3;14=4")
    ## Number of household members?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=18;2=0")
    ## Highest educational leve?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=0;2=5;3=5;4=12")
    ## Children 6-16 attend school?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=11;2=6;3=0")
    ## Mode of water supply?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=10;2=4;3=4;4=2;5=3;6=3;7=0")
    ## Type of toilet?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=7;2=6;3=5;4=5;5=0")
    ## Where shower is taken?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=0;2=2;3=8;4=1")
    ## Moped or care or van?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=15;2=8;3=0")
    ## fan?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=6;0=0")
    ## bed
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=4;2=0")
    ##
    scoreDF[ , "percentile40_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ##
    scoreDF[ , "percentile40_likelihood"] <- ppitables::ppiCIV2018[scoreDF[ , "percentile40_score"] + 1, "percentile40"]
  }
  ## Bottom 60th percentile
  if("percentile60" %in% category) {
    ## District where household resides?
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=7;2=6;3=7;4=3;5=0;6=3;7=2;8=3;9=5;10=0;11=0;12=2;13=2;14=3")
    ## Number of household members?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=19;2=0")
    ## Highest educational leve?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=0;2=4;3=5;4=11")
    ## Children 6-16 attend school?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=13;2=7;3=0")
    ## Mode of water supply?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=10;2=4;3=3;4=2;5=2;6=3;7=0")
    ## Type of toilet?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=8;2=7;3=5;4=6;5=0")
    ## Where shower is taken?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=0;2=3;3=9;4=2")
    ## Moped or care or van?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=15;2=7;3=0")
    ## fan?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=6;0=0")
    ## bed
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=4;2=0")
    ##
    scoreDF[ , "percentile60_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ##
    scoreDF[ , "percentile60_likelihood"] <- ppitables::ppiCIV2018[scoreDF[ , "percentile60_score"] + 1, "percentile60"]
  }
  ## Bottom 80th percentile
  if("percentile80" %in% category) {
    ## District where household resides?
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=6;2=5;3=7;4=3;5=0;6=4;7=3;8=5;9=7;10=1;11=0;12=2;13=1;14=4")
    ## Number of household members?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=19;2=0")
    ## Highest educational leve?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=0;2=4;3=6;4=11")
    ## Children 6-16 attend school?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=12;2=4;3=0")
    ## Mode of water supply?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=8;2=4;3=1;4=3;5=2;6=3;7=0")
    ## Type of toilet?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=9;2=6;3=5;4=6;5=0")
    ## Where shower is taken?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=0;2=3;3=5;4=1")
    ## Moped or care or van?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=19;2=6;3=0")
    ## fan?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=6;0=0")
    ## bed
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=5;2=0")
    ##
    scoreDF[ , "percentile80_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ##
    scoreDF[ , "percentile80_likelihood"] <- ppitables::ppiCIV2018[scoreDF[ , "percentile80_score"] + 1, "percentile80"]
  }
  ##
  return(scoreDF)
}

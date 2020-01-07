################################################################################
#
#' Get Colombia PPI data from IPA's data analysis and collection tool
#'
#' @param path Directory path to IPA's Colombia PPI data analysis and
#'   collection XLSX file
#'
#' @return A dataframe of household data based on IPA's Colombia PPI data
#'   analysis and collection tool
#'
#' @examples
#' \dontrun{
#'   get_data_col(path = "colombia.xlsx)
#' }
#'
#' @export
#'
#
################################################################################

get_data_col <- function(path) {
  df <- openxlsx::read.xlsx(xlsxFile = path,
                            sheet = "Data",
                            startRow = 8,
                            cols = 1:27)
  ## Return data.frame
  return(df)
}


################################################################################
#
#' Function to calculate Poverty Probability Index (PPI) for Colombia using
#' IPA's new PPI methodology
#'
#' @param path Directory path to IPA's Colombia PPI data analysis and
#'   collection XLSX file
#' @param category Poverty classifications that can be calculated for
#'   Colombia using IPA's new PPI methodology. By default, all 18 poverty
#'   classifications for Colombia are specified (nl100)
#'
#' @return A dataframe of PPI for each household in \code{df}.
#'
#' @examples
#' \dontrun{
#'   ppi_col_ipa(df = get_data_col(path = "colombia.xlsx"))
#' }
#'
#' @export
#'
#'
#
################################################################################

ppi_col_ipa <- function(path,
                        category = names(ppitables::ppiCOL2018)[2:ncol(ppitables::ppiCOL2018)]) {

  df <- get_data_col(path = path)

  scoreDF <- data.frame(matrix(data = NA, nrow = nrow(df), ncol = length(category) * 2))
  names(scoreDF) <- c(paste(category, "score", sep = "_"),
                      paste(category, "likelihood", sep = "_"))
  ## Recode data
  ## q1: Household members
  q1a <- bbw::recode(var = df[["1A"]],
                     recodes = "NA=NA;else=1")
  q1b <- bbw::recode(var = df[["1B"]],
                     recodes = "NA=NA;else=2")
  q1c <- bbw::recode(var = df[["1C"]],
                     recodes = "NA=NA;else=3")
  q1d <- bbw::recode(var = df[["1D"]],
                     recodes = "NA=NA;else=4")
  q1 <- rowSums(cbind(q1a, q1b, q1c, q1d), na.rm = TRUE)
  ## q2: Children age 6-12 attend preschool or school?
  q2a <- bbw::recode(var = df[["2A"]],
                     recodes = "NA=NA;else=1")
  q2b <- bbw::recode(var = df[["2B"]],
                     recodes = "NA=NA;else=2")
  q2c <- bbw::recode(var = df[["2C"]],
                     recodes = "NA=NA;else=3")
  q2 <- rowSums(cbind(q2a, q2b, q2c), na.rm = TRUE)
  ## q3: Reached higher education?
  q3 <- ifelse(!is.na(df[["3A"]]), 1, 0)
  ## q4: Job with a written contract?
  q4 <- ifelse(!is.na(df[["4A"]]), 1, 0)
  ## q5: Worker/empolyee of a private company?
  q5 <- ifelse(!is.na(df[["5A"]]), 1, 0)
  ## q6: A boss or employer?
  q6 <- ifelse(!is.na(df[["6A"]]), 1, 0)
  ## q7: Clothes washing machine?
  q7 <- ifelse(!is.na(df[["7A"]]), 1, 0)
  ## q8: Microwave oven?
  q8 <- ifelse(!is.na(df[["8A"]]), 1, 0)
  ## q9: Personal car?
  q9 <- ifelse(!is.na(df[["9A"]]), 1, 0)
  ## q10: Predominant material of floor of house?
  q10a <- bbw::recode(var = df[["10A"]],
                      recodes = "NA=NA;else=1")
  q10b <- bbw::recode(var = df[["10B"]],
                      recodes = "NA=NA;else=2")
  q10c <- bbw::recode(var = df[["10C"]],
                      recodes = "NA=NA;else=3")
  q10d <- bbw::recode(var = df[["10D"]],
                      recodes = "NA=NA;else=4")
  q10e <- bbw::recode(var = df[["10E"]],
                      recodes = "NA=NA;else=5")
  q10 <- rowSums(cbind(q10a, q10b, q10c, q10d, q10e), na.rm = TRUE)
  ## Create ppiDF object
  ppiDF <- data.frame(hhid = df$HHID, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10)
  ## National poverty line
  if("nl100" %in% category) {
    ## Household members
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=19;2=11;3=6;4=0")
    ## Children age 6-12 attend preschool or school?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=0;2=8;3=3")
    ## Reached higher education?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=6;0=0")
    ## Job with a written contract?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=14;0=0")
    ## Worker/employee of private company?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=9;0=0")
    ## A boss or employer?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=13;0=0")
    ## Clothes washing machine?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=7;0=0")
    ## Microwave oven?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=9;0=0")
    ## Personal car?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=10;0=0")
    ## Floor material
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=0;2=1;3=0;4=4;5=3")
    ## Add score to scoreDF
    scoreDF[ , "nl100_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ## Add likelihood to scoreDF
    scoreDF[ , "nl100_likelihood"] <- ppitables::ppiCOL2018[scoreDF[ , "nl100_score"] + 1, "nl100"]
  }
  ## National food poverty line
  if("extreme" %in% category) {
    ## Household members
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=12;2=9;3=6;4=0")
    ## Children age 6-12 attend preschool or school?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=0;2=6;3=3")
    ## Reached higher education?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=6;0=0")
    ## Job with a written contract?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=17;0=0")
    ## Worker/employee of private company?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=14;0=0")
    ## A boss or employer?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=14;0=0")
    ## Clothes washing machine?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=8;0=0")
    ## Microwave oven?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=7;0=0")
    ## Personal car?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=8;0=0")
    ## Floor material
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=0;2=4;3=0;4=7;5=0")
    ## Add score to scoreDF
    scoreDF[ , "extreme_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ## Add likelihood to scoreDF
    scoreDF[ , "extreme_likelihood"] <- ppitables::ppiCOL2018[scoreDF[ , "extreme_score"] + 1, "extreme"]
  }
  ## National poverty line - 150%
  if("nl150" %in% category) {
    ## Household members
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=23;2=10;3=3;4=0")
    ## Children age 6-12 attend preschool or school?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=0;2=9;3=1")
    ## Reached higher education?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=6;0=0")
    ## Job with a written contract?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=14;0=0")
    ## Worker/employee of private company?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=5;0=0")
    ## A boss or employer?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=13;0=0")
    ## Clothes washing machine?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=5;0=0")
    ## Microwave oven?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=10;0=0")
    ## Personal car?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=10;0=0")
    ## Floor material
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=0;2=0;3=0;4=6;5=7")
    ## Add score to scoreDF
    scoreDF[ , "nl150_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ## Add likelihood to scoreDF
    scoreDF[ , "nl150_likelihood"] <- ppitables::ppiCOL2018[scoreDF[ , "nl150_score"] + 1, "nl150"]
  }
  ## National poverty line - 200%
  if("nl200" %in% category) {
    ## Household members
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=21;2=10;3=4;4=0")
    ## Children age 6-12 attend preschool or school?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=0;2=9;3=3")
    ## Reached higher education?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=8;0=0")
    ## Job with a written contract?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=13;0=0")
    ## Worker/employee of private company?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=3;0=0")
    ## A boss or employer?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=12;0=0")
    ## Clothes washing machine?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=7;0=0")
    ## Microwave oven?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=9;0=0")
    ## Personal car?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=11;0=0")
    ## Floor material
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=0;2=1;3=0;4=5;5=7")
    ##
    scoreDF[ , "nl200_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ## Add likelihood to scoreDF
    scoreDF[ , "nl200_likelihood"] <- ppitables::ppiCOL2018[scoreDF[ , "nl200_score"] + 1, "nl200"]
  }
  ## 2011 Purchasing power parity $1.90/day
  if("ppp190" %in% category) {
    ## Household members
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=9;2=7;3=7;4=0")
    ## Children age 6-12 attend preschool or school?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=0;2=7;3=5")
    ## Reached higher education?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=6;0=0")
    ## Job with a written contract?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=16;0=0")
    ## Worker/employee of private company?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=15;0=0")
    ## A boss or employer?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=13;0=0")
    ## Clothes washing machine?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=10;0=0")
    ## Microwave oven?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=6;0=0")
    ## Personal car?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=8;0=0")
    ## Floor material
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=0;2=6;3=3;4=9;5=0")
    ##
    scoreDF[ , "ppp190_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ## Add likelihood to scoreDF
    scoreDF[ , "ppp190_likelihood"] <- ppitables::ppiCOL2018[scoreDF[ , "ppp190_score"] + 1, "ppp190"]
  }
  ## 2011 Purchasing power parity $3.20
  if("ppp320" %in% category) {
    ## Household members
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=14;2=9;3=5;4=0")
    ## Children age 6-12 attend preschool or school?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=0;2=5;3=1")
    ## Reached higher education?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=6;0=0")
    ## Job with a written contract?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=16;0=0")
    ## Worker/employee of private company?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=14;0=0")
    ## A boss or employer?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=15;0=0")
    ## Clothes washing machine?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=8;0=0")
    ## Microwave oven?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=7;0=0")
    ## Personal car?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=5;0=0")
    ## Floor material
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=0;2=5;3=0;4=10;5=2")
    ##
    scoreDF[ , "ppp320_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ## Add likelihood to scoreDF
    scoreDF[ , "ppp320_likelihood"] <- ppitables::ppiCOL2018[scoreDF[ , "ppp320_score"] + 1, "ppp320"]
  }
  ## 2011 Purchasing power parity $5.50
  if("ppp550" %in% category) {
    ## Household members
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=20;2=10;3=3;4=0")
    ## Children age 6-12 attend preschool or school?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=0;2=7;3=1")
    ## Reached higher education?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=6;0=0")
    ## Job with a written contract?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=15;0=0")
    ## Worker/employee of private company?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=9;0=0")
    ## A boss or employer?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=15;0=0")
    ## Clothes washing machine?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=6;0=0")
    ## Microwave oven?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=8;0=0")
    ## Personal car?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=5;0=0")
    ## Floor material
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=0;2=2;3=0;4=9;5=6")
    ##
    scoreDF[ , "ppp550_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ## Add likelihood to scoreDF
    scoreDF[ , "ppp550_likelihood"] <- ppitables::ppiCOL2018[scoreDF[ , "ppp550_score"] + 1, "ppp550"]
  }
  ## 2011 Purchasing power parity $8.00
  if("ppp800" %in% category) {
    ## Household members
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=22;2=11;3=6;4=0")
    ## Children age 6-12 attend preschool or school?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=0;2=6;3=0")
    ## Reached higher education?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=6;0=0")
    ## Job with a written contract?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=13;0=0")
    ## Worker/employee of private company?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=7;0=0")
    ## A boss or employer?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=9;0=0")
    ## Clothes washing machine?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=7;0=0")
    ## Microwave oven?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=10;0=0")
    ## Personal car?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=8;0=0")
    ## Floor material
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=0;2=2;3=3;4=9;5=12")
    ##
    scoreDF[ , "ppp800_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ## Add likelihood to scoreDF
    scoreDF[ , "ppp800_likelihood"] <- ppitables::ppiCOL2018[scoreDF[ , "ppp800_score"] + 1, "ppp800"]
  }
  ## 2011 Purchasing power parity $11.00
  if("ppp1100" %in% category) {
    ## Household members
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=23;2=10;3=2;4=0")
    ## Children age 6-12 attend preschool or school?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=0;2=11;3=4")
    ## Reached higher education?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=7;0=0")
    ## Job with a written contract?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=12;0=0")
    ## Worker/employee of private company?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=3;0=0")
    ## A boss or employer?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=9;0=0")
    ## Clothes washing machine?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=5;0=0")
    ## Microwave oven?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=7;0=0")
    ## Personal car?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=8;0=0")
    ## Floor material
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=0;2=0;3=0;4=8;5=15")
    ##
    scoreDF[ , "ppp1100_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ## Add likelihood to scoreDF
    scoreDF[ , "ppp1100_likelihood"] <- ppitables::ppiCOL2018[scoreDF[ , "ppp1100_score"] + 1, "ppp1100"]
  }
  ## 2011 Purchasing power parity $15.00
  if("ppp1500" %in% category) {
    ## Household members
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=23;2=12;3=5;4=0")
    ## Children age 6-12 attend preschool or school?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=0;2=14;3=9")
    ## Reached higher education?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=8;0=0")
    ## Job with a written contract?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=10;0=0")
    ## Worker/employee of private company?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=0;0=0")
    ## A boss or employer?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=9;0=0")
    ## Clothes washing machine?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=6;0=0")
    ## Microwave oven?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=5;0=0")
    ## Personal car?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=9;0=0")
    ## Floor material
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=0;2=2;3=1;4=10;5=15")
    ##
    scoreDF[ , "ppp1500_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ##
    scoreDF[ , "ppp1500_likelihood"] <- ppitables::ppiCOL2018[scoreDF[ , "ppp1500_score"] + 1, "ppp1500"]
  }
  ## 2011 Purchasing power parity $21.70
  if("ppp2170" %in% category) {
    ## Household members
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=17;2=9;3=5;4=0")
    ## Children age 6-12 attend preschool or school?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=0;2=14;3=11")
    ## Reached higher education?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=8;0=0")
    ## Job with a written contract?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=9;0=0")
    ## Worker/employee of private company?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=0;0=0")
    ## A boss or employer?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=7;0=0")
    ## Clothes washing machine?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=5;0=0")
    ## Microwave oven?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=7;0=0")
    ## Personal car?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=9;0=0")
    ## Floor material
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=0;2=12;3=12;4=17;5=23")
    ##
    scoreDF[ , "ppp2170_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ##
    scoreDF[ , "ppp2170_likelihood"] <- ppitables::ppiCOL2018[scoreDF[ , "ppp2170_score"] + 1, "ppp2170"]
  }
  ## 2005 Purchasing power parity $1.25
  if("ppp125" %in% category) {
    ## Household members
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=9;2=8;3=8;4=0")
    ## Children age 6-12 attend preschool or school?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=0;2=9;3=8")
    ## Reached higher education?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=7;0=0")
    ## Job with a written contract?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=15;0=0")
    ## Worker/employee of private company?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=14;0=0")
    ## A boss or employer?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=11;0=0")
    ## Clothes washing machine?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=11;0=0")
    ## Microwave oven?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=7;0=0")
    ## Personal car?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=8;0=0")
    ## Floor material
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=0;2=6;3=5;4=7;5=0")
    ##
    scoreDF[ , "ppp125_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ##
    scoreDF[ , "ppp125_likelihood"] <- ppitables::ppiCOL2018[scoreDF[ , "ppp125_score"] + 1, "ppp125"]
  }
  ## 2005 Purchasing power parity $2.50
  if("ppp250" %in% category) {
    ## Household members
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=14;2=9;3=5;4=0")
    ## Children age 6-12 attend preschool or school?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=0;2=6;3=2")
    ## Reached higher education?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=6;0=0")
    ## Job with a written contract?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=16;0=0")
    ## Worker/employee of private company?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=13;0=0")
    ## A boss or employer?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=15;0=0")
    ## Clothes washing machine?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=8;0=0")
    ## Microwave oven?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=7;0=0")
    ## Personal car?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=4;0=0")
    ## Floor material
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=0;2=5;3=0;4=10;5=2")
    ##
    scoreDF[ , "ppp250_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ##
    scoreDF[ , "ppp250_likelihood"] <- ppitables::ppiCOL2018[scoreDF[ , "ppp250_score"] + 1, "ppp250"]
  }
  ## 2005 Purchasing power parity $5.00
  if("ppp500" %in% category) {
    ## Household members
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=23;2=11;3=5;4=0")
    ## Children age 6-12 attend preschool or school?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=0;2=6;3=0")
    ## Reached higher education?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=6;0=0")
    ## Job with a written contract?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=13;0=0")
    ## Worker/employee of private company?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=9;0=0")
    ## A boss or employer?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=11;0=0")
    ## Clothes washing machine?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=7;0=0")
    ## Microwave oven?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=9;0=0")
    ## Personal car?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=8;0=0")
    ## Floor material
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=0;2=1;3=0;4=7;5=8")
    ##
    scoreDF[ , "ppp500_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ##
    scoreDF[ , "ppp500_likelihood"] <- ppitables::ppiCOL2018[scoreDF[ , "ppp500_score"] + 1, "ppp500"]
  }
  ## Bottom 20th percentile
  if("percentile20" %in% category) {
    ## Household members
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=21;2=10;3=3;4=0")
    ## Children age 6-12 attend preschool or school?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=0;2=8;3=1")
    ## Reached higher education?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=5;0=0")
    ## Job with a written contract?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=17;0=0")
    ## Worker/employee of private company?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=12;0=0")
    ## A boss or employer?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=16;0=0")
    ## Clothes washing machine?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=6;0=0")
    ## Microwave oven?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=4;0=0")
    ## Personal car?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=2;0=0")
    ## Floor material
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=0;2=2;3=0;4=9;5=1")
    ##
    scoreDF[ , "percentile20_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ##
    scoreDF[ , "percentile20_likelihood"] <- ppitables::ppiCOL2018[scoreDF[ , "percentile20_score"] + 1, "percentile20"]
  }
  ## Bottom 40th percentile
  if("percentile40" %in% category) {
    ## Household members
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=22;2=11;3=7;4=0")
    ## Children age 6-12 attend preschool or school?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=0;2=6;3=0")
    ## Reached higher education?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=6;0=0")
    ## Job with a written contract?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=13;0=0")
    ## Worker/employee of private company?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=7;0=0")
    ## A boss or employer?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=10;0=0")
    ## Clothes washing machine?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=7;0=0")
    ## Microwave oven?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=10;0=0")
    ## Personal car?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=8;0=0")
    ## Floor material
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=0;2=1;3=2;4=8;5=12")
    ##
    scoreDF[ , "percentile40_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ##
    scoreDF[ , "percentile40_likelihood"] <- ppitables::ppiCOL2018[scoreDF[ , "percentile40_score"] + 1, "percentile40"]
  }
  ## Bottom 60th percentile
  if("percentile60" %in% category) {
    ## Household members
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=22;2=11;3=4;4=0")
    ## Children age 6-12 attend preschool or school?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=0;2=10;3=4")
    ## Reached higher education?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=9;0=0")
    ## Job with a written contract?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=12;0=0")
    ## Worker/employee of private company?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=3;0=0")
    ## A boss or employer?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=9;0=0")
    ## Clothes washing machine?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=6;0=0")
    ## Microwave oven?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=8;0=0")
    ## Personal car?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=9;0=0")
    ## Floor material
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=0;2=2;3=0;4=9;5=12")
    ##
    scoreDF[ , "percentile60_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ##
    scoreDF[ , "percentile60_likelihood"] <- ppitables::ppiCOL2018[scoreDF[ , "percentile60_score"] + 1, "percentile60"]
  }
  ## Bottom 80th percentile
  if("percentile80" %in% category) {
    ## Household members
    s1 <- bbw::recode(var = ppiDF$q1,
                      recodes = "1=20;2=10;3=6;4=0")
    ## Children age 6-12 attend preschool or school?
    s2 <- bbw::recode(var = ppiDF$q2,
                      recodes = "1=0;2=12;3=7")
    ## Reached higher education?
    s3 <- bbw::recode(var = ppiDF$q3,
                      recodes = "1=8;0=0")
    ## Job with a written contract?
    s4 <- bbw::recode(var = ppiDF$q4, recodes = "1=10;0=0")
    ## Worker/employee of private company?
    s5 <- bbw::recode(var = ppiDF$q5, recodes = "1=0;0=0")
    ## A boss or employer?
    s6 <- bbw::recode(var = ppiDF$q6, recodes = "1=7;0=0")
    ## Clothes washing machine?
    s7 <- bbw::recode(var = ppiDF$q7, recodes = "1=5;0=0")
    ## Microwave oven?
    s8 <- bbw::recode(var = ppiDF$q8, recodes = "1=6;0=0")
    ## Personal car?
    s9 <- bbw::recode(var = ppiDF$q9, recodes = "1=9;0=0")
    ## Floor material
    s10 <- bbw::recode(var = ppiDF$q10, recodes = "1=0;2=9;3=10;4=16;5=22")
    ##
    scoreDF[ , "percentile80_score"] <- rowSums(cbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    ##
    scoreDF[ , "percentile80_likelihood"] <- ppitables::ppiCOL2018[scoreDF[ , "percentile80_score"] + 1, "percentile80"]
  }
  ##
  return(scoreDF)
}

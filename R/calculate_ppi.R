################################################################################
#
#' Function to calculate Poverty Probability Index (PPI) for Kenya
#'
#' @param df Dataframe containing PPI indicator variables for each household.
#' @param category Poverty classifications.
#'
#' @return A dataframe of PPI for each household in \code{df}.
#'
#' @examples
#' #calculate_ppi(df = household)
#'
#' @export
#'
#
################################################################################

calculate_ppi <- function(df,
                          category = c("nl100", "nlFood", "nl150", "nl200",
                                       "ppp100", "ppp190", "ppp320", "ppp550",
                                       "ppp800", "ppp125", "ppp250", "ppp500",
                                       "percentile20", "percentile40",
                                       "percentile60", "percentile80")) {
  scoreDF <- data.frame(matrix(data = NA, nrow = nrow(df), ncol = length(category)))
  names(scoreDF) <- category

  ## National poverty line
  if("nl100" %in% category) {
    ## County
    q1 <- bbw::recode(var = df$cid,
                      recodes = "1=8;2=5;3=7;4=6;5=7;6=6;7=9;8=7;9=3;10=6;11=4;12=2;13=9")
    ## Highest education of female head/spouse
    q2 <- bbw::recode(var = df$hh1,
                      recodes = "1=0;2=7;3=10;4=15;5=16")
    ## Highest education of any household member
    q3 <- bbw::recode(var = df$hh2,
                      recodes = "1=0;2=3;3=1;4=5")
    ## Bread
    q4 <- bbw::recode(var = df$hh3, recodes = "1=10;2=0")
    ## Meat/fish
    q5 <- bbw::recode(var = df$hh4, recodes = "1=12;2=0")
    ## Ripe bananas
    q6 <- bbw::recode(var = df$hh5, recodes = "1=9;2=0")
    ## Towels
    q7 <- bbw::recode(var = df$hh6, recodes = "1=10;2=0")
    ## Thermos flask
    q8 <- bbw::recode(var = df$hh7, recodes = "1=9;2=0")
    ## Wall material
    q9 <- bbw::recode(var = df$hh8, recodes = "1=8;2=6;3=0")
    ## Floor material
    q10 <- bbw::recode(var = df$hh9, recodes = "1=0;2=7")
    ##
    scoreDF[ , "nl100"] <- q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10
  }
  ## National food poverty line
  if("nlFood" %in% category) {
    ## County
    q1 <- bbw::recode(var = df$cid,
                      recodes = "1=7;2=6;3=9;4=6;5=5;6=8;7=6;8=6;9=2;10=6;11=5;12=0;13=11")
    ## Highest education of female head/spouse
    q2 <- bbw::recode(var = df$hh1,
                      recodes = "1=0;2=7;3=9;4=10;5=12")
    ## Highest education of any household member
    q3 <- bbw::recode(var = df$hh2,
                      recodes = "1=0;2=5;3=5;4=7")
    ## Bread
    q4 <- bbw::recode(var = df$hh3, recodes = "1=11;2=0")
    ## Meat/fish
    q5 <- bbw::recode(var = df$hh4, recodes = "1=11;2=0")
    ## Ripe bananas
    q6 <- bbw::recode(var = df$hh5, recodes = "1=11;2=0")
    ## Towels
    q7 <- bbw::recode(var = df$hh6, recodes = "1=10;2=0")
    ## Thermos flask
    q8 <- bbw::recode(var = df$hh7, recodes = "1=9;2=0")
    ## Wall material
    q9 <- bbw::recode(var = df$hh8, recodes = "1=8;2=8;3=0")
    ## Floor material
    q10 <- bbw::recode(var = df$hh9, recodes = "1=0;2=7")
    ##
    scoreDF[ , "nlFood"] <- q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10
  }
  ## National poverty line - 150%
  if("nl150" %in% category) {
    ## County
    q1 <- bbw::recode(var = df$cid,
                      recodes = "1=8;2=5;3=7;4=6;5=7;6=6;7=9;8=7;9=3;10=6;11=4;12=2;13=9")
    ## Highest education of female head/spouse
    q2 <- bbw::recode(var = df$hh1,
                      recodes = "1=0;2=7;3=10;4=13;5=16")
    ## Highest education of any household member
    q3 <- bbw::recode(var = df$hh2,
                      recodes = "1=0;2=3;3=1;4=5")
    ## Bread
    q4 <- bbw::recode(var = df$hh3, recodes = "1=10;2=0")
    ## Meat/fish
    q5 <- bbw::recode(var = df$hh4, recodes = "1=12;2=0")
    ## Ripe bananas
    q6 <- bbw::recode(var = df$hh5, recodes = "1=9;2=0")
    ## Towels
    q7 <- bbw::recode(var = df$hh6, recodes = "1=9=0")
    ## Thermos flask
    q8 <- bbw::recode(var = df$hh7, recodes = "1=9;2=0")
    ## Wall material
    q9 <- bbw::recode(var = df$hh8, recodes = "1=9;2=5;3=0")
    ## Floor material
    q10 <- bbw::recode(var = df$hh9, recodes = "1=0;2=7")
    ##
    scoreDF[ , "nl150"] <- q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10
  }
  ## National poverty line - 150%
  if("nl150" %in% category) {
    ## County
    q1 <- bbw::recode(var = df$cid,
                      recodes = "1=8;2=5;3=7;4=6;5=7;6=6;7=9;8=7;9=3;10=6;11=4;12=2;13=9")
    ## Highest education of female head/spouse
    q2 <- bbw::recode(var = df$hh1,
                      recodes = "1=0;2=7;3=10;4=13;5=16")
    ## Highest education of any household member
    q3 <- bbw::recode(var = df$hh2,
                      recodes = "1=0;2=3;3=1;4=5")
    ## Bread
    q4 <- bbw::recode(var = df$hh3, recodes = "1=10;2=0")
    ## Meat/fish
    q5 <- bbw::recode(var = df$hh4, recodes = "1=12;2=0")
    ## Ripe bananas
    q6 <- bbw::recode(var = df$hh5, recodes = "1=9;2=0")
    ## Towels
    q7 <- bbw::recode(var = df$hh6, recodes = "1=9;2=0")
    ## Thermos flask
    q8 <- bbw::recode(var = df$hh7, recodes = "1=9;2=0")
    ## Wall material
    q9 <- bbw::recode(var = df$hh8, recodes = "1=9;2=5;3=0")
    ## Floor material
    q10 <- bbw::recode(var = df$hh9, recodes = "1=0;2=7")
    ##
    nl150Score <- q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10
  }
  ## National poverty line - 200%
  if("nl200" %in% category) {
    ## County
    q1 <- bbw::recode(var = df$cid,
                      recodes = "1=7;2=6;3=9;4=6;5=5;6=8;7=6;8=6;9=2;10=6;11=5;12=0;13=11")
    ## Highest education of female head/spouse
    q2 <- bbw::recode(var = df$hh1,
                      recodes = "1=0;2=7;3=10;4=13;5=16")
    ## Highest education of any household member
    q3 <- bbw::recode(var = df$hh2,
                      recodes = "1=0;2=3;3=1;4=5")
    ## Bread
    q4 <- bbw::recode(var = df$hh3, recodes = "1=11;2=0")
    ## Meat/fish
    q5 <- bbw::recode(var = df$hh4, recodes = "1=11;2=0")
    ## Ripe bananas
    q6 <- bbw::recode(var = df$hh5, recodes = "1=9;2=0")
    ## Towels
    q7 <- bbw::recode(var = df$hh6, recodes = "1=10;2=0")
    ## Thermos flask
    q8 <- bbw::recode(var = df$hh7, recodes = "1=8;2=0")
    ## Wall material
    q9 <- bbw::recode(var = df$hh8, recodes = "1=9;2=5;3=0")
    ## Floor material
    q10 <- bbw::recode(var = df$hh9, recodes = "1=0;2=7")
    ##
    nl200Score <- q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10
  }
  ## Purchasing power parity $1.00
  if("ppp100" %in% category) {
    ## County
    q1 <- bbw::recode(var = df$cid,
                      recodes = "1=9;2=4;3=7;4=6;5=8;6=7;7=9;8=5;9=2;10=8;11=6;12=7;13=8")
    ## Highest education of female head/spouse
    q2 <- bbw::recode(var = df$hh1,
                      recodes = "1=0;2=4;3=6;4=10;5=13")
    ## Highest education of any household member
    q3 <- bbw::recode(var = df$hh2,
                      recodes = "1=0;2=2;3=4;4=6")
    ## Bread
    q4 <- bbw::recode(var = df$hh3, recodes = "1=10;2=0")
    ## Meat/fish
    q5 <- bbw::recode(var = df$hh4, recodes = "1=10;2=0")
    ## Ripe bananas
    q6 <- bbw::recode(var = df$hh5, recodes = "1=9;2=0")
    ## Towels
    q7 <- bbw::recode(var = df$hh6, recodes = "1=8;2=0")
    ## Thermos flask
    q8 <- bbw::recode(var = df$hh7, recodes = "1=8;2=0")
    ## Wall material
    q9 <- bbw::recode(var = df$hh8, recodes = "1=6;2=7;3=0")
    ## Floor material
    q10 <- bbw::recode(var = df$hh9, recodes = "1=0;2=7")
    ##
    ppp100Score <- q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10
  }
  ## Purchasing power parity $1.90
  if("ppp190" %in% category) {
    ## County
    q1 <- bbw::recode(var = df$cid,
                      recodes = "1=10;2=4;3=5;4=8;5=7;6=8;7=9;8=7;9=4;10=8;11=7;12=9;13=7")
    ## Highest education of female head/spouse
    q2 <- bbw::recode(var = df$hh1,
                      recodes = "1=0;2=5;3=7;4=9;5=16")
    ## Highest education of any household member
    q3 <- bbw::recode(var = df$hh2,
                      recodes = "1=0;2=2;3=2;4=6")
    ## Bread
    q4 <- bbw::recode(var = df$hh3, recodes = "1=10;2=0")
    ## Meat/fish
    q5 <- bbw::recode(var = df$hh4, recodes = "1=11;2=0")
    ## Ripe bananas
    q6 <- bbw::recode(var = df$hh5, recodes = "1=9;2=0")
    ## Towels
    q7 <- bbw::recode(var = df$hh6, recodes = "1=9;2=0")
    ## Thermos flask
    q8 <- bbw::recode(var = df$hh7, recodes = "1=8;2=0")
    ## Wall material
    q9 <- bbw::recode(var = df$hh8, recodes = "1=8;2=7;3=0")
    ## Floor material
    q10 <- bbw::recode(var = df$hh9, recodes = "1=0;2=9")
    ##
    ppp190Score <- q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10
  }
  ## Purchasing power parity $3.20
  if("ppp320" %in% category) {
    ## County
    q1 <- bbw::recode(var = df$cid,
                      recodes = "1=9;2=4;3=13;4=7;5=5;6=10;7=7;8=5;9=0;10=6;11=4;12=0;13=12")
    ## Highest education of female head/spouse
    q2 <- bbw::recode(var = df$hh1,
                      recodes = "1=0;2=6;3=9;4=11;5=18")
    ## Highest education of any household member
    q3 <- bbw::recode(var = df$hh2,
                      recodes = "1=0;2=1;3=0;4=4")
    ## Bread
    q4 <- bbw::recode(var = df$hh3, recodes = "1=9;2=0")
    ## Meat/fish
    q5 <- bbw::recode(var = df$hh4, recodes = "1=11;2=0")
    ## Ripe bananas
    q6 <- bbw::recode(var = df$hh5, recodes = "1=9;2=0")
    ## Towels
    q7 <- bbw::recode(var = df$hh6, recodes = "1=9;2=0")
    ## Thermos flask
    q8 <- bbw::recode(var = df$hh7, recodes = "1=8;2=0")
    ## Wall material
    q9 <- bbw::recode(var = df$hh8, recodes = "1=9;2=6;3=0")
    ## Floor material
    q10 <- bbw::recode(var = df$hh9, recodes = "1=0;2=9")
    ##
    ppp320Score <- q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10
  }
  ## Purchasing power parity $5.50
  if("ppp550" %in% category) {
    ## County
    q1 <- bbw::recode(var = df$cid,
                      recodes = "1=7;2=6;3=9;4=4;5=5;6=5;7=6;8=6;9=1;10=5;11=5;12=3;13=5")
    ## Highest education of female head/spouse
    q2 <- bbw::recode(var = df$hh1,
                      recodes = "1=0;2=6;3=9;4=13;5=19")
    ## Highest education of any household member
    q3 <- bbw::recode(var = df$hh2,
                      recodes = "1=0;2=1;3=0;4=3")
    ## Bread
    q4 <- bbw::recode(var = df$hh3, recodes = "1=10;2=0")
    ## Meat/fish
    q5 <- bbw::recode(var = df$hh4, recodes = "1=10;2=0")
    ## Ripe bananas
    q6 <- bbw::recode(var = df$hh5, recodes = "1=9;2=0")
    ## Towels
    q7 <- bbw::recode(var = df$hh6, recodes = "1=10;2=0")
    ## Thermos flask
    q8 <- bbw::recode(var = df$hh7, recodes = "1=7;2=0")
    ## Wall material
    q9 <- bbw::recode(var = df$hh8, recodes = "1=10;2=6;3=0")
    ## Floor material
    q10 <- bbw::recode(var = df$hh9, recodes = "1=0;2=10")
    ##
    ppp550Score <- q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10
  }
  ## Purchasing power parity $8.00
  if("ppp800" %in% category) {
    ## County
    q1 <- bbw::recode(var = df$cid,
                      recodes = "1=7;2=6;3=9;4=4;5=5;6=5;7=6;8=6;9=1;10=5;11=5;12=3;13=5")
    ## Highest education of female head/spouse
    q2 <- bbw::recode(var = df$hh1,
                      recodes = "1=0;2=6;3=9;4=12;5=17")
    ## Highest education of any household member
    q3 <- bbw::recode(var = df$hh2,
                      recodes = "1=0;2=4;3=4;4=7")
    ## Bread
    q4 <- bbw::recode(var = df$hh3, recodes = "1=9;2=0")
    ## Meat/fish
    q5 <- bbw::recode(var = df$hh4, recodes = "1=10;2=0")
    ## Ripe bananas
    q6 <- bbw::recode(var = df$hh5, recodes = "1=9;2=0")
    ## Towels
    q7 <- bbw::recode(var = df$hh6, recodes = "1=10;2=0")
    ## Thermos flask
    q8 <- bbw::recode(var = df$hh7, recodes = "1=6;2=0")
    ## Wall material
    q9 <- bbw::recode(var = df$hh8, recodes = "1=10;2=6;3=0")
    ## Floor material
    q10 <- bbw::recode(var = df$hh9, recodes = "1=0;2=10")
    ##
    ppp800Score <- q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10
  }

  #ppitables::ppiKEN2018[score + 1, "nl100"]

  #return(score)
}

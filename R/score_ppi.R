################################################################################
#
#' score_ppi
#'
#' Given country-specific survey data with information on the ten questions on
#' which PPI scores are based on, recode the data accordingly and calculate
#' the corresponding PPI score
#'
#' @param ccode Three letter ISO code for a country
#'
#' @return A PPI score ranging between 0 to 100
#'
#' @examples
#' # use score_ppi for Bangladesh
#' \dontrun{score_ppi(ccode = "BGD")}
#'
#' @export
#'
#'
#
################################################################################

score_ppi <- function(ccode) {
  #
  # Check country
  #
  if(ccode == "BGD") {
    #
    # ppi1: Number of household members 12-years old or younger
    #
    q1 <- menu(title = "How many household members are 12-years-old or younger?",
               choices = c("None", "One", "Two", "Three or more"))
    ppi1 <- ifelse(q1 == 1, 32,
              ifelse(q1 == 2, 16,
                ifelse(q1 == 3, 10, 0)))
    #
    # ppi2: Do household members 6-12 years old attend school?
    #
    q2 <- menu(title = "Do all household members ages 6-to-12 currently attend a
                        school/educational institution?",
               choices = c("Yes", "No", "No one 6-12 years old"))
    ppi2 <- ifelse(q2 == 1, 6, 0)
    #
    # ppi3: In past year, any household member do paid work?
    #
    q3 <- menu(title = "In the past year, did any household member ever do work
                        for which he/she was paid on a daily basis?",
               choices = c("Yes", "No"))
    ppi3 <- ifelse(q3 == 2, 8, 0)
    #
    # ppi4: Number of rooms used by household
    #
    q4 <- menu(title = "How many rooms does your household occupy (excluding
                        rooms used for business)?",
               choices = c("One", "Two", "Three or more"))
    ppi4 <- ifelse(q4 == 3, 5,
              ifelse(q4 == 2, 3, 0))
    #
    # ppi5: Main construction material of the walls of the main room
    #
    q5 <- menu(title = "What is the main construction material of the walls of
                        the main room?",
               choices = c("Hemp/hay/bamboo, or other",
                           "Mud brick, or C.I. sheet/wood",
                           "Brick/cement"))
    ppi5 <- ifelse(q5 == 3, 9,
              ifelse(q5 == 2, 2, 0))
    #
    # ppi6: Does the household own television?
    #
    q6 <- menu(title = "Does the household own any televisions?",
               choices = c("Yes", "No"))
    ppi6 <- ifelse(q6 == 1, 7, 0)
    #
    # ppi7: Number of fans the household owns
    #
    q7 <- menu(title = "How many fans does the household own?",
               choices = c("None", "One", "Two or more"))
    ppi7 <- ifelse(q7 == 3, 7,
              ifelse(q7 == 2, 4, 0))
    #
    # ppi8: Number of mobile phones the household owns
    #
    q8 <- menu(title = "How many mobile phones does the household own?",
               choices = c("None", "One", "Two or more"))
    ppi8 <- ifelse(q8 == 3, 15,
              ifelse(q8 == 2, 8, 0))
    #
    # ppi9: Does household own bicycles, motorcycles/scooters, cars?
    #
    q9 <- menu(title = "Does the household own any bicycles, motorcycle/scooters,
                        or motor cars etc.?",
               choices = c("Yes", "No"))
    ppi9 <- ifelse(q9 == 1, 4, 0)
    #
    # ppi10: Does the household own/rent/sharecrop/mortgage in or out 51 or more
    #        decimals of cultivable agricultural land
    #
    q10 <- menu(title = "Does the household own (or rent/sharecrop/mortgage in
                         or out) 51 or more decimals of cultivable agricultural \
                         land (excluding uncultivable land and dwelling-house/homestead
                         land)?",
                choices = c("Yes", "No"))
    ppi10 <- ifelse(q10 == 1, 7, 0)
    #
    # ppi: total score
    #
    ppi <- ppi1 + ppi2 + ppi3 + ppi4 + ppi5 + ppi6 + ppi7 + ppi8 + ppi9 + ppi10
  }
  if(!is.na(ppi)){ return(ppi) }
}

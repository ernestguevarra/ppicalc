################################################################################
#
#' score_ppi_cohort
#'
#'
#'
#'
#'
#'
#'
#' @export
#'
#'
#
################################################################################

score_ppi_cohort <- function(data, ccode) {
  #
  # Check if country is Bangladesh
  #
  if(ccode == "BGD") {
    #
    # ppi1: Number of household members 12-years old or younger
    #
    ppi1 <- ifelse(data$ppi1 == "None", 32,
                   ifelse(data$ppi1 == "One", 16,
                          ifelse(data$ppi1 == "Two", 10, 0)))
    #
    # ppi2: Do household members 6-12 years old attend school?
    #
    ppi2 <- ifelse(data$ppi2 == "Yes", 6, 0)
    #
    # ppi3: In past year, any household member do paid work?
    #
    ppi3 <- ifelse(data$ppi3 == "No", 8, 0)
    #
    # ppi4: Number of rooms used by household
    #
    ppi4 <- ifelse(data$ppi4 == "Three or more", 5,
                   ifelse(data$ppi4 == "Two", 3, 0))
    #
    # ppi5: Main construction material of the walls of the main room
    #
    ppi5 <- ifelse(data$ppi5 == "Brick/cement", 9,
                   ifelse(data$ppi5 == "Mud brick, or C.I. sheet/wood", 2, 0))
    #
    # ppi6: Does the household own television?
    #
    ppi6 <- ifelse(data$ppi6 == "Yes", 7, 0)
    #
    # ppi7: Number of fans the household owns
    #
    ppi7 <- ifelse(data$ppi7 == "Two or more", 7,
                   ifelse(data$ppi7 == "One", 4, 0))
    #
    # ppi8: Number of mobile phones the household owns
    #
    ppi8 <- ifelse(data$ppi8 == "Two or more", 15,
                   ifelse(data$ppi8 == "One", 8, 0))
    #
    # ppi9: Does household own bicycles, motorcycles/scooters, cars?
    #
    ppi9 <- ifelse(data$ppi9 == "Yes", 4, 0)
    #
    # ppi10: Does the household own/rent/sharecrop/mortgage in or out 51 or more
    #        decimals of cultivable agricultural land
    #
    ppi10 <- ifelse(data$ppi10 == "Yes", 7, 0)
    #
    # ppi: total score
    #
    ppi <- ppi1 + ppi2 + ppi3 + ppi4 + ppi5 + ppi6 + ppi7 + ppi8 + ppi9 + ppi10
  }
  return(ppi)
}

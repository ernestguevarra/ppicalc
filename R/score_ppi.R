################################################################################
#
#' score_ppi
#'
#' Given country-specific survey data with information on the ten questions on
#' which PPI scores are based on, recode the data accordingly and calculate
#' the corresponding PPI score
#'
#' @param ccode Three letter ISO code for a country
#' @return A PPI score ranging between 0 to 100
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
  #
  # Check country - Ghana
  #
  if(ccode == "GHA") {
    #
    # ppi1: How many members does the household have?
    #
    q1 <- menu(title = "How many members does the household have?",
               choices = c("Eight or more", "Seven", "Six", "Five", "Four", "Three", "Two", "One"))
    ppi1 <- ifelse(q1 == 1, 0,
              ifelse(q1 == 2, 4,
                ifelse(q1 == 3, 9,
                  ifelse(q1 == 4, 13,
                    ifelse(q1 == 5, 14,
                      ifelse(q1 == 6, 21,
                        ifelse(q1 == 7, 24, 29)))))))
    #
    # ppi2: Are all household members ages 5 to 17 currently in school?
    #
    q2 <- menu(title = "Are all household members ages 5 to 17 currently in school?",
               choices = c("Yes", "No", "No one 5-17 years old"))
    ppi2 <- ifelse(q2 == 1, 2,
              ifelse(q2 == 2, 0, 3))
    #
    # ppi3: Can the male head/spouse read a phrase/sentence in English?
    #
    q3 <- menu(title = "Can the male head/spouse read a phrase/sentence in English?",
               choices = c("Yes", "No", "No male head/spouse"))
    ppi3 <- ifelse(q3 == 1, 5,
              ifelse(q3 == 2, 0, 2))
    #
    # ppi4: What is the main construction material used for the outer wall?
    #
    q4 <- menu(title = "What is the main construction material used for the outer wall?",
               choices = c("Mud bricks/earth, wood, bamboo, metal 0 sheet/slate/asbestos, palm leaves/thatch (grass/raffia), or other",
                           "Cement/concrete blocks, landcrete, stone, or burnt bricks"))
    ppi4 <- ifelse(q4 == 1, 0, 5)
    #
    # ppi5: What type of toilet facility is usually used by the household?
    #
    q5 <- menu(title = "What type of toilet facility is usually used by the household?",
               choices = c("No toilet facility (bush, beach), or other",
                           "Pit latrine, bucket/pan",
                           "Public toilet (e.g., W.C., KVIP, pit pan)",
                           "KVIP, or W.C."))
    ppi5 <- ifelse(q5 == 1, 0,
              ifelse(q5 %in% 2:3, 4, 6))
    #
    # ppi6: What is the main fuel used by the household for cooking?
    #
    q6 <- menu(title = "What is the main fuel used by the household for cooking?",
               choices = c("None, no cooking",
                           "Wood, crop residue, sawdust, animal waste, or other",
                           "Charcoal, or kerosene",
                           "Gas, or electricity"))
    ppi6 <- ifelse(q6 == 1, 0,
              ifelse(q6 == 2, 6,
                ifelse(q6 == 3, 13, 22)))
    #
    # ppi7: Does any household member own a working box iron or electric iron?
    #
    q7 <- menu(title = "Does any household member own a working box iron or electric iron?",
               choices = c("Yes", "No"))
    ppi7 <- ifelse(q7 == 1, 0, 4)
    #
    # ppi8: Does any household member own a working television, video player,
    #       VCD/DVD/MP3/MP4 player/iPod, or satellite dish?
    #
    q8 <- menu(title = "Does any household member own a working television, video
                        player, VCD/DVD/MP3/MP4 player/iPod, or satellite dish?",
               choices = c("No", "Only television",
                           "Video player, VCD/DVD/MP3/MP4 player/iPod, or satellite dish (regardless of T.V.)"))
    ppi8 <- ifelse(q8 == 1, 0,
              ifelse(q8 == 2, 2, 8))
    #
    # ppi9: How many working mobile phones are owned by members of the household?
    #
    q9 <- menu(title = "How many working mobile phones are owned by members of the household?",
               choices = c("None", "One", "Two", "Three or more"))
    ppi9 <- ifelse(q9 == 1, 0,
              ifelse(q9 == 2, 4,
                ifelse(q9 == 3, 8, 10)))
    #
    # ppi10: Does any household member own a working bicycle, motor cycle, or car?
    #
    q10 <- menu(title = "Does the household own (or rent/sharecrop/mortgage in
                or out) 51 or more decimals of cultivable agricultural \
                land (excluding uncultivable land and dwelling-house/homestead
                land)?",
                choices = c("None", "Only bicycle", "Motor cycle or car (regardless of bicycle)"))
    ppi10 <- ifelse(q10 == 1, 0,
               ifelse(q10 == 2, 3, 8))
    #
    # ppi: total score
    #
    ppi <- ppi1 + ppi2 + ppi3 + ppi4 + ppi5 + ppi6 + ppi7 + ppi8 + ppi9 + ppi10
  }
  #
  #
  #
  if(!is.na(ppi)){ return(ppi) }
}

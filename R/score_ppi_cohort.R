################################################################################
#
#' score_ppi_cohort
#'
#' @param data A data frame containing responses to the 10 questions used to
#'     elicit information for calculating the PPI score
#' @param ccode Three letter ISO code for a country
#' @return A numeric vector of PPI scores for each household
#' @examples
#' #
#' \dontrun{
#' score_ppi_cohort(data = surveyDataBGD, ccode = "BGD")
#' }
#'
#' @export
#'
#'
#
################################################################################

score_ppi_cohort <- function(data, ccode) {
  #
  # Check if country is Afghanistan
  #
  if(ccode == "AFG") {
    #
    # ppi1: Number of household members 16-years old or younger
    #
    ppi1 <- ifelse(data$ppi1 == "None", 29,
              ifelse(data$ppi1 == "One", 23,
                ifelse(data$ppi1 == "Two", 17,
                  ifelse(data$ppi1 == "Three", 12,
                    ifelse(data$ppi1 == "Four", 9,
                      ifelse(data$ppi1 == "Five or six", 4, 12))))))
    #
    # ppi2: Both male and female head of household can dread and write
    #
    ppi2 <- ifelse(data$ppi2 == "Yes", 11,
              ifelse(data$ppi2 == "No", 5,
                ifelse(data$ppi2 == "No female head/spouse", 5, 0)))
    #
    # ppi3: Type of dwelling
    #
    ppi3 <- ifelse(data$ppi3 == "Single-family house", 3, 0)
    #
    # ppi4: How many rooms
    #
    ppi4 <- ifelse(data$ppi4 == "Five or more", 4, 0)
    #
    # ppi5: Toilet facility
    #
    ppi5 <- ifelse(data$ppi5 == "Improved latrine, or flush latrine", 11,
              ifelse(data$ppi5 == "Traditional covered latrine", 6,
                ifelse(data$ppi5 == "Open pit", 5, 0)))
    #
    # ppi6: Main source of cooking fuel
    #
    ppi6 <- ifelse(data$ppi6 == "Crop residues, firewood, charcoal/coal, kerosene or oil, gas, or electricity", 4, 0)
    #
    # ppi7: How many stoves/gas cylinders
    #
    ppi7 <- ifelse(data$ppi7 == "Two or more", 9,
              ifelse(data$ppi7 == "One", 1, 0))
    #
    # ppi8: Own sewing machines
    #
    ppi8 <- ifelse(data$ppi8 == "Yes", 3, 0)
    #
    # ppi9: Own motorcycles or cars
    #
    ppi9 <- ifelse(data$ppi9 == "Car (regardless of motorcyle)", 22,
              ifelse(data$ppi9 == "Motorcycle only", 12, 0))
    #
    # ppi10: Own/access to irrigated land
    #
    ppi10 <- ifelse(data$ppi10 == "Yes", 4, 0)
    #
    # ppi: total score
    #
    ppi <- ppi1 + ppi2 + ppi3 + ppi4 + ppi5 + ppi6 + ppi7 + ppi8 + ppi9 + ppi10
  }
  #
  # Check if country is Angola
  #
  if(ccode == "AGO") {
    #
    # ppi1: What province?
    #
    ppi1 <- ifelse(data$ppi1 == "Moxico, Cunene, Zaire, or Cabinda", 19,
              ifelse(data$ppi1 == "Namibe, Bengo, or Kwanza Sul", 16,
                ifelse(data$ppi1 == "Uige, or Kuando Kubango", 12,
                  ifelse(data$ppi1 == "Huila, or Luanda", 10,
                    ifelse(data$ppi1 == "Lunda Sul, or Lunda Norte", 9,
                      ifelse(data$ppi1 == "Kwanza Norte, Huambo, or Bie", 5, 0))))))
    #
    # ppi2: How many household members
    #
    ppi2 <- ifelse(data$ppi2 == "One", 100,
              ifelse(data$ppi2 == "Two", 31,
                ifelse(data$ppi2 == "Three", 26,
                  ifelse(data$ppi2 == "Four", 21,
                    ifelse(data$ppi2 == "Five", 16,
                      ifelse(data$ppi2 == "Six", 11,
                        ifelse(data$ppi2 == "Seven", 9,
                          ifelse(data$ppi2 == "Eight", 5, 0))))))))
    #
    # ppi3: Work for someone else
    #
    ppi3 <- ifelse(data$ppi3 == "Yes", 3, 0)
    #
    # ppi4: Male head/spouse know how to read and write
    #
    ppi4 <- ifelse(data$ppi4 == "Yes", 2,
              ifelse(data$ppi4 == "No male head/spouse", 1, 0))
    #
    # ppi5: Female head/spouse know how to read and write
    #
    ppi5 <- ifelse(data$ppi5 == "Yes", 5,
              ifelse(data$ppi5 == "No", 2, 0))
    #
    # ppi6: Material of the floor of the residence
    #
    ppi6 <- ifelse(data$ppi6 == "Cement, wood or parquet, marble, granite, brick, or other", 5, 0)
    #
    # ppi7: Main type of cooking fuel
    #
    ppi7 <- ifelse(data$ppi7 == "LPG, electricity, or does not cook", 100,
              ifelse(data$ppi7 == "Kerosene, or charcoal", 5, 0))
    #
    # ppi8: Number of beds
    #
    ppi8 <- ifelse(data$ppi8 == "Two or more", 7,
              ifelse(data$ppi8 == "One", 3, 0))
    #
    # ppi9: Black and white or colour television
    #
    ppi9 <- ifelse(data$ppi9 == "Yes, color (regardless of black-and-white)", 9,
              ifelse(data$ppi9 == "Yes, only black-and-white", 6, 0))
    #
    # ppi10: Bicycle, motorcycle/scooter or car in good working order
    #
    ppi10 <- ifelse(data$ppi10 == "Two or more motorcycles, or a car (regardless of bicycle)", 13,
               ifelse(data$ppi10 == "One motorcycle, but no car (regardless of bicycle)", 6,
                 ifelse(data$ppi10 == "Only bicycle", 5, 0)))
    #
    # ppi: total score
    #
    ppi <- ppi1 + ppi2 + ppi3 + ppi4 + ppi5 + ppi6 + ppi7 + ppi8 + ppi9 + ppi10
  }
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
  #
  # Check if country is Benin
  #
  if(ccode == "BEN") {
    #
    # ppi1: Department
    #
    ppi1 <- ifelse(data$ppi1 == "Atakora", 14,
              ifelse(data$ppi1 == "Alibori", 13,
                ifelse(data$ppi1 == "Donga, or Borgou", 12,
                  ifelse(data$ppi1 == "Oueme", 11,
                    ifelse(data$ppi1 == "Plateau", 7,
                      ifelse(data$ppi1 == "Couffo", 4,
                        ifelse(data$ppi1 == "Zou, Atlantique, or Collines", 3,
                          ifelse(data$ppi1 == "Mono", 1, 0))))))))
    #
    # ppi2: Material for exterior walls
    #
    ppi2 <- ifelse(data$ppi2 == "Bricks", 4,
              ifelse(data$ppi2 == "Mud plastered with cement", 1, 0))
    #
    # ppi3: Household members
    #
    ppi3 <- ifelse(data$ppi3 == "One", 48,
              ifelse(data$ppi3 == "Two", 40,
                ifelse(data$ppi3 == "Three", 30,
                  ifelse(data$ppi3 == "Four", 20,
                    ifelse(data$ppi3 == "Five", 14,
                      ifelse(data$ppi3 == "Six", 10,
                        ifelse(data$ppi3 == "Seven", 6, 0)))))))
    #
    # ppi4: Female head/spouse know how to read and write with understanding in French
    #
    ppi4 <- ifelse(data$ppi4 %in% c("There is no female head/spouse", "Yes"), 3, 0)
    #
    # ppi5: Main source of energy for lighting in household
    #
    ppi5 <- ifelse(data$ppi5 == "Kerosene", 0, 4)
    #
    # ppi6: How many rooms for sleeping
    #
    ppi6 <- ifelse(data$ppi6 == "Three or more", 5,
              ifelse(data$ppi6 == "Two", 2, 0))
    #
    # ppi7: Main cooking fuel
    #
    ppi7 <- ifelse(data$ppi7 == "Firewood, or straw", 0, 3)
    #
    # ppi8: Motorcycle, scooter, or automobile
    #
    ppi8 <- ifelse(data$ppi8 == "Yes", 5, 0)
    #
    # ppi9: Number of mobile telephones
    #
    ppi9 <- ifelse(data$ppi9 == "Two or more", 9,
              ifelse(data$ppi9 == "One", 2, 0))
    #
    # ppi10: Land ownership
    #
    ppi10 <- ifelse(data$ppi10 == "Does own etc., and some land is sub-divided, developed, or irrigated", 5,
               ifelse(data$ppi10 == "Does own etc., but land is not sub-divided, developed, or irrigated", 2, 0))
    #
    # ppi: total score
    #
    ppi <- ppi1 + ppi2 + ppi3 + ppi4 + ppi5 + ppi6 + ppi7 + ppi8 + ppi9 + ppi10
  }
  #
  # Check if country is Bolivia
  #
  if(ccode == "BOL") {
    #
    # ppi1: Household members
    #
    ppi1 <- ifelse(data$ppi1 == "One", 23,
              ifelse(data$ppi1 == "Two", 20,
                ifelse(data$ppi1 == "Three", 14,
                  ifelse(data$ppi1 == "Four", 9, 0))))
    #
    # ppi2: Male work for at least one hour
    #
    ppi2 <- ifelse(data$ppi2 == "Yes", 10,
              ifelse(data$ppi2 == "No", 0, 6))
    #
    # ppi3: Mother tongue of the female head/spouse
    #
    ppi3 <- ifelse(data$ppi3 == "No female head/spouse", 10,
              ifelse(data$ppi3 == "Spanish", 6, 0))
    #
    # ppi4: How many rooms
    #
    ppi4 <- ifelse(data$ppi4 == "Five or more", 7,
              ifelse(data$ppi4 == "Four", 5,
                ifelse(data$ppi4 == "Three", 2, 0)))
    #
    # ppi5: Material of floors
    #
    ppi5 <- ifelse(data$ppi5 == "Dirt, or other", 0,
              ifelse(data$ppi5 == "Bricks, or cement", 5, 11))
    #
    # ppi6: Toilet arrangements
    #
    ppi6 <- ifelse(data$ppi6 == "None/bush/field", 0, 5)
    #
    # ppi7: Main fuel for cooking
    #
    ppi7 <- ifelse(data$ppi7 == "Piped-in natural gas, electricity, or does not cook", 12,
              ifelse(data$ppi7 == "LPG from a cylinder", 7, 0))
    #
    # ppi8: Refrigerator or freezer
    #
    ppi8 <- ifelse(data$ppi8 == "Yes", 7, 0)
    #
    # ppi9: Television
    #
    ppi9 <- ifelse(data$ppi9 == "Yes", 9, 0)
    #
    # ppi10: Motorcyle or automobile
    #
    ppi10 <- ifelse(data$ppi10 == "Yes", 6, 0)
    #
    # ppi: total score
    #
    ppi <- ppi1 + ppi2 + ppi3 + ppi4 + ppi5 + ppi6 + ppi7 + ppi8 + ppi9 + ppi10
  }
  #
  # Check if country is Brazil
  #
  if(ccode == "BRA") {
    #
    # ppi1: Members
    #
    ppi1 <- ifelse(data$ppi1 == "One", 20,
              ifelse(data$ppi1 == "Two", 17,
                ifelse(data$ppi1 == "Three", 11,
                  ifelse(data$ppi1 == "Four", 6, 0))))
    #
    # ppi2: children go to school
    #
    ppi2 <- ifelse(data$ppi2 == "No", 0,
              ifelse(data$ppi2 == "Yes", 5, 7))
    #
    # ppi3: Years of schooling of femal head/spouse
    #
    ppi3 <- ifelse(data$ppi3 == "Three or less", 0,
              ifelse(data$ppi3 == "Four to eleven", 2, 8))
    #
    # ppi4: Employees
    #
    ppi4 <- ifelse(data$ppi4 == "None", 0,
              ifelse(data$ppi4 == "One", 4, 13))
    #
    # ppi5: Managers
    #
    ppi5 <- ifelse(data$ppi5 == "None", 0, 8)
    #
    # ppi6: Rooms
    #
    ppi6 <- ifelse(data$ppi6 == "Eight or more", 11,
              ifelse(data$ppi6 == "Seven", 7,
                ifelse(data$ppi6 == "Six", 5,
                  ifelse(data$ppi6 == "Five", 2, 0))))
    #
    # ppi7: Sewage disposal
    #
    ppi7 <- ifelse(data$ppi7 == "Direct connection to public sewage/rainwater system", 5,
              ifelse(data$ppi7 == "Septic tank connected to public sewage/rainwater system", 4,
                ifelse(data$ppi7 == "Septic tank not connected to public sewage/rainwater system", 3,
                  ifelse(data$ppi7 == "Simple hole, or directly into river, lake, or ocean", 2, 0))))
    #
    # ppi8: Refrigerator
    #
    ppi8 <- ifelse(data$ppi8 == "Yes, with two doors", 10,
              ifelse(data$ppi8 == "Yes, with one door", 5, 0))
    #
    # ppi9: Washing machine
    #
    ppi9 <- ifelse(data$ppi9 == "Yes", 7, 0)
    #
    # ppi10: Cellular or land-line telephone
    #
    ppi10 <- ifelse(data$ppi10 == "Both", 11,
               ifelse(data$ppi10 == "Land-line but not cellular", 6,
                 ifelse(data$ppi10 == "Cellular but not land-line", 5, 0)))
    #
    # ppi: total score
    #
    ppi <- ppi1 + ppi2 + ppi3 + ppi4 + ppi5 + ppi6 + ppi7 + ppi8 + ppi9 + ppi10
  }
  #
  # Check if country is Burkina Faso
  #
  if(ccode == "BFA") {
    #
    # ppi1: Region
    #
    ppi1 <- ifelse(data$ppi1 == "Centre", 16,
              ifelse(data$ppi1 == "Sahel", 15,
                ifelse(data$ppi1 %in% c("Sud Ouest", "Centre Sude"), 11,
                  ifelse(data$ppi1 == "Centre Nord", 10,
                    ifelse(data$ppi1 %in% c("Est", "Centre Est"), 9,
                      ifelse(data@ppi1 == "Plateau Central", 8,
                        ifelse(data$ppi1 == "Cascade", 6,
                          ifelse(data$ppi1 == "Centre Ouest", 5,
                            ifelse(data$ppi1 == "Hauts Bassins", 3, 0)))))))))
    #
    # ppi2: Household members
    #
    ppi2 <- ifelse(data$ppi2 == "4 or less", 20,
              ifelse(data$ppi2 == "5 to 6", 13,
                ifelse(data$ppi2 == "7 to 8", 9, 0)))
    #
    # ppi3: Male head/spouse can read and write
    #
    ppi3 <- ifelse(data$ppi3 == "Yes", 5, 0)
    #
    # ppi4: child going to school
    #
    ppi4 <- ifelse(data$ppi4 == "Every child aged 7 to 14 attended formal school during the last school year", 5,
              ifelse(data$ppi4 == "No child aged 7 to 14", 7, 0))
    #
    # ppi5: Material to construct floor
    #
    ppi5 <- ifelse(data$ppi5 == "Sand Blasted", 7,
              ifelse(data$ppi5 == "Cement Screed", 5,
                ifelse(data$ppi5 == "Tile", 8, 0)))
    #
    # ppi6: Main source of lighting
    #
    ppi6 <- ifelse(data$ppi6 == "Battery Torch", 0,
              ifelse(data$ppi6 == "Network Electricity", 12,
                ifelse(data$ppi6 == "Solar Energy", 5, 4)))
    #
    # ppi7: Own car
    #
    ppi7 <- ifelse(data$ppi7 == "Yes", 10, 0)
    #
    # ppi8: Own mattress
    #
    ppi8 <- ifelse(data$ppi8 == "Yes", 6, 0)
    #
    # ppi9: Consumed milk and/or dairy
    #
    ppi9 <- ifelse(data$ppi9 == "Yes", 9, 0)
    #
    # ppi10: Consumed sugar
    #
    ppi10 <- ifelse(data$ppi10 == "Yes", 7, 0)
    #
    # ppi: total score
    #
    ppi <- ppi1 + ppi2 + ppi3 + ppi4 + ppi5 + ppi6 + ppi7 + ppi8 + ppi9 + ppi10
  }
  #
  # Check if country is Cambodia
  #
  if(ccode == "KHM") {
    #
    # ppi1: Household members
    #
    ppi1 <- ifelse(data$ppi1 == "Eight or more", 0,
              ifelse(data$ppi1 == "Seven", 7,
                ifelse(data$ppi1 == "Six", 9,
                  ifelse(data$ppi1 == "Five", 17,
                    ifelse(data$ppi1 == "Four", 22,
                      ifelse(data$ppi1 == "Three", 32, 40))))))
    #
    # ppi2: Work
    #
    ppi2 <- ifelse(data$ppi2 == "None or one", 0,
              ifelse(data$ppi2 == "Two", 3, 5))
    #
    # ppi3: Female head/spouse read or write a simple message in any language
    #
    ppi3 <- ifelse(data$ppi3 == "No", 0,
              ifelse(data$ppi3 == "Yes", 2, 1))
    #
    # ppi4: Rooms
    #
    ppi4 <- ifelse(data$ppi4 == "One", 0,
              ifelse(data$ppi4 == "Two", 5, 12))
    #
    # ppi5: Wall material
    #
    ppi5 <- ifelse(data$ppi5 == "Concrete, brick, or stone", 4,
              ifelse(data$ppi5 == "Wood, logs, plywood, galvanized iron or aluminium or other metal sheets, or fibrous cement/asbestos", 3, 0))
    #
    # ppi6: Roof material
    #
    ppi6 <- ifelse(data$ppi6 == "Tiles, fibrous cement, or concrete", 4,
              ifelse(data$ppi6 == "Galvanized iron or aluminium, or mixed but predominantly galvanized iron/aluminium/tiles/fibrous cement", 1, 0))
    #
    # ppi7: Wardrobes or cabinets
    #
    ppi7 <- ifelse(data$ppi7 == "None", 0,
              ifelse(data$ppi7 == "One", 6, 8))
    #
    # ppi8: TV, video/VCD/DVD player
    #
    ppi8 <- ifelse(data$ppi8 == "No", 0,
              ifelse(data$ppi8 == "Only television", 3, 6))
    #
    # ppi9: Phones
    #
    ppi9 <- ifelse(data$ppi9 == "None", 0,
              ifelse(data$ppi9 == "One", 4, 9))
    #
    # ppi10: Motorcycles or motorboats
    #
    ppi10 <- ifelse(data$ppi10 == "None", 0,
               ifelse(data$ppi10 == "One", 6, 10))
    #
    # ppi: total score
    #
    ppi <- ppi1 + ppi2 + ppi3 + ppi4 + ppi5 + ppi6 + ppi7 + ppi8 + ppi9 + ppi10
  }
  #
  # Check if country is Cameroon
  #
  if(ccode == "CMR") {
    #
    # ppi1: Household members
    #
    ppi1 <- ifelse(data$ppi1 == "One or two", 29,
              ifelse(data$ppi1 == "Three", 20,
                ifelse(data$ppi1 == "Four", 16,
                  ifelse(data$ppi1 == "Five", 12,
                    ifelse(data$ppi1 == "Six", 10, 0)))))
    #
    # ppi2: School
    #
    ppi2 <- ifelse(data$ppi2 == "No", 0,
              ifelse(data$ppi2 == "Yes", 2, 7))
    #
    # ppi3: Work
    #
    ppi3 <- ifelse(data$ppi3 == "Yes", 0, 2)
    #
    # ppi4: Male head/spouse read and write a simple sentence in French or English
    #
    ppi4 <- ifelse(data$ppi4 == "No", 0,
              ifelse(data$ppi4 %in% c("Only English", "Only French"), 2, 3))
    #
    # ppi5: Female head/spouse read and write a simple sentence in French or English
    #
    ppi5 <- ifelse(data$ppi5 == "No", 0,
              ifelse(data$ppi5 == "No female head/spouse", 2,
                ifelse(data$ppi5 == "Only English", 4,
                  ifelse(data$ppi5 == "Only French", 6, 8))))
    #
    # ppi6: Floor material
    #
    ppi6 <- ifelse(data$ppi6 == "Dirt, or other", 0, 6)
    #
    # ppi7: Fuel for cooking
    #
    ppi7 <- ifelse(data$ppi7 == "LPG", 19,
              ifelse(data$ppi7 == "Collected/gifted firewood, or other", 0, 9))
    #
    # ppi8: Electric iron
    #
    ppi8 <- ifelse(data$ppi8 == "No", 0, 6)
    #
    # ppi9: Radio or television
    #
    ppi9 <- ifelse(data$ppi9 == "No", 0,
              ifelse(data$ppi9 == "Only radio", 7, 14))
    #
    # ppi10: Buffet or wardrobe
    #
    ppi10 <- ifelse(data$ppi10 == "No", 0, 6)
    #
    # ppi: total score
    #
    ppi <- ppi1 + ppi2 + ppi3 + ppi4 + ppi5 + ppi6 + ppi7 + ppi8 + ppi9 + ppi10
  }
  #
  # Check if country is Colombia
  #
  if(ccode == "COL") {
    #
    # ppi1: Household members 18 and younger
    #
    ppi1 <- ifelse(data$ppi1 == "None", 23,
              ifelse(data$ppi1 == "One", 17,
                ifelse(data$ppi1 == "Two", 11,
                  ifelse(data$ppi1 == "Three", 5, 0))))
    #
    # ppi2: Female education
    #
    ppi2 <- ifelse(data$ppi2 == "None, or pre-school", 0,
              ifelse(data$ppi2 == "Primary or middle school", 3,
                ifelse(data$ppi2 == "High School", 6,
                  ifelse(data$ppi2 == "No female head/spouse", 8,
                    ifelse(data$ppi2 == "Post-secondary or college (1 to 4 years)", 9, 17)))))
    #
    # ppi3: Household members working
    #
    ppi3 <- ifelse(data$ppi3 == "None", 0,
              ifelse(data$ppi3 == "One", 9, 14))
    #
    # ppi4: Work as wage or salary employees
    #
    ppi4 <- ifelse(data$ppi4 == "None", 0,
              ifelse(data$ppi4 == "One", 4, 11))
    #
    # ppi5: Rate class for electricity
    #
    ppi5 <- ifelse(data$ppi5 == "Four, five, or six", 9,
              ifelse(data$ppi5 == "Three", 4, 0))
    #
    # ppi6: Fuel for cooking
    #
    ppi6 <- ifelse(data$ppi6 == "Does not cook", 6,
              ifelse(data$ppi6 == "Natural gas from a public network", 3,
                ifelse(data$ppi6 == "LPG from a cylinder or tank", 2, 0)))
    #
    # ppi7: Washing machine
    #
    ppi7 <- ifelse(data$ppi7 == "No", 0, 4)
    #
    # ppi8: Refrigerator or freezer
    #
    ppi8 <- ifelse(data$ppi8 == "No", 0, 3)
    #
    # ppi9: working DVD
    #
    ppi9 <- ifelse(data$ppi9 == "No", 0, 4)
    #
    # ppi10: Motorcycle or car
    #
    ppi10 <- ifelse(data$ppi10 == "None", 0,
               ifelse(data$ppi10 == "Motorcycle only", 3, 9))
    #
    # ppi: total score
    #
    ppi <- ppi1 + ppi2 + ppi3 + ppi4 + ppi5 + ppi6 + ppi7 + ppi8 + ppi9 + ppi10
  }
  #
  # Check if country is Dominican Republic
  #
  if(ccode == "DOM") {
    #
    # ppi1: Household members 16 years or younger
    #
    ppi1 <- ifelse(data$ppi1 == "None", 12,
              ifelse(data$ppi1 == "One", 11,
                ifelse(data$ppi1 == "Two", 7,
                  ifelse(data$ppi1 == "Three", 3, 0))))
    #
    # ppi2: Female head education
    #
    ppi2 <- ifelse(data$ppi2 == "None, or up to first grade", 0,
              ifelse(data$ppi2 == "Second to fifth grade", 2,
                ifelse(data$ppi2 == "Sixth or seventh grade", 3,
                  ifelse(data$ppi2 %in% c("Eight to twelfth grade", "There is no female head/spouse"), 5,
                    ifelse(data$ppi2 == "One to three years of post-secondary school or college", 7, 10)))))
    #
    # ppi3: Attend private school
    #
    ppi3 <- ifelse(data$ppi3 == "No", 0, 4)
    #
    # ppi4: Business
    #
    ppi4 <- ifelse(data$ppi4 == "None", 22,
              ifelse(data$ppi4 == "One", 19,
                ifelse(data$ppi4 == "Two", 11,
                  ifelse(data$ppi4 == "Three", 7,
                    ifelse(data$ppi4 == "Four", 6, 0)))))
    #
    # ppi5: Roof material
    #
    ppi5 <- ifelse(data$ppi5 == "Reinforced concrete", 3, 0)
    #
    # ppi6: toilet
    #
    ppi6 <- ifelse(data$ppi6 == "Private flush toilet", 4, 0)
    #
    # ppi7: Water meter
    #
    ppi7 <- ifelse(data$ppi7 == "Yes", 8, 0)
    #
    # ppi8: Fuel for cooking
    #
    ppi8 <- ifelse(data$ppi8 == "Does not cook", 13,
              ifelse(data$ppi8 == "Electricity or propane", 8, 0))
    #
    # ppi9: Motorcycle, car, SUV or pick-up
    #
    ppi9 <- ifelse(data$ppi9 == "No", 0,
              ifelse(data$pp9 == "Motorcyle only", 5, 17))
    #
    # ppi10: VCR or DVD
    #
    ppi10 <- ifelse(data$ppi10 == "No", 0, 7)
    #
    # ppi: total score
    #
    ppi <- ppi1 + ppi2 + ppi3 + ppi4 + ppi5 + ppi6 + ppi7 + ppi8 + ppi9 + ppi10
  }
  #
  # Check if country is Ecuador
  #
  if(ccode == "ECU") {
    #
    # ppi: total score
    #
    ppi <- ppi1 + ppi2 + ppi3 + ppi4 + ppi5 + ppi6 + ppi7 + ppi8 + ppi9 + ppi10
  }
  #
  # Check if country is Egypt
  #
  if(ccode == "EGY") {
    #
    # ppi: total score
    #
    ppi <- ppi1 + ppi2 + ppi3 + ppi4 + ppi5 + ppi6 + ppi7 + ppi8 + ppi9 + ppi10
  }
  #
  # Check if country is El Salvador
  #
  if(ccode == "SLV") {
    #
    # ppi: total score
    #
    ppi <- ppi1 + ppi2 + ppi3 + ppi4 + ppi5 + ppi6 + ppi7 + ppi8 + ppi9 + ppi10
  }
  #
  # Check if country is Ghana
  #
  if(ccode == "GHA") {
    #
    # ppi1: Number of household members
    #
    ppi1 <- ifelse(data$ppi1 == "One", 29,
              ifelse(data$ppi1 == "Two", 24,
                ifelse(data$ppi1 == "Three", 21,
                  ifelse(data$ppi1 == "Four", 14,
                    ifelse(data$ppi1 == "Five", 13,
                      ifelse(data$ppi1 == "Six", 9,
                        ifelse(data$ppi1 == "Seven", 4, 0)))))))
    #
    # ppi2: Are all household members ages 5 to 17 currently in school?
    #
    ppi2 <- ifelse(data$ppi2 == "Yes", 2,
              ifelse(data$ppi2 == "No one ages 5 to 17", 3, 0))
    #
    # ppi3: Can the male head/spouse read a phrase/sentence in English?
    #
    ppi3 <- ifelse(data$ppi3 == "No male head/spouse", 2,
              ifelse(data$ppi3 == "Yes", 5, 0))
    #
    # ppi4: What is the main construction material used for the outer wall?
    #
    ppi4 <- ifelse(data$ppi4 == "Cement/concrete blocks, landcrete, stone, or burnt bricks", 5, 0)
    #
    # ppi5: What type of toilet facility is usually used by the household?
    #
    ppi5 <- ifelse(data$ppi5 == "KVIP, or W.C.", 6,
              ifelse(data$ppi5 == "Public toilet (e.g., W.C., KVIP, pitpan)", 4,
                ifelse(data$ppi5 == "Pit latrine, bucket/pan", 4, 0)))
    #
    # ppi6: What is the main fuel used by the household for cooking?
    #
    ppi6 <- ifelse(data$ppi6 == "Gas, or electricity", 22,
              ifelse(data$ppi6 == "Charcoal, or kerosene", 13,
                ifelse(data$ppi6 == "Wood, crop residue, sawdust, animal waste, or other", 6, 0)))
    #
    # ppi7: Does any household member own a working box iron or electric iron?
    #
    ppi7 <- ifelse(data$ppi7 == "Yes", 4, 0)
    #
    # ppi8: Does any household member own a working television, video player,
    #       VCD/DVD/MP3/MP4 player/iPod, or satellite dish?
    #
    ppi8 <- ifelse(data$ppi8 == "No", 0,
              ifelse(data$ppi8 == "Only television", 2, 8))
    #
    # ppi9: How many working mobile phones are owned by members of the household?
    #
    ppi9 <- ifelse(data$ppi9 == "None", 0,
              ifelse(data$ppi9 == "One", 4,
                ifelse(data$ppi9 == "Two", 8, 10)))
    #
    # ppi10: Does any household member own a working bicycle, motor cycle, or car?
    #
    ppi10 <- ifelse(data$ppi10 == "None", 0,
               ifelse(data$ppi10 == "Only bicycle", 3, 8))
    #
    # ppi: total score
    #
    ppi <- ppi1 + ppi2 + ppi3 + ppi4 + ppi5 + ppi6 + ppi7 + ppi8 + ppi9 + ppi10
  }
  #
  # Check if country is Kenya
  #
  if(ccode == "Kenya") {
    #
    # ppi1: How many members does the household have
    #
    ppi1 <- ifelse(data$ppi1 == "One or two", 32,
              ifelse(data$ppi1 == "Three", 22,
                ifelse(data$ppi1 == "Four", 18,
                  ifelse(data$ppi1 == "Five", 12,
                    ifelse(data$ppi1 == "Six", 8,
                      ifelse(data$ppi1 == "Seven or eight", 5, 0))))))
    #
    # ppi2: What is the highest school grade that the female head/spouse has
    #       completed?
    #
    ppi2 <- ifelse(data$ppi2 == "Secondary form 4 or higher", 11,
                   ifelse(data$ppi2 == "No female head/spouse", 6,
                          ifelse(data$ppi2 == "Primary standard 8, or secondary forms 1 to 3", 6,
                                 ifelse(data$ppi2 == "Primary standard 7", 2,
                                        ifelse(data$ppi2 == "Primary standards 1 to 6", 1, 0)))))
    #
    # ppi3: What kind of business (type of industry) is the main occupation of the
    #       male head/spouse connected with?
    #
    ppi3 <- ifelse(data$ppi3 == "Any other", 9,
              ifelse(data$ppi3 == "Agriculture, hunting, forestry, fishing, mining, or quarrying", 7,
                ifelse(data$ppi3 == "No male head/spouse", 3,
                  ifelse(data$ppi3 == "Sixth grade", 2, 0))))
    #
    # ppi4: How many habitable rooms does this household occupy in its main
    #       dwelling (do not count bathrooms, toilets, storerooms, or gargage)?
    #
    ppi4 <- ifelse(data$ppi4 == "Four or more", 8,
              ifelse(data$ppi4 == "Three", 5,
                ifelse(data$ppi4 == "Two", 2, 0)))
    #
    # ppi5: The floor of the main dwelling is predominantly made of what material?
    #
    ppi5 <- ifelse(data$ppi5 == "Cement, or tiles", 3, 0)
    #
    # ppi6: What is the main source of lighting fuel for the household?
    #
    ppi6 <- ifelse(data$ppi6 == "Electricity, solar, or gas", 12,
              ifelse(data$ppi6 == "Paraffin, candles, biogas, or other", 6, 0))
    #
    # ppi7: Does your household own any irons (charcoal or electric)?
    #
    ppi7 <- ifelse(data$ppi7 == "Yes", 4, 0)
    #
    # ppi8: How many mosquito nets does your household own?
    #
    ppi8 <- ifelse(data$ppi8 == "Two or more", 4,
              ifelse(data$ppi8 == "One", 2, 0))
    #
    # ppi9: How many towels does your household own?
    #
    ppi9 <- ifelse(data$ppi9 == "Two or more", 10,
              ifelse(data$ppi9 == "One", 6, 0))
    #
    # ppi10: How many frying pans does your household own?
    #
    ppi10 <- ifelse(data$ppi10 == "Two or more", 7,
               ifelse(data$ppi10 == "One", 3, 0))
    #
    # ppi: total score
    #
    ppi <- ppi1 + ppi2 + ppi3 + ppi4 + ppi5 + ppi6 + ppi7 + ppi8 + ppi9 + ppi10
  }
  #
  # Check if country is Madagascar
  #
  if(ccode == "Madagascar") {
    #
    # ppi1: How many members does the household have
    #
    ppi1 <- ifelse(data$ppi1 == "One", 38,
              ifelse(data$ppi1 == "Two", 33,
                ifelse(data$ppi1 == "Three", 25,
                  ifelse(data$ppi1 == "Four", 19,
                    ifelse(data$ppi1 == "Five", 13,
                      ifelse(data$ppi1 == "Six", 9,
                        ifelse(data$ppi1 == "Seven", 6,
                          ifelse(data$ppi1 == "Eight", 5, 0))))))))
    #
    # ppi2: Can the (oldest) female head/spouse read a simple message?
    #
    ppi2 <- ifelse(data$ppi2 == "No", 0,
              ifelse(data$ppi2 == "Yes", 2, 3))
    #
    # ppi3: What is the main material of the floor of the residence?
    #
    ppi3 <- ifelse(data$ppi3 == "Cement, concrete, or fiberglass", 11,
              ifelse(data$ppi3 == "Wood, stone, or brick", 8,
                ifelse(data$ppi3 == "Dirt (with or without mats)", 5, 0)))
    #
    # ppi4: What is the main permanent ceiling material?
    #
    ppi4 <- ifelse(data$ppi4 == "Bark, leaves, stems, dirt, or mud", 0,
              ifelse(data$ppi4 == "No ceiling, or other", 3, 7))
    #
    # ppi5: How many tables does the household have?
    #
    ppi5 <- ifelse(data$ppi5 == "Two or more", 6,
              ifelse(data$ppi5 == "One", 2, 0))
    #
    # ppi6: How many beds does the household have?
    #
    ppi6 <- ifelse(data$ppi6 == "Three or more", 9,
                   ifelse(data$ppi6 == "Two", 4,
                          ifelse(data$ppi6 == "One", 2, 0)))
    #
    # ppi7: Does the household have a radio, radio/cassette player, or hi-fi
    #       stereo system?
    #
    ppi7 <- ifelse(data$ppi7 == "Yes", 5, 0)
    #
    # ppi8: Does the household have a television?
    #
    ppi8 <- ifelse(data$ppi8 == "Yes", 14, 0)
    #
    # ppi9: Does the household have a bicycle, motorcycle/scooter, tractor or
    #       car of its own (not counting business vehicles)?
    #
    ppi9 <- ifelse(data$ppi9 == "Yes", 4, 0)
    #
    # ppi10: Does the household have an agricultural storage shed?
    #
    ppi10 <- ifelse(data$ppi10 == "Yes", 3, 0)
    #
    # ppi: total score
    #
    ppi <- ppi1 + ppi2 + ppi3 + ppi4 + ppi5 + ppi6 + ppi7 + ppi8 + ppi9 + ppi10
  }
  #
  # Check if country is Mozambique
  #
  if(ccode == "Mozambique") {
    #
    # ppi1: How many members does the household have
    #
    ppi1 <- ifelse(data$ppi1 == "One", 34,
              ifelse(data$ppi1 == "Two", 30,
                ifelse(data$ppi1 == "Three", 23,
                  ifelse(data$ppi1 == "Four", 15,
                    ifelse(data$ppi1 == "Five", 9,
                      ifelse(data$ppi1 == "Six", 7,
                        ifelse(data$ppi1 == "Seven", 2, 0)))))))
    #
    # ppi2: What is the main material of the floor of the residence (excluding
    #       kitchen and bathrooms)?
    #
    ppi2 <- ifelse(data$ppi2 == "Uncovered, or other", 0, 6)
    #
    # ppi3: What is the main material of the walls of the residence?
    #
    ppi3 <- ifelse(data$ppi3 == "Adobe blocks, wattle and daub, cement blocks, or bricks", 7, 0)
    #
    # ppi4: What toilet arrangement does the household use in its residence?
    #
    ppi4 <- ifelse(data$ppi4 == "Toilet connected to a septic tank", 14,
              ifelse(data$ppi4 == "Latrine of any kind", 6, 0))
    #
    # ppi5: What is the main source of energy for lighting in the residence?
    #
    ppi5 <- ifelse(data$ppi5 == "Electricity, generator, or solar panel", 5,
              ifelse(data$ppi5 == "Other", 3,
                ifelse(data$ppi5 == "LPG, oil/paraffin/kerosene, or candles", 1, 0)))
    #
    # ppi6: Does the household have a non-electric or electric clothes iron?
    #
    ppi6 <- ifelse(data$ppi6 == "Yes", 3, 0)
    #
    # ppi7: Does the household have a clock (wall, wrist, or pocket)?
    #
    ppi7 <- ifelse(data$ppi7 == "Yes", 4, 0)
    #
    # ppi8: Does the household have a radio, stereo system, or cassette player?
    #
    ppi8 <- ifelse(data$ppi8 == "Stereo system or cassette player (regardless of radio)", 7,
              ifelse(data$ppi8 == "Radio only", 5, 0))
    #
    # ppi9: Does the household have a bicycle, motorcycle, or car?
    #
    ppi9 <- ifelse(data$ppi9 == "No", 0,
                   ifelse(data$ppi9 == "Bicycle only", 5, 15))
    #
    # ppi10: How many beds does the household have (single, double,
    #        beds, or for children)?
    #
    ppi10 <- ifelse(data$ppi10 == "None", 0,
                    ifelse(data$ppi10 == "One", 2, 5))
    #
    # ppi: total score
    #
    ppi <- ppi1 + ppi2 + ppi3 + ppi4 + ppi5 + ppi6 + ppi7 + ppi8 + ppi9 + ppi10
  }
  #
  # Check if country is Zambia
  #
  if(ccode == "Zambia") {
    #
    # ppi1: How many members does the household have
    #
    ppi1 <- ifelse(data$ppi1 == "One or two", 29,
              ifelse(data$ppi1 == "Three", 21,
                ifelse(data$ppi1 == "Four", 15,
                  ifelse(data$ppi1 == "Five", 11,
                    ifelse(data$ppi1 == "Six", 9,
                      ifelse(data$ppi1 == "Seven", 7, 0))))))
    #
    # ppi2: Are all household members ages 7 to 16 currently attending school?
    #
    ppi2 <- ifelse(data$ppi2 == "No", 0,
              ifelse(data$ppi2 == "Yes", 3, 6))
    #
    # ppi3: What is the highest grade that a female head/spouse has attained?
    #
    ppi3 <- ifelse(data$ppi3 == "Tenth grade or higher", 9,
              ifelse(data$ppi3 == "No female head/spouse", 5,
                ifelse(data$ppi3 == "Seventh to ninth grade", 4,
                  ifelse(data$ppi3 == "Sixth grade", 2, 0))))
    #
    # ppi4: What kind of building material is the floor of this dwelling made of?
    #
    ppi4 <- ifelse(data$ppi4 == "Concrete, or covered concrete", 2, 0)
    #
    # ppi5: What kind of building material is the roof of this dwelling made of?
    #
    ppi5 <- ifelse(data$ppi5 == "Concrete, asbestos sheets, or asbestos tiles", 5,
              ifelse(data$ppi5 == "Iron sheets, or other non-asbestos tiles", 3, 0))
    #
    # ppi6: What is the main type of energy that your household uses for cooking?
    #
    ppi6 <- ifelse(data$ppi6 == "Gas, electricity, solar, or kerosene/paraffin", 15,
              ifelse(data$ppi6 == "Charcoal", 4, 0))
    #
    # ppi7: Does your household own any televisions, DVDs/VCRs or home theatres,
    #       or satellite dish/decoders (free to air, or DSTV) or other pay-TV
    #       arrangements?
    #
    ppi7 <- ifelse(data$ppi7 == "TV, and something else (DVD, dish, etc.", 10,
              ifelse(data$ppi7 == "TV, but nothing else", 6,  0))
    #
    # ppi8: Does your household own any non-electric or electric irons?
    #
    ppi8 <- ifelse(data$ppi8 == "Electric, or both electric and non-electric", 11,
                   ifelse(data$ppi8 == "Only non-electric", 4, 0))
    #
    # ppi9: Does your household own any cellular phones?
    #
    ppi9 <- ifelse(data$ppi9 == "No", 0, 6)
    #
    # ppi10: How many beds and mattresses does your household own?
    #
    ppi10 <- ifelse(data$ppi10 == "Two or more mattresses (regardless of beds)", 7,
               ifelse(data$ppi10 == "One mattress (regardless of beds", 4,
                 ifelse(data$ppi10 == "One or more beds, but no mattresses", 2, 0)))
    #
    # ppi: total score
    #
    ppi <- ppi1 + ppi2 + ppi3 + ppi4 + ppi5 + ppi6 + ppi7 + ppi8 + ppi9 + ppi10
  }
  #
  # Return result
  #
  return(ppi)
}

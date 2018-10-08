setwd("/Users/Jaime/Desktop/Master/Statistics fot Data Analysis/CourseProject/Data/")

#realNames = c("License", "VehicleInProperty", "UseOfNewMobility", "KindOfNewMobility", "RangeOfHours", "LastTripDuration", "LastTripCost", "SlowVehiclesInRoad")

newMobility = read.csv("NewMobility.csv", sep=";", dec=",")#, col.names = realNames)
attach(newMobility)

parseUsageTime <- function(dataset){
  dataset$UsageTime <- dataset$X5Min
  dataset$UsageTime <- as.character(dataset$UsageTime)
  dataset[dataset$UsageTime == "Column 1",]$UsageTime <- 5
  dataset[dataset$X10Min == "Column 1",]$UsageTime <- 10
  dataset[dataset$X15Min == "Column 1",]$UsageTime <- 15
  dataset[dataset$X20Min == "Column 1",]$UsageTime <- 20
  dataset[dataset$X.20Min == "Column 1",]$UsageTime <- 21
  dataset$X5Min <- NULL
  dataset$X10Min <- NULL
  dataset$X15Min <- NULL
  dataset$X20Min <- NULL
  dataset$X.20Min <- NULL
  dataset$UsageTime <- as.factor(dataset$UsageTime)
  return(dataset)
}

newMobility <- parseUsageTime(newMobility)
newMobility$X <- NULL

parseTypeOfNewMobility <- function(dataset) {
  ### Codes for each class: 
  # 0 -> 'I don't use ant new mobility services'
  # 1 -> 'Carsharing'
  # 2 -> 'Motorsharing'
  # 3 -> 'Other (Lime, BiciMAD, etc)'
  dataset$TypeOfNewMobility <- as.character(dataset$TypeOfNewMobility)
  dataset[dataset$TypeOfNewMobility == "",]$TypeOfNewMobility <- "I don't use any new mobility servises"
  dataset[dataset$TypeOfNewMobility == "I don't use any new mobility servises",]$TypeOfNewMobility <- 0
  dataset[dataset$TypeOfNewMobility == "Carsharing",]$TypeOfNewMobility <- 1
  dataset[dataset$TypeOfNewMobility == "Motosharing",]$TypeOfNewMobility <- 2
  dataset[dataset$TypeOfNewMobility == "Other (Lime, BiciMAD, etc)",]$TypeOfNewMobility <- 3
  dataset$TypeOfNewMobility <- as.factor(dataset$TypeOfNewMobility)
  return(dataset)
}

newMobility <- parseTypeOfNewMobility(newMobility)

parseUsagePerMonth <- function(dataset) {
  dataset$UsagePerMonth <- as.character(dataset$UsagePerMonth)
  dataset$UsagePerMonth[dataset$UsagePerMonth == "Never"] <- 0
  dataset$UsagePerMonth[dataset$UsagePerMonth == "Nunca"] <- 0
  dataset$UsagePerMonth[dataset$UsagePerMonth == "None"] <- 0
  
  dataset$UsagePerMonth[dataset$UsagePerMonth == "Once every two months "] <- 0.5
  dataset$UsagePerMonth[dataset$UsagePerMonth == "Less than once a month "] <- 0.5
    
  dataset$UsagePerMonth[dataset$UsagePerMonth == "1 vez al mes"] <- 1
  dataset$UsagePerMonth[dataset$UsagePerMonth == "Once a month"] <- 1
  dataset$UsagePerMonth[dataset$UsagePerMonth == "Once"] <- 1
  dataset$UsagePerMonth[dataset$UsagePerMonth == "Monthly basis"] <- 1
  
  dataset$UsagePerMonth[dataset$UsagePerMonth == "Twice per month"] <- 2
  dataset$UsagePerMonth[dataset$UsagePerMonth == "It depends, but twice a month maybe "] <- 2
  dataset$UsagePerMonth[dataset$UsagePerMonth == "Two times "] <- 2
  dataset$UsagePerMonth[dataset$UsagePerMonth == "twice "] <- 2
  dataset$UsagePerMonth[dataset$UsagePerMonth == "Two days max"] <- 2
  dataset$UsagePerMonth[dataset$UsagePerMonth == "2-3 times"] <- 2
  
  dataset$UsagePerMonth[dataset$UsagePerMonth == "Three"] <- 3
  dataset$UsagePerMonth[dataset$UsagePerMonth == "3-4"] <- 3
  
  dataset$UsagePerMonth[dataset$UsagePerMonth == "Once a week"] <- 4
  dataset$UsagePerMonth[dataset$UsagePerMonth == "weekly"] <- 4
  dataset$UsagePerMonth[dataset$UsagePerMonth == "Weekly"] <- 4
  dataset$UsagePerMonth[dataset$UsagePerMonth == "Una vez a la semana "] <- 4
  dataset$UsagePerMonth[dataset$UsagePerMonth == "Every week "] <- 4
  
  dataset$UsagePerMonth[dataset$UsagePerMonth == "5 veces"] <- 5
  
  dataset$UsagePerMonth[dataset$UsagePerMonth == "Two time per week"] <- 8
  dataset$UsagePerMonth[dataset$UsagePerMonth == "20/month"] <- 20
  dataset$UsagePerMonth[dataset$UsagePerMonth == "25 days"] <- 25
  dataset$UsagePerMonth[dataset$UsagePerMonth == "Almost every day"] <- 25
  
  dataset$UsagePerMonth[dataset$UsagePerMonth == "30 times"] <- 30
  dataset$UsagePerMonth[dataset$UsagePerMonth == "8 veces por semana"] <- 30
  dataset$UsagePerMonth[dataset$UsagePerMonth == "Todos los d\303\255as "] <- 30
  dataset$UsagePerMonth[dataset$UsagePerMonth == "20 times a week"] <- 30
  
  dataset$UsagePerMonth[dataset$UsagePerMonth == "N/A"] <- "<NA>"
  dataset$UsagePerMonth[dataset$UsagePerMonth == "-"] <- "<NA>"
  dataset$UsagePerMonth[dataset$UsagePerMonth == ""] <- "<NA>"
  dataset$UsagePerMonth[is.na(dataset$UsagePerMonth)] <- "<NA>"
  
  dataset$UsagePerMonth <- as.factor(dataset$UsagePerMonth)
  return(dataset)
}

newMobility <- parseUsagePerMonth(newMobility)

unique(newMobility$CostLastTrip)

parseCost <- function(dataset) {
  dataset$CostLastTrip <- as.character(dataset$CostLastTrip)
  dataset$CostLastTrip[dataset$CostLastTrip == "Don\342\200\231t remember it "] <- "<NA>"
  dataset$CostLastTrip[dataset$CostLastTrip == "Don\342\200\231t remember"] <- "<NA>"
  dataset$CostLastTrip[is.na(dataset$CostLastTrip)] <- "<NA>"
  dataset$CostLastTrip[dataset$CostLastTrip == "Nothing"] <- "<NA>"
  dataset$CostLastTrip[dataset$CostLastTrip == "No"] <- "<NA>"
  dataset$CostLastTrip[dataset$CostLastTrip == ""] <- "<NA>"
  dataset$CostLastTrip[dataset$CostLastTrip == "-"] <- "<NA>"
  dataset$CostLastTrip <- as.factor(dataset$CostLastTrip)
  return(dataset)
}

newMobility <- parseCost(newMobility)
unique(newMobility$CostLastTrip)
names(newMobility)

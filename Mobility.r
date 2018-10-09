setwd("/Users/Jaime/Desktop/Master/Statistics fot Data Analysis/CourseProject/Data/")

#realNames = c("License", "VehicleInProperty", "UseOfNewMobility", "KindOfNewMobility", "RangeOfHours", "LastTripDuration", "LastTripCost", "SlowVehiclesInRoad")

newMobility = read.csv("NewMobility.csv", sep=";", dec=",")#, col.names = realNames)
attach(newMobility)

parseUsageTime <- function(dataset){
  dataset$UsageTime <- dataset$X5Min
  dataset$UsageTime <- as.character(dataset$UsageTime)
  dataset[dataset$UsageTime == "Column 1",]$UsageTime <- as.numeric(5,0)
  dataset[dataset$X10Min == "Column 1",]$UsageTime <- as.numeric(10,0)
  dataset[dataset$X15Min == "Column 1",]$UsageTime <- as.numeric(15,0)
  dataset[dataset$X20Min == "Column 1",]$UsageTime <- as.numeric(20,0)
  dataset[dataset$X.20Min == "Column 1",]$UsageTime <- as.numeric(21.0)
  dataset$UsageTime[dataset$UsageTime == ""] <- NA
  dataset$X5Min <- NULL
  dataset$X10Min <- NULL
  dataset$X15Min <- NULL
  dataset$X20Min <- NULL
  dataset$X.20Min <- NULL
  dataset$UsageTime <- as.numeric(dataset$UsageTime)
  return(dataset)
}

newMobility <- parseUsageTime(newMobility)
newMobility$X <- NULL

parseTypeOfNewMobility <- function(dataset) {
  ### Codes for each class: 
  # No -> 'I don't use ant new mobility services'
  # Car -> 'Carsharing'
  # Moto -> 'Motorsharing'
  # Other -> 'Other (Lime, BiciMAD, etc)'
  dataset$TypeOfNewMobility <- as.character(dataset$TypeOfNewMobility)
  dataset[dataset$TypeOfNewMobility == "",]$TypeOfNewMobility <- "I don't use any new mobility servises"
  dataset[dataset$TypeOfNewMobility == "I don't use any new mobility servises",]$TypeOfNewMobility <- "No"
  dataset[dataset$TypeOfNewMobility == "Carsharing",]$TypeOfNewMobility <- "Car"
  dataset[dataset$TypeOfNewMobility == "Motosharing",]$TypeOfNewMobility <- "Moto"
  dataset[dataset$TypeOfNewMobility == "Other (Lime, BiciMAD, etc)",]$TypeOfNewMobility <- "Other"
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
  
  dataset$UsagePerMonth[dataset$UsagePerMonth == "N/A"] <- NA
  dataset$UsagePerMonth[dataset$UsagePerMonth == "-"] <- NA
  dataset$UsagePerMonth[dataset$UsagePerMonth == ""] <- NA  
  dataset$UsagePerMonth[is.na(dataset$UsagePerMonth)] <- NA
  
  dataset$UsagePerMonth <- as.numeric(dataset$UsagePerMonth)
  return(dataset)
}

newMobility <- parseUsagePerMonth(newMobility)

unique(newMobility$CostLastTrip)

parseCost <- function(dataset) {
  dataset$CostLastTrip <- as.character(dataset$CostLastTrip)
  dataset$CostLastTrip[dataset$CostLastTrip == "Don\342\200\231t remember it "] <- NA
  dataset$CostLastTrip[dataset$CostLastTrip == "Don\342\200\231t remember"] <- NA
  dataset$CostLastTrip[is.na(dataset$CostLastTrip)] <- NA
  dataset$CostLastTrip[dataset$CostLastTrip == "Nothing"] <- NA
  dataset$CostLastTrip[dataset$CostLastTrip == "No"] <- NA
  dataset$CostLastTrip[dataset$CostLastTrip == ""] <- NA
  dataset$CostLastTrip[dataset$CostLastTrip == "-"] <- NA
  dataset$CostLastTrip <- as.numeric(dataset$CostLastTrip)
  return(dataset)
}

newMobility <- parseCost(newMobility)
#unique(newMobility$CostLastTrip)
#names(newMobility)

timeOfDay <- function(init, end) {
  init <- as.numeric(init)
  end <- as.numeric(end)
  result <- ""
  if ((init >= 0) && (end <= 6)) {
    # Early-Morning
    result <- "Early-Morning"
  } else if ((init >= 6) && (end <= 11)) {
    # Morning
    result <- "Morning"
  } else if (init >= 11) {
    # Afternoon
    if (end <= 17) {
      result <- "Afternoon"
    } else if (end <= 24) {
      result <- "Evening"
    }
  } else if (init >= 17) {
    # Evening
    if (end <= 21) {
      result <- "Evening"
    } else if ((end >= 21) && (end <= 24)) {
      # Night
      result <- "Night"
    }
  } else if ((init >= 21) && (end <= 24)) {
    # Night
    result <- "Night"
  } else {
    result <- "All day"
  }
  return(result)
} 

parseRangeOfHours <- function(dataset) {
  ### Values:
  # Early-Morning (00:00-6:00)
  # Morning (6:00-11:00)
  # Afternoon (12:00-17:00)
  # Evening (18:00-21:00)
  # Night (22:00-00:00)
  dataset$HourRange <- dataset$RangeOfHours
  dataset$HourRange <- as.character(dataset$HourRange)
  for (value in dataset$RangeOfHours) {
    dataset$RangeOfHours <- as.character(dataset$RangeOfHours)
    if (!is.na(value) && (value != "NA") && (value != "N/A")) {
      if (grepl("/", value)) {
        aux <- strsplit(value, "/")
        range1Init <- strsplit(strsplit(as.character(aux[[1]][1]), "-")[[1]][1], ":")[[1]][1]
        range1End <- strsplit(strsplit(as.character(aux[[1]][1]), "-")[[1]][2], ":")[[1]][1]
        range2Init <- strsplit(strsplit(as.character(aux[[1]][2]), "-")[[1]][1], ":")[[1]][1]
        range2End <- strsplit(strsplit(as.character(aux[[1]][2]), "-")[[1]][2], ":")[[1]][1]

        range1 <- timeOfDay(range1Init, range1End)
        range2 <- timeOfDay(range2Init, range2End)
        ranges <- paste(range1, range2, sep="/")
        
        dataset$HourRange[dataset$HourRange == value] <- ranges

      } else if (grepl("-", value)) {
        aux <- strsplit(value, "-")
        rangeInit <- strsplit(as.character(aux[[1]][1]), ":")[[1]][1]
        rangeEnd <- strsplit(as.character(aux[[1]][2]), ":")[[1]][1]
        
        range <- timeOfDay(rangeInit, rangeEnd)
        
        dataset$HourRange[dataset$HourRange == value] <- range
      }
    }
  }
  dataset$HourRange[dataset$HourRange == "Never"] <- NA
  dataset$HourRange[dataset$HourRange == "Nunca"] <- NA
  dataset$HourRange[dataset$HourRange == "N/A"] <- NA
  dataset$HourRange[dataset$HourRange == "No one range"] <- NA
  dataset$HourRange[is.na(dataset$HourRange)] <- NA
  dataset$HourRange[dataset$HourRange == ""] <- NA
  dataset$HourRange[dataset$HourRange == "Long trips"] <- "All day"
  dataset$HourRange <- as.factor(dataset$HourRange)
  return(dataset)
}

newMobility <- parseRangeOfHours(newMobility)

parseRight <- function(dataset) {
  dataset$RightSlowerVehicles <- as.character(dataset$RightSlowerVehicles)
  dataset$RightSlowerVehicles[dataset$RightSlowerVehicles == ""] <- NA
  dataset$RightSlowerVehicles <- as.factor(dataset$RightSlowerVehicles)
  return(dataset)
}

newMobility <- parseRight(newMobility)

## Variable Clasification
# DriversLicense -> Categorical/Qualitative Nominal
# VehicleInProperty -> Categorical/Qualitative Nominal
# UsesNewMobility -> Categorical/Qualitative Nominal
# TypeOfNewMobility -> Categorical/Qualitative Nominal
# UsagePerMonth -> Numerical Continuous
# UsageTime -> Categorical/Qualitative Ordinal
# RangeOfHours/HourRange -> Categorical/Qualitative Nominal
# CostLastTrip -> Numerical Continuous
# RightSlowerVehicles -> Categorical/Qualitative Nominal
##

describeQualitative <- function(qualitative) {
  print('Table: ')
  print(table(qualitative))
  print('Size: ') 
  print(sum(table(qualitative)))
  print('Frequency table: ')
  print(table(qualitative)/sum(table(qualitative)))
  barplot(table(qualitative))
}

names(newMobility)

describeQualitative(newMobility$DriversLicense)
describeQualitative(newMobility$VehicleInProperty)
describeQualitative(newMobility$UseNewMobility)
describeQualitative(newMobility$TypeOfNewMobility)
describeQualitative(newMobility$HourRange)
describeQualitative(newMobility$RightSlowerVehicles)
describeQualitative(newMobility$UsageTime)


setwd("/Users/Jaime/Desktop/Master/Statistics fot Data Analysis/CourseProject/Data/")

#realNames = c("License", "VehicleInProperty", "UseOfNewMobility", "KindOfNewMobility", "RangeOfHours", "LastTripDuration", "LastTripCost", "SlowVehiclesInRoad")

newMobility = read.csv("NewMobility.csv", sep=";", dec=",")#, col.names = realNames)

parseUsageTime <- function(dataset){
  dataset$UsageTime <- dataset$X5Min
  dataset$UsageTime <- as.character(dataset$UsageTime)
  dataset[dataset$UsageTime == "Column 1",]$UsageTime <- as.numeric(5.0)
  dataset[dataset$X10Min == "Column 1",]$UsageTime <- as.numeric(10.0)
  dataset[dataset$X15Min == "Column 1",]$UsageTime <- as.numeric(15.0)
  dataset[dataset$X20Min == "Column 1",]$UsageTime <- as.numeric(20.0)
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
  dataset[dataset$TypeOfNewMobility == "",]$TypeOfNewMobility <- "No"
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
attach(newMobility)
#######################################################
## Variable Clasification
# DriversLicense -> Categorical/Qualitative Nominal
# VehicleInProperty -> Categorical/Qualitative Nominal
# UsesNewMobility -> Categorical/Qualitative Nominal
# TypeOfNewMobility -> Categorical/Qualitative Nominal
# UsagePerMonth -> Numerical Continuous
# UsageTime -> Numerical Discrete
# RangeOfHours/HourRange -> Categorical/Qualitative Nominal
# CostLastTrip -> Numerical Continuous
# RightSlowerVehicles -> Categorical/Qualitative Nominal
##

describeQualitative <- function(qualitative, name) {
  print(paste(name, 'Absolute Frequency Table: '))
  print(table(qualitative))
  print(paste(name, 'Size: ')) 
  print(sum(table(qualitative)))
  print(paste(name, 'Relative Frequency table: '))
  print(table(qualitative)/sum(table(qualitative)))
  barplot(table(qualitative), main=name)
}

names(newMobility)

describeQualitative(DriversLicense, "DriversLicense")
describeQualitative(VehicleInProperty, "VehicleInProperty")
describeQualitative(UseNewMobility, "UseNewMobility")
describeQualitative(TypeOfNewMobility, "TypeOfNewMobility")
describeQualitative(HourRange, "HourRange")
describeQualitative(RightSlowerVehicles, "RightSlowerVehicles")

describeDiscrete <- function(discrete, name) {
  print(paste(name, 'Absolut Frequency Table: '))
  print(table(discrete))
  print(paste(name, 'Size: '))
  print(sum(table(discrete)))
  print(paste(name, 'Relative Frequency Table: '))
  print(table(discrete)/sum(table(discrete)))
  print(paste(name, 'ECDF: '))
  plot(ecdf(discrete), main=name)
}

describeDiscrete(UsageTime, "UsageTime")

describeContinuous <- function(continuous, name) {
  print(paste(name, 'Absolute frequency histogram: '))
  auxHist <- hist(continuous, xlab = name)
  print('Size: ')
  print(sum(table(continuous)))
  print(paste(name, 'Absolute frequency: '))
  print(auxHist$counts)
  print('Relative frequency: ')
  print(auxHist$counts/sum(table(continuous)))
  print(paste(name, 'ECDF: '))
  plot(ecdf(continuous), main=name)
}

describeContinuous(CostLastTrip, "CostLastTrip")
describeContinuous(UsagePerMonth, "UsagePerMonth")

locationMeasures <- function(data, name) {
  print(paste(name, 'Mean: '))
  print(mean(data, na.rm=TRUE))
  print(paste(name, 'Median: '))
  print(median(data, na.rm=TRUE))
  print(paste(name, 'First Quartile: '))
  Q1 <- quantile(data, na.rm=TRUE, probs = 0.25)
  print(Q1)
  print(paste(name, 'Third Quartile: '))
  Q3 <- quantile(data, na.rm=TRUE, probs = 0.75)
  print(Q3)
  print(paste(name, 'InterQuartile Range: '))
  IQR <- Q3-Q1
  print(IQR)
  print(paste(name, 'Outliers below: '))
  print(Q1-1.5*IQR)
  print(paste(name, 'Outliers above: '))
  print(Q3+1.5*IQR)
  boxplot(data, main=name)
}

locationMeasures(CostLastTrip, "CostLastTrip")
locationMeasures(UsageTime, "UsageTime")

library(e1071)

dispersionMeasures <- function(data, name) {
  #print(data)
  print(paste(name, 'Variance: '))
  print(var(data, na.rm=TRUE))
  print(paste(name, 'Standard deviation: '))
  print(sd(data, na.rm=TRUE))
  print(paste(name, 'Sample median absolute deviation: '))
  print(mad(data, na.rm=TRUE))
  print(paste(name, 'Coefficient of variation: '))
  print(sd(data, na.rm=TRUE)/mean(data, na.rm=TRUE))
  print(paste(name, 'Skewness coeff: '))
  print(skewness(data, na.rm = TRUE))
  if (!is.nan(skewness(data, na.rm = TRUE))) {
    if (skewness(data, na.rm = TRUE) < 0) {
      print('Left skewness')
    } else if (skewness(data, na.rm = TRUE) > 0) {
      print('Right skewness')
    }
  }
}

dispersionMeasures(CostLastTrip, "CostLastTrip")
dispersionMeasures(UsageTime, "UsageTime")

## Both Qualitative Analysis
# DriversLicense vs VehicleInProperty
# DriversLicense vs UseNewMobility
# DriversLicense vs TypeOfNewMobility
# DriversLicense vs RightSlowerVehicles
# DriversLicense vs HourRange
# VehicleInProperty vs UseNewMobility
# VehicleInProperty vs TypesOfNewMobility
# VehicleInProperty vs RightSlowerVehicles
# VehicleInProperty vs HourRange
# UseNewMobility vs TypeOfNewMobility
# UseNewMobility vs RightSlowerVehicles
# UseNewMobility vs HourRange
# TypeOfNewMobility vs RightSlowerVehicles
# TypeOfNewMobility vs HourRange
# RightSlowerVehicles vs HourRange

## TODO: Add Group bar plots
bothQualitative <- function(qual1, qual2, name1, name2) {
  n <- 1
  if (sum(table(qual1)) == sum(table(qual2))) {
    n <- sum(table(qual1))
  } else if (sum(table(qual1)) < sum(table(qual2))) {
    n <- sum(table(qual1))
  } else {
    n <- sum(table(qual2))
  }
  tableAux <- table(qual1, qual2, dnn=c(name1, name2))
  print(paste('Joint Absolute Frequency for', name1, 'and', name2, ': '))
  print(tableAux)
  print(paste('Joint Relative Frequency for', name1, 'and', name2, ': '))
  print(tableAux/n)
  barplot(tableAux, beside=TRUE, legend = rownames(tableAux), ylab=name1, xlab=name2)
}

bothQualitative(DriversLicense, VehicleInProperty, "DriversLicense", "VehicleInProperty")
bothQualitative(DriversLicense, UseNewMobility, "DriversLicense", "UseNewMobility")
# There are people that drive a car not having a drivers license somehow...
bothQualitative(DriversLicense, TypeOfNewMobility, "DriversLicense", "TypeOfNewMobility")
bothQualitative(DriversLicense, RightSlowerVehicles, "DriversLicense", "RightSlowerVehicles")
##bothQualitative(RightSlowerVehicles, DriversLicense, "RightSlowerVehicles", "DriversLicense")
# This one doesn't really represent anything...
bothQualitative(DriversLicense, HourRange, "DriversLicense", "HourRange")
bothQualitative(VehicleInProperty, UseNewMobility, "VehicleInProperty", "UseNewMobility")
bothQualitative(VehicleInProperty, TypeOfNewMobility, "VehicleInProperty", "TypeOfNewMobility")
# This one is interesting for some analysis
bothQualitative(VehicleInProperty, RightSlowerVehicles, "VehicleInProperty", "RightSlowerVehicles")
# Don't really know what could we get out of this one
bothQualitative(VehicleInProperty, HourRange, "VehicleInProperty", "HourRange")
# This shows that there might be people that don't know how to read... 
bothQualitative(UseNewMobility, TypeOfNewMobility, "UseNewMobility", "TypeOfNewMobility")
bothQualitative(UseNewMobility, RightSlowerVehicles, "UseNewMobility", "RightSlowerVehicles")
# This one doesn't really represent anything, since only cares about people who use new mobility
bothQualitative(UseNewMobility, HourRange, "UseNewMobility", "HourRange") #### Delete
# Doesn't make sense to me... why would there be people that use bikes and mopads and think that they shouldn't go in the road?
bothQualitative(TypeOfNewMobility, RightSlowerVehicles, "TypeOfNewMobility", "RightSlowerVehicles")
# This table is too big to show anything
bothQualitative(TypeOfNewMobility, HourRange, "TypeOfNewMobility", "HourRange")

#### Conditional Frequency Tables?? We've have to think which ones are of interest

## One qualitative, one quantitative --> Box plots, Histograms and Summary Statistics conditioning
# on the different values of the qualitative variable

oneQoneQ <- function(qual, quan, name1, name2) {
  boxplot(quan ~ qual, xlab=name1, ylab=name2)
  for (uniq in unique(qual)) {
    if (!is.na(uniq)) {
      print(paste(name1, uniq, name2))
      print(length(quan[qual==uniq]))
      hist(quan[qual==uniq], main=paste(name1, uniq), xlab=name2)
      describeContinuous(quan[qual==uniq], paste(name1,'==',uniq,name2))
      locationMeasures(quan[qual==uniq], paste(name1,'==',uniq,name2))
      dispersionMeasures(quan[qual==uniq], paste(name1,'==',uniq,name2))
    }
  }
}

### The following analysis are considering only people who use new mobility services!!!

## Given DriversLicense (Yes/No): 
# CostLastTrip (Who spends more?)
# UsagePerMonth (Who uses it more often?)
# UsageTime (Who uses it more time?)

oneQoneQ(DriversLicense, CostLastTrip, 'DriversLicense', 'CostLastTrip')
oneQoneQ(DriversLicense, UsagePerMonth, 'DriversLicense', 'UsagePerMonth')
oneQoneQ(DriversLicense, UsageTime, 'DriversLicense', 'UsageTime')

## Given TypesOfNewMobility != 'I don't use any': [car vs motorbike vs bike/mopad]
# CostLastTrip (Who spends more?) 
# UsagePerMonth (Who uses it more often?)
# UsageTime (Who uses it more time?)

## Somehow there's someone that doesn't use new mobility services but has spent some money... !!!!
oneQoneQ(TypeOfNewMobility[TypeOfNewMobility != "No"], CostLastTrip[TypeOfNewMobility != "No"], 'TypeOfNewMobility', 'CostLastTrip')
## Probably too much people who uses a car has said that their last trip was 0 euros...
oneQoneQ(TypeOfNewMobility[TypeOfNewMobility != "No"], UsagePerMonth[TypeOfNewMobility != "No"], 'TypeOfNewMobility', 'UsagePerMonth')
oneQoneQ(TypeOfNewMobility[TypeOfNewMobility != "No"], UsageTime[TypeOfNewMobility != "No"], 'TypeOfNewMobility', 'UsageTime')

## Given VehicleInProperty:
# CostLastTrip (Who spends more?) 
# UsagePerMonth (Who uses it more often?)
# UsageTime (Who uses it more time?)

oneQoneQ(VehicleInProperty, CostLastTrip, 'VehicleInProperty', 'CostLastTrip')
oneQoneQ(VehicleInProperty, UsagePerMonth, 'VehicleInProperty', 'UsagePerMonth')
oneQoneQ(VehicleInProperty, UsageTime, 'VehicleInProperty', 'UsageTime')

## Given HourRange: (this one could be a problem since there are NOT ENOUGH SAMPLES)
# CostLastTrip (Who spends more?) 
# UsagePerMonth (Who uses it more often?)
# UsageTime (Who uses it more time?)

## Given RightSlowerVehicles:
# CostLastTrip (Who spends more?) 
# UsagePerMonth (Who uses it more often?)
# UsageTime (Who uses it more time?)

oneQoneQ(RightSlowerVehicles, CostLastTrip, 'RightSlowerVehicles', 'CostLastTrip')
oneQoneQ(RightSlowerVehicles, UsagePerMonth, 'RightSlowerVehicles', 'UsagePerMonth')
oneQoneQ(RightSlowerVehicles, UsageTime, 'RightSlowerVehicles', 'UsageTime')

### Both Quantitative 
##### There's a problem when analyzing thow quantitative variables which is that 
##### covariance and correlation can't be properly calculated because there are 
##### two many NA in this variables...
# Sample covariance
# Sample correlation
# Scatter plots
### CostLastTrip vs UsageTime vs UsagePerMonth

bothQuantitative <- function(quan1, quan2, name1, name2) {
  plot(quan1, quan2, xlab = name1, ylab = name2)
}

bothQuantitative(CostLastTrip, UsagePerMonth, 'CostLastTrip', 'UsagePerMonth')
bothQuantitative(CostLastTrip, UsageTime, 'CostLastTrip', 'UsageTime')
bothQuantitative(UsagePerMonth, UsageTime, 'UsagePerMonth', 'UsageTime')




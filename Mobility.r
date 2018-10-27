setwd("/Users/Jaime/Desktop/Master/Statistics fot Data Analysis/CourseProject/Data/")

#realNames = c("License", "VehicleInProperty", "UseOfNewMobility", "KindOfNewMobility", "RangeOfHours", "LastTripDuration", "LastTripCost", "SlowVehiclesInRoad")

newMobility = read.csv("NewMobility_v2.csv", sep=";", dec=",")#, col.names = realNames)
attach(newMobility)
parseUsageTime <- function(dataset){
  dataset$UsageTime <- dataset$X5Min
  dataset$UsageTime <- as.character(dataset$UsageTime)
  #dataset[dataset$X5Min == "Column 1",]$UsageTime <- as.numeric(5.0)
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
  #dataset[dataset$TypeOfNewMobility == "",]$TypeOfNewMobility <- "No"
  dataset[dataset$TypeOfNewMobility == "I don't use any new mobility services",]$TypeOfNewMobility <- "No"
  dataset[dataset$TypeOfNewMobility == "Carsharing",]$TypeOfNewMobility <- "Car"
  dataset[dataset$TypeOfNewMobility == "Motosharing",]$TypeOfNewMobility <- "Moto"
  dataset[dataset$TypeOfNewMobility == "Other (Lime, BiciMAD, etc)",]$TypeOfNewMobility <- "Other"
  dataset$TypeOfNewMobility <- as.factor(dataset$TypeOfNewMobility)
  return(dataset)
}

newMobility <- parseTypeOfNewMobility(newMobility)
attach(newMobility)
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
  print(dataset$CostLastTrip)
  dataset$CostLastTrip <- as.numeric(sub(",", ".",dataset$CostLastTrip))
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
  plot(ecdf(discrete), main=name, col="#00146E", xlab = "Time (min)", ylab="Fn(Time)")
  hist(discrete, main=name, col="#00146E", xlab="Time Range", ylab="Frequency", breaks = 4)
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
  plot(ecdf(continuous), main=name, xlab='Number of Times', ylab='Fn(Number of Times)', col = "#00146E")
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
locationMeasures(UsagePerMonth, "UsagePerMonth")

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
  barplot(tableAux, beside=TRUE, legend = rownames(tableAux), ylab='Frequency', xlab=name2, args.legend=list(
    x=ncol(tableAux) + 4.5,
    y=max(colSums(tableAux))-20,
    bty = "n"
  ), col = c("#00146E", "#00249B", "#0029AF", "#0030CE"), main=paste(name1, 'VS', name2))
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
  boxplot(quan ~ qual, xlab=name1, ylab=name2, col="#00146E", main=paste(name1, 'vs', name2))
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

bothQuantitative(UsagePerMonth, CostLastTrip, 'UsagePerMonth', 'CostLastTrip')
bothQuantitative(UsageTime, CostLastTrip, 'UsageTime', 'CostLastTrip')
bothQuantitative(UsagePerMonth, UsageTime, 'UsagePerMonth', 'UsageTime')

### TODO:
# Mirar AIC Test
# Razonar por que MLE es "mejor" que MME
# Como tenemos un sample size grande, utilizamos el CLT (Central Limit Theorem) para la inferencia
library(fitdistrplus)

## CostLastTrip > Probability Density Function Fit
# Exponential Method of Moments
fitExp = fitdist(CostLastTrip[!is.na(CostLastTrip)],"exp",method="mme")
hist(CostLastTrip[!is.na(CostLastTrip)], freq = F, ylim = c(0,0.12))#, nclass = 3)
grid=seq(0,30,0.012)
lines(grid,dexp(grid,fitExp$estimate[1]),col="red")
## We should be using for the gamm distribution the maximum likelihood method
############
# Gamma Method of Moments
fitGamma = fitdist(CostLastTrip[!is.na(CostLastTrip)],"gamma",method = "mme")
lines(grid,dgamma(grid,fitGamma$estimate[1],fitGamma$estimate[2]),col="blue")
# Gamma Maximum Likelihood Estimation Method
fitGammaMle = fitdist(CostLastTrip[!is.na(CostLastTrip)],"gamma",method="mle")
lines(grid,dgamma(grid,fitGammaMle$estimate[1],fitGammaMle$estimate[2]),col="green")
## AIC


## CostLastTrip > Distribution Function graphs
# Empirical
plot(ecdf(CostLastTrip[!is.na(CostLastTrip)]))
# Exponential
lines(grid,pexp(grid,fitExp$estimate[1]),col="red")
# Gamma Method of Moments
lines(grid,pgamma(grid,fitGamma$estimate[1],fitGamma$estimate[2]),col="blue")
# Gamma Maximum Likelihood Estimation Method
lines(grid,pgamma(grid,fitGammaMle$estimate[1],fitGammaMle$estimate[2]),col="green")
## AIC


UsagePerMonthAux <- UsagePerMonth[UsagePerMonth!=0.0&!is.na(UsagePerMonth)]
## UsagePerMonth > Probability Density Function Fit
hist(UsagePerMonthAux, freq = F, ylim = c(0,0.2))
# Exponential by Method of Moments
fitExp2 = fitdist(UsagePerMonthAux,"exp",method = "mme")
grid2 = seq(0,30,0.01)
lines(grid,dexp(grid,fitExp2$estimate[1]),col="red")
# Gamma by Maximum Likelihood Estimation Method
fitGamma2 = fitdist(UsagePerMonthAux,"gamma",method="mle")
lines(grid,dgamma(grid,fitGamma2$estimate[1],fitGamma2$estimate[2]),col="blue")
## AIC


hist(UsagePerMonthAux[UsagePerMonthAux < 15], freq = F, ylim=c(0,0.4))
fitExp3 = fitdist(UsagePerMonthAux[UsagePerMonthAux < 15],"exp",method="mme")
grid3 = seq(0,15,0.01)
lines(grid3,dexp(grid3,fitExp3$estimate[1]),col="red")
fitGamma3 = fitdist(UsagePerMonthAux[UsagePerMonthAux < 15], "gamma", method = "mle")
lines(grid3,dgamma(grid3,fitGamma3$estimate[1],fitGamma3$estimate[2]), col="blue")
fitGamma3MME = fitdist(UsagePerMonthAux[UsagePerMonthAux < 15], "gamma", method = "mme")
lines(grid3,dgamma(grid3,fitGamma3MME$estimate[1],fitGamma3MME$estimate[2]), col="green")
## AIC


plot(ecdf(UsagePerMonthAux))
lines(grid2,pexp(grid2,fitExp2$estimate[1]),col="red")
lines(grid2,pgamma(grid2,fitGamma2$estimate[1],fitGamma2$estimate[2]),col="blue")

plot(ecdf(UsagePerMonthAux[UsagePerMonthAux < 15]))
lines(grid3,pexp(grid3,fitExp3$estimate[1]),col="red")
lines(grid3,pgamma(grid3,fitGamma3$estimate[1],fitGamma3$estimate[2]),col="blue")


t.test(CostLastTrip[!is.na(CostLastTrip)])
costMeanExample = 4.25
# Hemos obtenido este valor de la meadia de una noticia en Internet que decia que estaba entre 3,5 y 5 el gasto
t.test(CostLastTrip[!is.na(CostLastTrip)], mu = costMeanExample, alternative="g")
# Since p is very small we can reject H0, and therefore we know H1 is true (The mean is greater than the one in the article).



names(newMobility)
table(TypeOfNewMobility,RightSlowerVehicles)
table(TypeOfNewMobility,RightSlowerVehicles)/126
chisq.test(TypeOfNewMobility,RightSlowerVehicles)
# We can't reject H0 (b/c p-value > 0.05) => We know nothing
chisq.test(TypeOfNewMobility,RightSlowerVehicles)$observed
# Si las variables fuesen independientes, tendrian estos valores
chisq.test(TypeOfNewMobility,RightSlowerVehicles)$expected

Chi = chisq.test(TypeOfNewMobility,RightSlowerVehicles)$statistic
N = 126
V = sqrt(Chi/N)
# As expected V is not close to one and therefore the variables do not have a strong dependency
V

table(VehicleInProperty,RightSlowerVehicles)
chisq.test(VehicleInProperty,RightSlowerVehicles)
# This p-value is small and therefore we can reject H0 (Y1 and Y2 being independent) => H1 is true = (Y1 and Y2 are dependent)
chisq.test(VehicleInProperty,RightSlowerVehicles)$observed
chisq.test(VehicleInProperty,RightSlowerVehicles)$expected
Chi2 = chisq.test(VehicleInProperty,RightSlowerVehicles)$statistic
V2 = sqrt(Chi2/N)
# Since V Coefficient is not close to one, this means that the variable dependency is NOT strong
# F(X,Y) = F(X)F(Y)
### H0 -> Independientes => F(X,Y) = F(X)F(Y)
### H1 -> Dependientes
V2

table(TypeOfNewMobility,UseNewMobility)
chisq.test(TypeOfNewMobility,UseNewMobility)
chisq.test(TypeOfNewMobility,UseNewMobility)$expected
Chi3 = chisq.test(TypeOfNewMobility,UseNewMobility)$statistic
N = 134
V3 = sqrt(Chi3/N)
V3


names(newMobility)
## RightSlowerVehicles does NOT matter when looking at Costs
Y1 = CostLastTrip[RightSlowerVehicles=="Yes"]
Y2 = CostLastTrip[RightSlowerVehicles=="No"]
boxplot(log(Y1),log(Y2))
#t.test(Y1,Y2,var.equal = T, alternative = "g")
t.test(log(Y1),log(Y2),var.equal = F, alternative = "l")

YCars = CostLastTrip[TypeOfNewMobility=="Car"]
YNotCar = c(CostLastTrip[TypeOfNewMobility=="Moto"], CostLastTrip[TypeOfNewMobility=="Other"])
boxplot(log(YCars),log(YNotCar))
t.test(log(YCars),log(YNotCar),var.equal = F, alternative = "g")
# If instead of using a confidence interval of 0.95 we were to use a confidence interval of 0.92, we could reject H0 (mu0 = mu1) and therefore we could say that we've found statistical evidence for H1 (mu0>mu1)

YHasVehicle = CostLastTrip[VehicleInProperty=="Yes"]
YNotHasVehicle = CostLastTrip[VehicleInProperty=="No"]
boxplot(log(YHasVehicle),log(YNotHasVehicle))
t.test(log(YHasVehicle),log(YNotHasVehicle),var.equal = F,alternative = "g")
# In this case we cannot reject H0 and therefore we know nothing

TypeOfNewMobility2 <- factor(TypeOfNewMobility[TypeOfNewMobility!="No"])
CostLastTrip2 <- as.numeric(factor(CostLastTrip[TypeOfNewMobility!="No"]))

boxplot(log(CostLastTrip2)~TypeOfNewMobility2)
summary(aov(log(CostLastTrip2)~TypeOfNewMobility2))
# Hay que acordarse de hacer alusi??n Coste dado que usas coche / Coste dado que NO usas coche
### TODO
# Hacer ANOVA para UsagePerMonth TypeOfNewMobility




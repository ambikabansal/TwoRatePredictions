
source("R/ProcessTabletData.R")
source("R/twoRates_tablet.R")

#ANOVA for Rebound

library(ez)
getReboundANOVA <- function(group) {
  
  reboundData <- read.csv(file = sprintf ('data/processedData/rebound_%s.csv', group), stringsAsFactors = FALSE)
  reboundData$participant <- as.factor(reboundData$participant)
  reboundData$condition <- as.factor(reboundData$condition)
  anova <- ezANOVA(data = reboundData, dv = rebound, wid = participant, within = c(condition), type=3)
  print(anova)
  
}


#convert reach deviations to percentage of adaptation

getPercentageOfAdaptation <- function() {
  
  allData <- loadAllData()
  
  for (perturbation in c('abrupt','gradual')) {
    
    conditionname <- sprintf("%s",perturbation)
    df <- allData [[conditionname]]
    

    for (part in names(participantdata)) {
      participantdata <- allData (participant=participant)
      data <- participantdata [[part]]
      data <- getConvertedReachAngles(data)
      
      conditionname <- ifelse(data$rotation[which(data$block ==2)[1]] == 0, 'gradual', 'abrupt')

      
      if (conditionname %in% names(allData)) {
        cat('appending data to existing data frame\n')
        df <- allData [[conditionname]]
        df[sprintf("p%03d", participant)] <- data$reachdeviation
        allData [[conditionname]] <- df
        
      } else {
        # create data frame with: block / trial / rotation and converted reach deviation with participant ID
        df <- subset(data,select = c("block","trial","rotation"))
        df[sprintf("p%03d", participant)] <- data$reachdeviation
        
        allData [[conditionname]] <- df
      }
      
    }
  }
  
    #save data
    for (conditionname in names(allData)) {
      
      df <- allData [[conditionname]]
      write.csv(df, file = sprintf ('data/processedData/%s_PercentageOfAdaptation_30_tablet.csv',conditionname), quote = FALSE, row.names = FALSE)
    
  }
  
}


getConvertedReachAngles <- function(df) {
  
  targetangles <- data$reachdeviation
  data$reachdeviation <- targetangles*df$rotation
  
}




#polynomial logistic regression

pLogRegression <- function(group) {
  
  df <- read.csv('data/Tablet_data/60-deg_rawprocessing/rebound.csv', stringsAsFactors = F)
  
  df$condition <- as.factor(df$condition)
  
  print(summary(glm(formula = condition ~ Rs + Ls + Rf + Lf, family = binomial(link = "logit"), 
                    data = df)))
  
  
}


#polynomial logistic regression between 30 and 60 degree rotation

pLogRegressionMag <- function() {
  
  df <- read.csv('data/30_60_gradual.csv', stringsAsFactors = F)
  
  df$condition <- as.factor(df$condition)
  
  print(summary(glm(formula = condition ~ Rs + Ls + Rf + Lf, family = binomial(link = "logit"), 
                    data = df)))
  
  
}


#Tablet vs. VR ANOVA

library(ez)
getSetupANOVA <- function() {
  
  reboundData <- read.csv(file = sprintf ('data/tablet_VR_ANOVA.csv'), stringsAsFactors = FALSE)
  reboundData$participant <- as.factor(reboundData$participant)
  reboundData$condition <- as.factor(reboundData$condition)
  anova <- ezANOVA(data = reboundData, dv = rebound, wid = participant, between = c(condition), type=3)
  print(anova)
  
}


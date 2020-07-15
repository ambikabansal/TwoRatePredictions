source("R/ProcessTabletData.R")
source("R/twoRates_tablet.R")

#plotting perturbation schedule

plotRotationSchedule <- function(reversaltrials = 12, reversalmagnitude = -30, errorclampmagnitude = 0) {
  
  alignedtrials <- 32
  rotatedtrials <- 100
  rotatedmagnitude <- 30
  errorclamptrials <- 20
  #errorclampmagnitude <- 0
  

#setting (x,y) points 
  
  x <- c(1,
         alignedtrials +1, 
         alignedtrials +1,
         alignedtrials + rotatedtrials +1,
         alignedtrials + rotatedtrials +1,
         alignedtrials + rotatedtrials + reversaltrials+ 1,
         alignedtrials + rotatedtrials + reversaltrials+ 1,
         alignedtrials + rotatedtrials + reversaltrials + errorclamptrials)

  y <- c(0,
         0,
         rotatedmagnitude,
         rotatedmagnitude,
         reversalmagnitude,
         reversalmagnitude,
         errorclampmagnitude,
         errorclampmagnitude)

#creating plots
  
  lines(x,y,col='black')
  axis(1,c(1,32,132,144,164),las=2)
  axis(2,c(-30,-15,0,15,30))
}


plotGradualSchedule <- function(reversaltrials = 12, reversalmagnitude = -30, errorclampmagnitude = 0) {
  
  alignedtrials <- 32
  rotatedtrials <- 100
  rotatedmagnitude <- 30
  errorclamptrials <- 20
  
  #setting (x,y) points 
  
  x <- c(1,
         alignedtrials +1, 
         alignedtrials +1 +40,
         alignedtrials + rotatedtrials +1,
         alignedtrials + rotatedtrials +1,
         alignedtrials + rotatedtrials + reversaltrials+ 1,
         alignedtrials + rotatedtrials + reversaltrials+ 1,
         alignedtrials + rotatedtrials + reversaltrials + errorclamptrials)
  
  y <- c(0,
         0,
         rotatedmagnitude,
         rotatedmagnitude,
         reversalmagnitude,
         reversalmagnitude,
         errorclampmagnitude,
         errorclampmagnitude)
  
  #creating plots
  
  lines(x,y,col='black')
  axis(1,c(1,32,132,144,164),las=2)
  axis(2,c(-30,-15,0,15,30))
}



#plotting behavioural data

library(svglite)

plotAllData <- function(group, target='inline') {
  
  if (target == 'svg') {
    svgfile <- sprintf('data/allReaches_%s.svg',group)
    svglite(file=svgfile, width=7, height=4, system_fonts=list(sans = "Arial"))
  }
  
  allData <- loadAllData()
  
  par(mar=c(4,4,2,1), mfrow=c(1,2), las = 1)
  
  for (perturbation in c('abrupt','gradual')){
    
      conditionname <- sprintf("%s",perturbation)
      
      df <- allData [[conditionname]]

#creating plots
      
      plot(-1000,-1000, main = "data", xlim = c(0,165), ylim = c(-80,80), xlab = "", ylab = "",bty='n', ax = FALSE)
      
      if (perturbation == 'abrupt') {
        plotRotationSchedule()
        } else { plotGradualSchedule()
        }
      
      plotReachDeviations(df)
      
#plotting two-rate model
      
      reaches <- getMedianReachDeviations(df)
      par <- twoRateModelFit(schedule = df$rotation*-1, reaches = reaches)
      twoRate <- twoRateModel(par = par, schedule = df$rotation*-1)
      lines(twoRate$slow,col='#ff8200ff')
      lines(twoRate$fast,col='#0087FF')
      lines(twoRate$total,col='#e51636ff')
      
      
      #plot(-1000,-1000, main = "model", bty = 'n')
      #plotRotationSchedule(reversaltrials = duration, reversalmagnitude = magnitude)
      
  }
  
  if (target %in% c('svg')) {
    dev.off()
  }
  
}


plotReachDeviations <- function(df) {
  
  #plotting median reach deviations
  
    medianReachDeviations <- getMedianReachDeviations(df)
    lines(medianReachDeviations, col='#8266f4ff')
  
  
  #plotting mean reach deviations
  
  # meanReachDeviations <- getMeanReachDeviations(df)
  # lines(meanReachDeviations)
  
  
  #plotting individual reach deviations

  # df[1:3] <- NULL
  # 
  # for (participant in df) {
  #   print(participant)
  #   lines(participant, col='#ff82002f')
  #}
  
  
  
  #plotting 95% confidence intervals
  
  reachCI <- getReachCI(df)

  x <- c(1:nrow(df),rev(1:nrow(df)))
  y <- c(reachCI$hi,rev(reachCI$lo))

  polygon(x,y, col = "#ff00ff22", border=NA)
  
}


#getting reach median deviations
  
  getMedianReachDeviations <- function(df) {
    
    
    allmedianreaches <- c()
    df[1:3] <- NULL
    for (rowindex in c(1:nrow(df))) {
      medianreach <- median(as.numeric(df[rowindex, ]), na.rm = TRUE)
      allmedianreaches <- c(allmedianreaches, medianreach)   
    }
    
    return(allmedianreaches)
    
  }


#getting reach mean deviations

getMeanReachDeviations <- function(df) {
  
  
  allmeanreaches <- c()
  df[1:3] <- NULL
  for (rowindex in c(1:nrow(df))) {
    meanreach <- mean(as.numeric(df[rowindex, ]), na.rm = TRUE)
    allmeanreaches <- c(allmeanreaches, meanreach)   
  }
  
  return(allmeanreaches)
  
}


#used to get 95% CI

getConfidenceInterval <- function(data, variance = var(data), conf.level = 0.95, method='t-distr', resamples=1000, FUN=mean) {
  
  if (method %in% c('t-distr','t')) {
    
    z = qt((1 - conf.level)/2, df = length(data) - 1, lower.tail = FALSE)
    
    xbar = mean(data)
    sdx = sqrt(variance/length(data))
    
    return(c(xbar - z * sdx, xbar + z * sdx))
    
  }
  
  if (method %in% c('bootstrap','b')) {
    
    data <- data[which(is.finite(data))] #need is.finite due to NA values
    
    samplematrix <- matrix(sample(data, size = resamples*length(data), replace = TRUE), nrow = resamples)
    #apply mean function to this new matrix
    BS <- apply(samplematrix, c(1), FUN=FUN) 
    
    lo <- (1-conf.level)/2.
    hi <- 1 - lo
    
    return(quantile(BS, probs = c(lo,hi)))
  }
}



#getting Confidence Interval from Data

getReachCI <- function(df) {
  
  #removing first 3 coloumns of data that we don't need 
  df[1:3] <- NULL
  
  lo <- c()
  hi <- c()
  
  for (trial in c(1:nrow(df))) {
    CI <- as.numeric(df[trial,]) 
    CI <- CI[!is.na(CI)] #removing NA
    confint <- getConfidenceInterval(data=CI)
    #allreachCI <- c(allreachCI, confint)   
    
    lo <- c(lo, confint[1])
    hi <- c(hi, confint[2])
  }

  allreachCI <- data.frame(lo,hi)
  return(allreachCI)
}


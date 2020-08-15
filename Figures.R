source("orderEffects.R")
source("R/twoRates_tablet.R")
source("ExtentofLearning.R")
library(svglite)

plotOrderData <- function(group) {
  
  par(mar=c(4,4,2,1), mfrow=c(1,3), las = 1)
  
  #load data
  groups <- c('tablet30', 'tablet60', 'VR30')
  
  for (group in groups) {
    df <- orderData(group)
    plot(-1000,-3000, main = sprintf('%s', group), xlim = c(0,7), ylim = c(0,1), xlab = "", ylab = "normalized reach deviation", bty='n', ax = FALSE)
    
    #grab reach deviations for each block averaged across participants for both abrupt and gradual conditions
    ordernum <- c(1,2)
    blocks <- c('first', 'second', 'last')
    
    for (order in ordernum) {
        for (block_idx in c(1:length(blocks))) { 
          blockname <- blocks[block_idx] 
          meanreachdevs <- mean(df$reachdeviation [which(df$block==blockname & df$order==order)], na.rm=TRUE)
          
          spacing <- (block_idx - 1) * 2.5 + (order - 1)
          colour <- c('#ff8200ff', '#0087FF')[order]

        #create plots
        barplot(width=1, height=meanreachdevs, space = spacing, beside = TRUE, col = colour, main = sprintf('%s', group), xlim = c(0,10), ylim = c(0,5), xlab = "block", ylab = "", axes = TRUE, add = TRUE)
      }
    }
    
    axis(side = 1, at = c(1, 3.5, 6), labels = blocks)
    
  }
}
  


plotExtentofLearning <- function(group) {
  
  par(mar=c(4,4,2,1), mfrow=c(1,3), las = 1)
  
  #load data
  groups <- c('tablet30', 'tablet60', 'VR30')
  
  for (group in groups) {
    df <- getExtentofLearning(group)
    plot(-1000,-3000, main = sprintf('%s', group), xlim = c(0,5), ylim = c(-1,1), xlab = "", ylab = "normalized reach deviation", bty='n', ax = FALSE)
    
    #grab reach deviations for each block averaged across participants for both abrupt and gradual conditions
    
    conditions <- c('abrupt', 'gradual')
    blocks <- c('last training', 'last reversal')
    
    for (condition_idx in c(1:length(conditions))) {
      for (block_idx in c(1:length(blocks))) { 
        condition <- conditions[condition_idx]
        blockname <- blocks[block_idx] 
        meanreachdevs <- mean(df$reachdeviation [which(df$block==blockname & df$condition==condition)], na.rm=TRUE)
        
        spacing <- (block_idx- 1) * 2.5 + (condition_idx- 1)
        colour <- c('#ff8200ff', '#0087FF')[condition_idx]
        
        #create plots
        barplot(width=1, height=meanreachdevs, space = spacing, beside = TRUE, col = colour, main = sprintf('%s', group), xlim = c(0,10), ylim = c(0,5), xlab = "block", ylab = "", axes = TRUE, add = TRUE)
      }
    }
    
    axis(side = 1, at = c(1, 3.5), labels = blocks)
    
  }
}




plotReboundData <- function(group) {
  
  par(mar=c(4,4,2,1), mfrow=c(1,3), las = 1)
  
  #load data
  
  groups <- c('tablet30', 'tablet60', 'VR30')
  
  for (group in groups) {
    df <- read.csv(file = sprintf ('data/processedData/%s_tworates_new.csv', group), stringsAsFactors = FALSE)
    plot(-1000,-3000, main = sprintf('%s', group), xlim = c(0,5), ylim = c(0,0.4), xlab = "", ylab = "normalized reach deviation", bty='n', ax = FALSE)
    
    #grab reach deviations averaged across participants for both abrupt and gradual conditions
    
    conditions <- c('abrupt', 'gradual')
    
      for (condition_idx in c(1:length(conditions))) { 
        condition <- conditions[condition_idx] 
        meanrebound <- mean(df$rebound [which(df$condition==condition)], na.rm=TRUE)
        colour <- c('#ff8200ff', '#0087FF')[condition_idx]
        spacing <- (condition_idx - 1)
      
        #create plots
        barplot(width=1, height=meanrebound, space = spacing, beside = TRUE, col = colour, main = sprintf('%s', group), xlim = c(0,10), ylim = c(0,5), xlab = "", ylab = "", axes = TRUE, add = TRUE)
    }
  }
}
     
     
    
     
    
     #plotting perturbation schedule
     
     plotRotationSchedule <- function(reversaltrials = 12, reversalmagnitude = -1, errorclampmagnitude = 0) {
       
       alignedtrials <- 32
       rotatedtrials <- 100
       rotatedmagnitude <- 1
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
       axis(2,c(-1,0,1))
     }
     
     
     plotGradualSchedule <- function(reversaltrials = 12, reversalmagnitude = -1, errorclampmagnitude = 0) {
       
       alignedtrials <- 32
       rotatedtrials <- 100
       rotatedmagnitude <- 1
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
       axis(2,c(-1,-0, 1))
     }
     
     
     
     
     

plotAllData <- function(group, target='inline') {
       
       if (target == 'svg') {
         svgfile <- sprintf('data/allReaches_%s.svg',group)
         svglite(file=svgfile, width=7, height=4, system_fonts=list(sans = "Arial"))
       }
       
       par(mar=c(4,4,2,1), mfrow=c(3,2), las = 1)
       
       groups <- c('tablet30', 'tablet60', 'VR30')
       conditions <- c('abrupt', 'gradual')
       
       for (group in groups) {
         for (condition in conditions) {
           df <- read.csv(sprintf('data/processedData/%s_%s_new.csv', group, condition), stringsAsFactors=F)
        
         #creating plots
         
         plot(-1000,-3000, main = sprintf('%s %s', group, condition), xlim = c(0,165), ylim = c(-1.2,1.2), xlab = "trial", ylab = "normalized reach deviation", bty='n', ax = FALSE)
         
         if (condition == 'abrupt') {
           plotRotationSchedule()
         } else { plotGradualSchedule()
         }
         
         plotReachDeviations(df)
         
         #plotting two-rate model
         reaches <- getMedianReachDeviations(df)
         par <- twoRateModelFit(schedule = df$rotation, reaches = reaches)
         twoRate <- twoRateModel(par = par, schedule = df$rotation)
         lines(twoRate$slow,col='#ff8200ff')
         lines(twoRate$fast,col='#0087FF')
         lines(twoRate$total,col='#e51636ff')
         
       #}
       
       if (target %in% c('svg')) {
         dev.off()
       }
         }
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
       
       #print(str(df))
       allmedianreaches <- c()
       df[1:2] <- NULL
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
     
     
     
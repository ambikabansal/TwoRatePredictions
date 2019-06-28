source("R/ProcessParticipantData.R")
source("R/twoRates.R")

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

library(svglite)

plotAllData <- function() {
  
  svglite(file='example_twoRate.svg', width=7, height=4, system_fonts=list(sans = "Arial"))
  
  par(mar=c(4,4,2,1), mfrow=c(1,1), las = 1)
  
  filename <- sprintf("data/Pilot/12trial-30deg.csv")
  df <- read.csv(filename)

  plot(-1000,-1000, main = "fig", xlim = c(0,165), ylim = c(-30,35), xlab = "Trial", ylab = "Angular Deviation",bty='n', ax=FALSE)   
  plotRotationSchedule()
  
  par <- c("Ls" = .05,"Rs" = .99,"Lf" = .15,"Rf" = .75)

  twoRate <- twoRateModel(par = par, schedule = df$rotation)
  lines(twoRate$slow,col='#ff8200ff')
  lines(twoRate$fast,col='#0087FF')
  lines(twoRate$total,col='#e51636ff')

dev.off()

}


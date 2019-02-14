plotRotationSchedule <- function(reversaltrials = 12, reversalmagnitude = -30) {
  
  alignedtrials <- 32
  rotatedtrials <- 100
  rotatedmagnitude <- 30
  errorclamptrials <- 20
  errorclampmagnitude <- 0
  
  
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
  
  lines(x,y,col='black')
}

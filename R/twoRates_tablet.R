#source("R/Figures_tablet.R")

library(optimx)

twoRateModel <- function(par, schedule) {
  
  # par[1] = Ls
  # par[2] = Rs
  # par[3] = Lf
  # par[4] = Rf
  

  slow <- c()
  fast <- c()
  total <- c()
  
  #par should be a vector with 4 named elements
  #schedule should be a vector of the rotation values, and the errorclamp trials should be NA
  
  E_t0 <- 0
  S_t0 <- 0
  F_t0 <- 0

  for (trial in c(1:length(schedule))) {
    
    # based on previous experience, what does the model do now?
    S_t1 <- (as.numeric(par["Rs"])*S_t0) - (as.numeric(par["Ls"])*E_t0)
    F_t1 <- (as.numeric(par["Rf"])*F_t0) - (as.numeric(par["Lf"])*E_t0)
    X_t1 <- S_t1 + F_t1
    
    # how wrong is that? what is the reach error the model makes?
   
    if (is.na(schedule[trial])) {
      E_t1 <- 0
    } else {
      E_t1 <- X_t1 + schedule[trial]
    }
    
    # update previous error, and previous output of fast and slow
    E_t0 <- E_t1 
    S_t0 <- S_t1
    F_t0 <- F_t1 
    
    # and store the slow, fast and total output somewhere?
    slow <- c(slow, S_t1)
    fast <- c(fast, F_t1)
    total <- c(total, X_t1)
  }
  
  modeloutput <- data.frame(slow, fast, total)
  
  return(modeloutput)

}


twoRateModelMSE <- function(par, schedule, reaches) {

  # par[1] = Ls
  # par[2] = Rs
  # par[3] = Lf
  # par[4] = Rf
  
  #print(par)
  
  # when the model makes no sense we return a very large error:
  max_error <- max(schedule[!is.na(schedule)]^2)
  
  # the fast process HAS to learn faster than the slow process:
  if (par["Lf"] <= par["Ls"]) {
    return(max_error)
  }
  
  # the slow process HAS to retain more than the fast process:
  if (par["Rs"] <= par["Rf"]) {
    return(max_error)
  }
  
  # all parameters have to be between 0 and 1:
  #if (any(0 > par | par > 1)) {
    #return(max_error)
  #}
  
  # see what the model does with these parameters and this schedule:
  model <- twoRateModel(par, schedule)
  # compare to the data:
  modelerrors <- model$total - reaches
  #print(modelerrors)
  # return the MSE:
  return(mean(modelerrors^2, na.rm=T))

}


twoRateModelFit <- function(schedule, reaches) {

  # par[1] = Ls
  # par[2] = Rs
  # par[3] = Lf
  # par[4] = Rf
  
  # somewhat sensible defaults?
  #par <- c("Ls" = .05,"Rs" = .99,"Lf" = .15,"Rf" = .75)

  # make search grid:
  npar <- 6
  searchgrid <- expand.grid('Ls'=seq((1/npar)/2, 1-((1/npar)/2), 1/npar),
                            'Lf'=seq((1/npar)/2, 1-((1/npar)/2), 1/npar),
                            'Rs'=seq((1/npar)/2, 1-((1/npar)/2), 1/npar),
                            'Rf'=seq((1/npar)/2, 1-((1/npar)/2), 1/npar)) 
  # evaluate starting positions:
  MSE <- apply(searchgrid,FUN=twoRateModelMSE, MARGIN=c(1), schedule, reaches)
  
  # run optimx on the best starting positions:
  allfits <- do.call("rbind",
                     apply( searchgrid[order(MSE)[1:10],],
                            MARGIN=c(1),
                            FUN=optimx,
                            fn=twoRateModelMSE,
                            method='L-BFGS-B',
                            lower = c(0,0,0,0), upper = c(1,1,1,1),
                            schedule=schedule,
                            reaches=reaches ) )
  # pick the best fit:
  win <- allfits[order(allfits$value)[1],]
  
  #output 'win' as named list
  winParameters <- win[1:4]
  
  return(winParameters)

}


#getting rebound:
  
getReboundsForANOVA <- function(group) {
  
  participant <- c()
  condition <- c()
  Rs <- c()
  Ls <- c()
  Rf <- c()
  Lf <- c()
  rebound <- c()

  for (perturbation in c('abrupt','gradual')) {
      print(perturbation)
      #open the file for these conditions:
      filename <- sprintf('data/processedData/%s_%s.csv',perturbation, group)
      print(filename)
      #print(getwd())
      df <- read.csv(filename, stringsAsFactors = FALSE)
      schedule <- df$rotation*-1
      participants <- names(df)[4:dim(df)[2]]
      
      # loop through participants:
      for (p.ID in participants) {
        print(p.ID)
        reaches <- unlist(df[p.ID])
        
        # fit the model for the participant
        par <- twoRateModelFit(schedule,reaches)

        
        thisrebound <- getRebound(reaches,schedule)
        participant <- c(participant, p.ID)
        condition <- c(condition, perturbation)
        Rs <- c(Rs, par$Rs)
        Ls <- c(Ls, par$Ls)
        Rf <- c(Rf, par$Rf)
        Lf <- c(Lf, par$Lf)
        rebound <- c(rebound, thisrebound)
      
      }
    }
  
  # print(participant)
  # print(condition)
  # print(Rs)
  # print(Ls)
  # print(Rf)
  # print(Lf)
  # print(rebound)

  df <- data.frame(participant, condition, Rs, Ls, Rf, Lf, rebound)
  write.csv(df, file = sprintf('data/processedData/rebound_%s.csv', group))
  
}


getRebound <- function(reaches,schedule) {
  
  
  #reversalstart <- 137
  #errorclampstart <- reversalstart + 12
  
  #rebound <- mean(model$total[errorclampstart:length(schedule)]) - min(model$total[reversalstart:length(schedule)])
  
  reversals <- reaches[137:148]
  reversals <- reversals[!is.na(reversals)]
  rebound <- mean(reaches[(length(schedule)-9):length(schedule)], na.rm=TRUE) - reversals[length(reversals)]
    
  return(rebound)
  
}

#ANOVA 

# library(ez)
# getANOVA <- function(group) {
#   
#   reboundData <- read.csv(file = sprintf ('data/%s/rebound.csv', group), stringsAsFactors = FALSE)
#   reboundData$participant <- as.factor(reboundData$participant)
#   reboundData$condition <- as.factor(reboundData$condition)
#   anova <- ezANOVA(data = reboundData, dv = rebound, wid = participant, between = c(condition), type=3)
#   print(anova)
#   
# }



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


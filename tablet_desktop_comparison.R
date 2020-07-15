#ANOVA

library(ez)
getANOVA <- function() {
  
  reboundData <- read.csv(file = sprintf ('data/Tablet_data/tablet_desktop_comparison.csv'), stringsAsFactors = FALSE)
  reboundData$participant <- as.factor(reboundData$participant)
  reboundData$setup <- as.factor(reboundData$setup)
  anova <- ezANOVA(data = reboundData, dv = rebound, wid = participant, between = c(setup), type=3)
  print(anova)
  
}


#polynomial logistic regression

pLogRegression <- function() {
  
  df <- read.csv('data/Tablet_data/tablet_desktop_comparison.csv', stringsAsFactors = F)
  
  df$setup <- as.factor(df$setup)
  
  print(summary(glm(formula = setup ~ Rs + Ls + Rf + Lf, family = binomial(link = "logit"), 
                    data = df)))
  
  
}
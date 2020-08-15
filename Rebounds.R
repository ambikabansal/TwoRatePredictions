library(ez)
getReboundANOVA <- function(group) {
  
  reboundData <- read.csv(file = sprintf ('data/processedData/%s_tworates_new.csv', group), stringsAsFactors = FALSE)
  reboundData$participant <- as.factor(reboundData$participant)
  reboundData$condition <- as.factor(reboundData$condition)
  anova <- ezANOVA(data = reboundData, dv = rebound, wid = participant, within = c(condition), type=3)
  print(anova)
  
}

library(ez)

setupReboundANOVA <- function() {
  
  df_tablet30 <- read.csv(sprintf('data/processedData/tablet30_tworates_new.csv', stringsAsFactors = FALSE))
  df_VR30 <- read.csv(sprintf('data/processedData/VR30_tworates_new.csv', stringsAsFactors = FALSE))
  
  df_tablet30$setup <- 'tablet30'
  df_VR30$setup <- 'VR'
  df <- rbind(df_tablet30, df_VR30)
  
  #return(df)
  
  df$participant <- as.factor(df$participant)
  df$setup <- as.factor(df$setup)
  df$condition <- as.factor(df$condition)
  
  print(ezANOVA(data=df, dv=rebound, wid=participant, within= condition, between = setup)  )
  
}

rotationReboundANOVA <- function() {
  
  df_tablet30 <- read.csv(sprintf('data/processedData/tablet30_tworates_new.csv', stringsAsFactors = FALSE))
  df_tablet60 <- read.csv(sprintf('data/processedData/tablet60_tworates_new.csv', stringsAsFactors = FALSE))

  df_tablet60$rawRebound <- df_tablet60$rebound*2
  df_tablet30$rawRebound <- df_tablet30$rebound
  
  df_tablet30$rotation <- 'tablet30'
  df_tablet60$rotation <- 'tablet60'
  
  df <- rbind(df_tablet30, df_tablet60)
  
  #return(df)
  
  df$participant <- as.factor(df$participant)
  df$rotation <- as.factor(df$rotation)
  df$condition <- as.factor(df$condition)
  
  print(ezANOVA(data=df, dv=rawRebound, wid=participant, within= condition, between = rotation)  )
  
}

normalized_rotationReboundANOVA <- function() {
  
  df_tablet30 <- read.csv(sprintf('data/processedData/tablet30_tworates_new.csv', stringsAsFactors = FALSE))
  df_tablet60 <- read.csv(sprintf('data/processedData/tablet60_tworates_new.csv', stringsAsFactors = FALSE))
  
  df_tablet30$rotation <- 'tablet30'
  df_tablet60$rotation <- 'tablet60'
  df <- rbind(df_tablet30, df_tablet60)
  
  #return(df)
  
  df$participant <- as.factor(df$participant)
  df$rotation <- as.factor(df$rotation)
  df$condition <- as.factor(df$condition)
  
  print(ezANOVA(data=df, dv=rebound, wid=participant, within= condition, between = rotation)  )
  
}


getReboundTtest <- function(group) {
  
  df <- read.csv(file = sprintf ('data/processedData/%s_tworates_new.csv', group), stringsAsFactors = FALSE)
  df$participant <- as.factor(df$participant)
  x <- df$rebound [which(df$condition=='abrupt')]
  y <- df$rebound [which(df$condition=='gradual')]

  ttest <- t.test(x,y, paired = TRUE)
  print(ttest)
  
}



abrupt_ReboundTtest <- function(group) {
  
  df <- read.csv(file = sprintf ('data/processedData/%s_tworates_new.csv', group), stringsAsFactors = FALSE)
  df$participant <- as.factor(df$participant)
  x <- df$rebound [which(df$condition=='abrupt')]

  ttest <- t.test(x, mu=0, alternative = "greater")
  print(ttest)
  
}

gradual_ReboundTtest <- function(group) {
  
  df <- read.csv(file = sprintf ('data/processedData/%s_tworates_new.csv', group), stringsAsFactors = FALSE)
  df$participant <- as.factor(df$participant)
  x <- df$rebound [which(df$condition=='gradual')]
  
  ttest <- t.test(x, mu=0, alternative = "greater")
  print(ttest)
  
}
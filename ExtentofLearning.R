library(ez)

getExtentofLearning <- function(group) {
  
  #create empty vectors OUTSIDE OF THE LOOPS
  participant <- c()
  block <- c()
  condition <-c()
  reachdeviation <- c()
  
    # loop through condition
  for (perturbation in c('abrupt','gradual')) {
    #open the file for these conditions:
    df <- read.csv(sprintf('data/processedData/%s_%s_new.csv', group, perturbation, stringsAsFactors = FALSE))
    #cat(sprintf(' %s\n', perturbation))

    participants <- names(df)[3:dim(df)[2]]
    
    #print(participants)
    blocktrials <- list('last training'=c(129,130,131,132), 'last reversal'=c(141:144))

    # loop through participants:
    for (p.ID in participants) {
      reachdevs <- df[,p.ID]
      #cat(sprintf('   %s %s\n', perturbation, p.ID))   
      
      # loop through blocks
      for (blockname in names(blocktrials)) {
        #cat(sprintf('      %s %s %s\n', perturbation, p.ID, blockname))   
          
      # get reach deviations
        meanreachdev <- mean(reachdevs[blocktrials[[blockname]]], na.rm=TRUE)

       # fill in vectors
         participant <- c(participant, p.ID)
         block <- c(block, blockname)
         condition <- c(condition, perturbation)
         reachdeviation <- c(reachdeviation, meanreachdev)
         
      }
    }
  }
  
  # create data frame
  df <- data.frame(participant, block, condition, reachdeviation)
  #print(df)
  return(df)
}


# ExtentOfLearningANOVA <- function(group) {
#   
#   df <- getExtentofLearning(group=group)
#   
#   df$participant <- as.factor(df$participant)
#   df$block <- as.factor(df$block)
#   df$condition <- as.factor(df$condition)
#   
#   print(ezANOVA(data=df, dv=reachdeviation, wid=participant, within= c(block, condition))  )
#   
# }


setupLastTrainingANOVA <- function() {
  
  df_tablet30 <- getExtentofLearning(group='tablet30')
  df_VR30 <- getExtentofLearning(group='VR30')
  
  
  df_tablet30$setup <- 'tablet'
  df_VR30$setup <- 'VR'
  df <- rbind(df_tablet30, df_VR30)

  df <- subset(df, df$block == 'last training')

   df$participant <- as.factor(df$participant)
   df$setup <- as.factor(df$setup)
   df$condition <- as.factor(df$condition)

  print(ezANOVA(data=df, dv=reachdeviation, wid=participant, within= condition, between = setup))

}

rotationLastTrainingANOVA <- function() {
  
  df_tablet30 <- getExtentofLearning(group='tablet30')
  df_tablet60 <- getExtentofLearning(group='tablet60')
  
  
  df_tablet30$rotation <- 'tablet30'
  df_tablet60$rotation <- 'tablet60'
  df <- rbind(df_tablet30, df_tablet60)
  df <- subset(df, df$block == 'last training')

  df$participant <- as.factor(df$participant)
  df$rotation <- as.factor(df$rotation)
  df$condition <- as.factor(df$condition)

  print(ezANOVA(data=df, dv=reachdeviation, wid=participant, within= condition, between = rotation))

}

setupLastReversalANOVA <- function() {
  
  df_tablet30 <- getExtentofLearning(group='tablet30')
  df_VR30 <- getExtentofLearning(group='VR30')
  
  
  df_tablet30$setup <- 'tablet'
  df_VR30$setup <- 'VR'
  df <- rbind(df_tablet30, df_VR30)
  
  df <- subset(df, df$block == 'last reversal')
  
  df$participant <- as.factor(df$participant)
  df$setup <- as.factor(df$setup)
  df$condition <- as.factor(df$condition)
  
  print(ezANOVA(data=df, dv=reachdeviation, wid=participant, within= condition, between = setup))
  
}

rotationLastReversalANOVA <- function() {
  
  df_tablet30 <- getExtentofLearning(group='tablet30')
  df_tablet60 <- getExtentofLearning(group='tablet60')
  
  
  df_tablet30$rotation <- 'tablet30'
  df_tablet60$rotation <- 'tablet60'
  df <- rbind(df_tablet30, df_tablet60)
  df <- subset(df, df$block == 'last reversal')
  
  df$participant <- as.factor(df$participant)
  df$rotation <- as.factor(df$rotation)
  df$condition <- as.factor(df$condition)
  
  print(ezANOVA(data=df, dv=reachdeviation, wid=participant, within= condition, between = rotation))
  
}
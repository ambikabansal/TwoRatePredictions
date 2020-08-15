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

    # participants <- sapply(names(df)[3:dim(df)[2]], function(name) as.numeric(substr(name, nchar(name)-1, nchar(name))))
    participants <- names(df)[3:dim(df)[2]]
    
    #print(participants)
    blocktrials <- list('last training'=c(129,130,131,132), 'last reversal'=c(141:144))

    # loop through participants:
    for (p.ID in participants) {
      #ppname <- sprintf('%s_p%02d', group, p.ID)
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
         
         # cat(sprintf('      %s %s %s: %0.2f\n', perturbation, p.ID,
         #             blockname, meanreachdev))
         # cat(sprintf('      (vector lengths: %d, %d, %d, %d)\n',
         #             length(participant), length(block), length(condition),
         #             length(reachdeviation)))

      }
    }
  }
  
  # create data frame
  df <- data.frame(participant, block, condition, reachdeviation)
  # print(df)
  return(df)
}


ExtentOfLearningANOVA <- function(group) {
  
  df <- getExtentofLearning(group=group)
  
  df$participant <- as.factor(df$participant)
  df$block <- as.factor(df$block)
  df$condition <- as.factor(df$condition)
  
  print(ezANOVA(data=df, dv=reachdeviation, wid=participant, within= c(block, condition))  )
  
}
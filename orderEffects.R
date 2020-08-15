library(ez)

orderData <- function(group) {
  
  df <- read.csv(sprintf('data/processedData/%s_abrupt_new.csv', group), stringsAsFactors = FALSE)
  
  demographics <- read.csv(sprintf('data/processedData/%s_demographics.csv', group), stringsAsFactors = FALSE)
  
  blocktrials <- list('first'=c(33,34,35,36), 'second'=c(37,38,39,40), 'last'=c(129,130,131,132))
  
  
  participant <- c()
  block <- c()
  order <-c()
  reachdeviation <- c()
  
  ppnos <- sapply(names(df)[3:dim(df)[2]], function(name) as.numeric(substr(name, nchar(name)-1, nchar(name))))
  
  for (ppno in ppnos) {
    
    ppname <- sprintf('%s_p%02d', group, ppno)
    
    reachdevs <- df[,ppname]
    
    pporder <- demographics[which(demographics$ID == ppno),'order']
    
    for (blockname in names(blocktrials)) {
      
      meanreachdev <- mean(reachdevs[blocktrials[[blockname]]], na.rm=TRUE)
      
      participant <- c(participant, ppno)
      block <- c(block, blockname)
      order <- c(order, pporder)
      reachdeviation <- c(reachdeviation, meanreachdev)
      
    }
    
    
  }
  
  df <- data.frame(participant, block, order, reachdeviation)
  
  return(df)
  
}


orderANOVA <- function(group) {
  
  df <- orderData(group=group)
  
  df$participant <- as.factor(df$participant)
  df$block <- as.factor(df$block)
  df$order <- as.factor(df$order)
  
  print(ezANOVA(data=df, dv=reachdeviation, wid=participant, within=block, between=order)  )
  
}
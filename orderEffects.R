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

      participant <- c(participant, ppname)
      block <- c(block, blockname)
      order <- c(order, pporder)
      reachdeviation <- c(reachdeviation, meanreachdev)
      
    }
    
    
  }
  
  df <- data.frame(participant, block, order, reachdeviation)
  return(df)
  
}


# orderANOVA <- function(group) {
#   
#   df <- orderData(group=group)
#   
#   df$participant <- as.factor(df$participant)
#   df$block <- as.factor(df$block)
#   df$order <- as.factor(df$order)
#   
#   print(ezANOVA(data=df, dv=reachdeviation, wid=participant, within=block, between=order)  )
#   
# }



setup_order_first_ANOVA <- function() {
  
  df_tablet30 <- orderData(group='tablet30')
  df_VR30 <- orderData(group='VR30')
  
  
  df_tablet30$setup <- 'tablet'
  df_VR30$setup <- 'VR'
  df <- rbind(df_tablet30, df_VR30)
  
  df <- subset(df, df$block == 'first')
  
  df$participant <- as.factor(df$participant)
  df$setup <- as.factor(df$setup)
  df$order <- as.factor(df$order)

  print(ezANOVA(data=df, dv=reachdeviation, wid=participant, between = c(setup, order)))

}

rotation_order_first_ANOVA <- function() {
  
  df_tablet30 <- orderData(group='tablet30')
  df_tablet60 <- orderData(group='tablet60')
  
  
  df_tablet30$rotation <- 'tablet30'
  df_tablet60$rotation <- 'tablet60'
  df <- rbind(df_tablet30, df_tablet60)
  df <- subset(df, df$block == 'first')
  
  df$participant <- as.factor(df$participant)
  df$rotation <- as.factor(df$rotation)
  df$order <- as.factor(df$order)
  
  print(ezANOVA(data=df, dv=reachdeviation, wid=participant, between = c(rotation, order)))
  
}


setup_order_second_ANOVA <- function() {
  
  df_tablet30 <- orderData(group='tablet30')
  df_VR30 <- orderData(group='VR30')
  
  
  df_tablet30$setup <- 'tablet'
  df_VR30$setup <- 'VR'
  df <- rbind(df_tablet30, df_VR30)
  
  df <- subset(df, df$block == 'second')
  
  df$participant <- as.factor(df$participant)
  df$setup <- as.factor(df$setup)
  df$order <- as.factor(df$order)
  
  print(ezANOVA(data=df, dv=reachdeviation, wid=participant, between = c(setup, order)))
  
}

rotation_order_second_ANOVA <- function() {
  
  df_tablet30 <- orderData(group='tablet30')
  df_tablet60 <- orderData(group='tablet60')
  
  
  df_tablet30$rotation <- 'tablet30'
  df_tablet60$rotation <- 'tablet60'
  df <- rbind(df_tablet30, df_tablet60)
  df <- subset(df, df$block == 'second')
  
  df$participant <- as.factor(df$participant)
  df$rotation <- as.factor(df$rotation)
  df$order <- as.factor(df$order)
  
  print(ezANOVA(data=df, dv=reachdeviation, wid=participant, between = c(rotation, order)))
  
}

setup_order_last_ANOVA <- function() {
  
  df_tablet30 <- orderData(group='tablet30')
  df_VR30 <- orderData(group='VR30')
  
  
  df_tablet30$setup <- 'tablet'
  df_VR30$setup <- 'VR'
  df <- rbind(df_tablet30, df_VR30)
  
  df <- subset(df, df$block == 'last')
  
  df$participant <- as.factor(df$participant)
  df$setup <- as.factor(df$setup)
  df$order <- as.factor(df$order)
  
  print(ezANOVA(data=df, dv=reachdeviation, wid=participant, between = c(setup, order)))
  
}

rotation_order_last_ANOVA <- function() {
  
  df_tablet30 <- orderData(group='tablet30')
  df_tablet60 <- orderData(group='tablet60')
  
  
  df_tablet30$rotation <- 'tablet30'
  df_tablet60$rotation <- 'tablet60'
  df <- rbind(df_tablet30, df_tablet60)
  df <- subset(df, df$block == 'last')
  
  df$participant <- as.factor(df$participant)
  df$rotation <- as.factor(df$rotation)
  df$order <- as.factor(df$order)
  
  print(ezANOVA(data=df, dv=reachdeviation, wid=participant, between = c(rotation, order)))
  
}
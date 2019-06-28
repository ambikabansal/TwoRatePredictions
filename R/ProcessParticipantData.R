#reads participant data into data frame 
#splits into part 1 and 2
#returns the two parts
#block 1 is familiarization and is ignored

getParticipantData <- function(participant = 1) {
  
  filename <- sprintf("data/Pilot/TwoRate_p%03d.csv", participant)
  #print(filename)
  df <- read.csv(filename)
  feedback <- c()
  part1 <- getTrialReachDeviations(df[df$blockno %in% c(2:5), ])
  part2 <- getTrialReachDeviations(df[df$blockno %in% c(6:9), ])
  part1$block <- part1$block - (min(part1$block) - 1)
  part2$block <- part2$block - (min(part2$block) - 1)
  
  return(list('first'=part1, 'second'=part2))
}


getAdjustedReachAngles <-function(df) {
  
  targetangles <- unique(df$targetangle)
  
  #df$reachdeviation <- df$reachdeviation - df$targetangle
  
  if (df$rotation [which(df$block ==2)[1]] > 0) {
    df$rotation <- df$rotation*-1
    df$reachdeviation <- df$reachdeviation*-1
  } 
  
  
  for(targetangle in targetangles) {
    
    # get the median reach deviation of the second half of the aligned part:
    # - trials in block 1
    # - trials with targetangle == targetangle
    # - trials with trial > XXX (the number of trials in the aligned phase)
    bias <- median(df$reachdeviation[which(df$trial > 16 & df$block == 1 & df$targetangle == targetangle)] , na.rm=T)
    
    idx <- which(df$targetangle == targetangle)
    df$reachdeviation [idx] <- df$reachdeviation [idx] - bias
    
   
  }
  
  return(df)
  
}



getTrialReachDeviations <-function(df) {
  
  
  blocknos <- unique(df$blockno)
  #print(blocknos)
  trial <- c()
  targetangle <- c()
  reachdeviation <- c()
  block <- c()
  rotation <- c()
  
  for (bno in blocknos) {
  #for (bno in c(blocknos[length(blocknos)])) {
    trialnos <- unique(df$trialno [df$blockno==bno])
    #print(c(bno,trialnos))
    for(trialnumber in trialnos) {
      
      #currenttrial <- subset(df,trialno == trialnumber)
      currenttrial <- df[which(df$trialno == trialnumber & df$blockno == bno),]
      trialreachangle <- getTrialReachAngleAt(trialdf = currenttrial,location = "per33.33333")
      
      reachdeviation <- c(reachdeviation,trialreachangle[1,1])
      #print(trialreachangle[1,1])
      targetangle <- c(targetangle,trialreachangle[1,2])
      trial <- c(trial, trialnumber)
      
      block <- c(block,bno)
      
      
      
      if (currenttrial$feedback[1] == 1) {
        rotation <- c(rotation,currenttrial$rotation[1])
      } else {
        rotation <- c(rotation,NaN)
      }

      
    }
    
    
    
    #print(c(bno,trial))
    #trialdf <- subset(df,df$blockno==bno & df$trialno==trial)
    
    #plotting      
    #plot(main=sprintf('trial: %d',trial),x=trialdf$Xcursor,y=trialdf$Ycursor,type='l',xlim=c(-500,500), ylim=c(0,500),asp=1)
    #lines(x=trialdf$Xmouse,y=trialdf$Ymouse,col='blue')
    #points(x=0,y=0, col='green')
    #points(x=trialdf$Xtarget,y=trialdf$Ytarget, col='yellow')
    #print(atan2(trialdf$Ymouse,trialdf$Xmouse))
  }
  
  angularreachdeviations <- data.frame(trial,targetangle,reachdeviation,block,rotation)
  
  #print(str(angularreachdeviations))
  
  return(angularreachdeviations)
  
}

rotateTrajectory <- function(X,Y,angle) {
  
  # create rotation matrix to rotate the X,Y coordinates
  th <- (angle/180) * pi
  R <- t(matrix(data=c(cos(th),sin(th),-sin(th),cos(th)),nrow=2,ncol=2))
  
  # put coordinates in a matrix as well
  coordinates <- matrix(data=c(X,Y),ncol=2)
  
  # rotate the coordinates
  Rcoordinates <- coordinates %*% R
  
  # return the rotated reach
  return(Rcoordinates)
  
}


getTrialReachAngleAt <- function(trialdf, location='per') {
  
  
  # location (string) determines where the angle of thereach is
  #determines, it is one of:
  # maxvel: maximum velocity (default)
  # endpoint: end of the reach
  # cmX: the last sample before this distance from home, where X is
  #replaced by a numeral
  # perX: % portion of target distance, where X is replaced by a numeral
  
  # return a matrix of two numbers:
  reachangle = matrix(data=NA,nrow=1,ncol=2)
  
  # if the trial was rejected, return empty matrix now
  #if (trialdf[1,'trialselected'] == 0) {
  
  #return(reachangle);
  
  #}
  
  # extract the relevant reach information
  #X <- trialdf[trialdf$sampleselected == 1,'Xmouse']
  #Y <- trialdf[trialdf$sampleselected == 1,'Ymouse']
  X <- trialdf[,'Xmouse']
  Y <- trialdf[,'Ymouse']
  #MV <- trialdf[trialdf$sampleselected == 1,'maxvelocity']
  angle <- trialdf[1,'target_angle']
  
  # print(X)
  
  # rotate the trajectory
  # (this avoids problems in the output of atan2 for large angles)
  trajectory <- rotateTrajectory(X,Y,-1*angle)
  X <- trajectory[,1]
  Y <- trajectory[,2]
  
  # now try find the specified location in this reach:
  # if we can't find it, we need to know
  invalidlocation <- TRUE
  
  # maximum velocity, should be in the data
  if (location == 'maxvel') {
    rown <- which(MV == 1)
    if (length(rown) > 1) {
      rown <- rown[1]
    }
    if (length(rown) == 0) {
      # no maximum velocity defined!
      return(reachangle)
    }
    invalidlocation <- FALSE
  }
  # end point, just the last point in the selected stretch of the reach
  if (location == 'endpoint') {
    rown <- length(X)
    invalidlocation <- FALSE
  }
  # cutoff in centimers, the last sample before this cutoff distance is
  #reached
  # this assumes that people don't go back, or that there is only one
  #movement from home to target
  if (substring(location,1,2) == 'cm') {
    distance <- as.numeric(substring(location, 3))
    
    # get the distance from home:
    dist <- sqrt(X^2 + Y^2)
    
    # if there are no selected samples below 3 cm: return NAs
    if (length(which(dist < distance)) == 0) {
      return(reachangle)
    }
    
    # find the last sample, where dist < 3
    rown <- max(which(dist < distance))
    invalidlocation <- FALSE
  }
  
  if (substring(location,1,3) == 'per') {
    percentage <- as.numeric(substring(location, 4))
    Xtarget <- trialdf[1,'Xtarget']
    Ytarget <- trialdf[1,'Ytarget']
    #print(sqrt(Xtarget^2 + Ytarget^2))
    targetdist <- sqrt(Xtarget^2 + Ytarget^2)*(percentage/100)
    #print((percentage/100))
    #print(targetdist)
    
    # get the distance from home:
    dist <- sqrt(X^2 + Y^2)
    
    # if there are no selected samples below the cutoff value: return NAs
    if (length(which(dist < targetdist)) == 0) {
      return(reachangle)
    }
    
    # find the last sample, where dist below cutoff value
    rown <- max(which(dist < targetdist))
    invalidlocation <- FALSE
    #print(rown)
    #print(length(dist))
  }
  
  # if we don't have a valid location, we can't calculate an angle to
  #return
  if (invalidlocation) {
    return(reachangle)
  }
  
  # calculate the angle at that point for the rotated trajectory
  # this is the angular deviation we are looking for
  angulardeviation <- (atan2(Y[rown],X[rown]) / pi) * 180
  
  # put the result in the little matrix:
  reachangle[1,1] <- angulardeviation
  reachangle[1,2] <- angle
  
  return(reachangle)
  
}


getReachDeviations <- function(participants = c(1:35)) {
  
  # this function is going to save 1 data frame per condition
  
  allData <- list()
  
  for(participant in participants) {
    print(participant)
    participantdata <- getParticipantData(participant = participant) 
    for (part in names(participantdata)) {
      data <- participantdata [[part]]
      data <- getAdjustedReachAngles(data)
      duration <- max (data$trial [which(data$block == 3)])
      magnitude <- abs (data$rotation [which(data$block == 3)[1]])
      #print(duration)
      #print(magnitude)
      
      conditionname <- sprintf("%dtrial-%ddeg",duration, magnitude)
      
      if (conditionname %in% names(allData)) {
        
        df <- allData [[conditionname]]
        df[sprintf("p%03d", participant)] <- data$reachdeviation
        allData [[conditionname]] <- df

      } else {
        
        # create data frame with: block / trial / rotation and reach deviation with participant ID
        df <- subset(data,select = c("block","trial","rotation"))
        df[sprintf("p%03d", participant)] <- data$reachdeviation
        
        allData [[conditionname]] <- df
      }

    }
  }
  
  #save data
  
  for (conditionname in names(allData)) {
    
    df <- allData [[conditionname]]
    
    # outlier removal?
    
    write.csv(df, file = sprintf ('data/Pilot/%s.csv',conditionname), quote = FALSE, row.names = FALSE)
    
  }
  
}


#loading data from all 4 conditions 

loadAllData <- function(){
  
  allData <- list()
  
  for (duration in c(4,12)) {
    for (magnitude in c(0,30)){
      
      conditionname <- sprintf("%dtrial-%ddeg",duration, magnitude)
      
      allData [[conditionname]] <- read.csv(file = sprintf ('data/Pilot/%s.csv', conditionname), stringsAsFactors = FALSE)
    }
  }

  return(allData) 
}



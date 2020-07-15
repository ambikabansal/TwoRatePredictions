getTabletData <- function(participant = 1) {
  
  filename <- sprintf("data/VR_data/VR_selectableData/%02d.csv", participant)
  df <- read.csv(filename)

  #adding feedback coloumn
  #empty vector to hold trial type
  feedback <- c()

  #looping through row numbers to get trial type for each sample
  for (rownum in c(1:nrow(df))) {

    if (df$trial_type[rownum] == 1) {
      feedback <- c(feedback, 1)
    } else {
      feedback <- c(feedback, 2)
    }

  }

   df <- cbind(df, feedback)
  
  filename <- sprintf("data/VR_data/VR_rawdata/%02d/S001/trial_results.csv", participant)
  trialdf <- read.csv(filename)

  for (blockno in unique(df$task_num)) {

    trialoffset <- min(df$trial_num[which(df$task_num == blockno)]) - 1

    for (trialno in unique(df$trial_num[which(df$task_num == blockno)])) {

      targetangle <- 180 - trialdf$target_angle[which(trialdf$block_num == blockno & trialdf$trial_num == trialno)]
      #print(targetangle)
      df$target_angle[which(df$task_num == blockno & df$trial_num == trialno)] <- targetangle

    }

  }


  filename <- sprintf("data/VR_data/VR_selectabledata/%02d_raw.csv", participant)
  write.csv(df, filename, quote = F, row.names = F)

  #shorter way of coding above
  # df$mariusfeedback <- 1
  # df$mariusfeedback[which(df$trial_type == 'errorclamp')] <- 2
  #cat('all task nums in input data frame:\n')
  #print(unique(df$task_num))
  part1 <- getTrialReachDeviations(df[df$task_num %in% c(1:4), ])
  part2 <- getTrialReachDeviations(df[df$task_num %in% c(5:8), ])
  part1$block <- part1$block - (min(part1$block) - 1)
  part2$block <- part2$block - (min(part2$block) - 1)
  #cat('unique part 1 blocks:\n')
  #print(unique(part1$block))
  #cat('unique part 2 blocks:\n')
  #print(unique(part2$block))
  return(list('first'=part1, 'second'=part2))
}


getAdjustedReachAngles <-function(df) {
  
  targetangles <- unique(df$targetangle)
    #print(names(df))
    #print(unique(df$block))
    #print(which(df$block ==3))
    #print(df$rotation [which(df$block ==3)][1] > 0)
    if (df$rotation [which(df$block ==3)][1] > 0) {
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
  
  blocknos <- unique(df$task_num)
  trial <- c()
  targetangle <- c()
  reachdeviation <- c()
  block <- c()
  rotation <- c()
  
  for (bno in blocknos) {
    trialnos <- unique(df$trial_num [df$task_num==bno])
    for(trialnumber in trialnos) {
      
      currenttrial <- df[which(df$trial_num == trialnumber & df$task_num == bno),]
      trialreachangle <- getTrialReachAngleAt(trialdf = currenttrial)

      reachdeviation <- c(reachdeviation,trialreachangle[1,1])
      targetangle <- c(targetangle,trialreachangle[1,2])
      trial <- c(trial, trialnumber)
      
      block <- c(block,bno)
      

      if (currenttrial$feedback[1] == 1) {
        rotation <- c(rotation,currenttrial$rotation[1])
      } else {
        rotation <- c(rotation,NaN)
      }
      
      
    }
 
  }
  
  #print(length(trial))
  #print(length(targetangle))
  #print(length(reachdeviation))
  #print(length(block))
  #print(length(rotation))
  
  angularreachdeviations <- data.frame(trial,targetangle,reachdeviation,block,rotation)

  
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


getTrialReachAngleAt <- function(trialdf, location='per66.66') {
  
  #***DON'T CHANGE THIS FUNCTION****
  
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
  X <- trialdf[,'mousex_px']
  Y <- trialdf[,'mousey_px']
  # HERE
  #MV <- trialdf[trialdf$selected == 1,'maxV']
  #MV <- trialdf$maxV # there can be only one?
  #print(MV)
  #print(trialdf$selected[1])
  
  angle <- trialdf[1,'target_angle']
  #yy <- trialdf[1,'targety_px']-trialdf[1,'homey_px']
  #xx <- trialdf[1,'targetx_px']-trialdf[1,'homex_px']
  #angle <- (atan2(yy, xx) / pi) * 180
  #print(c(xx, yy))
  print(angle)
  reachangle[1,2] <- angle
  
  # if (trialdf$selected[1] != 1) {
    #cat('not selected\n')
     #return(reachangle)
  #}
  
  #print((atan2(trialdf[1,'targety_px'],trialdf[1,'targetx_px']) / pi) * 180)
  
  # print(X)
  homeX <- trialdf[1,'homex_px']
  homeY <- trialdf[1,'homey_px']
  
  # rotate the trajectory
  # (this avoids problems in the output of atan2 for large angles)
  trajectory <- rotateTrajectory(X-homeX,Y-homeY,-1*angle)
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
    Xtarget <- trialdf[1,'targetx_px']
    Ytarget <- trialdf[1,'targety_px']
    print(sqrt(Xtarget^2 + Ytarget^2))
    targetdist <- sqrt(Xtarget^2 + Ytarget^2)*(percentage/100)
    print((percentage/100))
    print(targetdist)
    
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
  #print(rown)
  # calculate the angle at that point for the rotated trajectory
  # this is the angular deviation we are looking for
  angulardeviation <- (atan2(Y[rown],X[rown]) / pi) * 180
  
  print(angulardeviation)
  # put the result in the little matrix:

    reachangle[1,1] <- angulardeviation

   return(reachangle)
  
}


getReachDeviations <- function(participants = c(1:32)) {
  
  # this function is going to save 1 data frame per condition
  
  allData <- list()

    for(participant in participants) {
    cat(sprintf('working on participant: %d\n',participant))
    participantdata <- getTabletData(participant = participant) 
    for (part in names(participantdata)) {
      data <- participantdata [[part]]
      data <- getAdjustedReachAngles(data)
      
      #taskname <- any('A' %in% df$task_name[1])
      conditionname <- ifelse(data$rotation[which(data$block ==2)[1]] == 0, 'gradual', 'abrupt')
      #print(conditionname)
      
      if (conditionname %in% names(allData)) {
        cat('appending data to existing data frame\n')
        df <- allData [[conditionname]]
        df[sprintf("p%03d", participant)] <- data$reachdeviation
        allData [[conditionname]] <- df
        
      } else {
        #cat('create new data frame for data\n')
        # create data frame with: block / trial / rotation and reach deviation with participant ID
        #print(names(df))
        #str(data)
        df <- subset(data,select = c("block","trial","rotation"))
        df[sprintf("p%03d", participant)] <- data$reachdeviation
        
        allData [[conditionname]] <- df
      }
      
    }
  }
  
  #save data
  #print(names(allData))
  for (conditionname in names(allData)) {
    
    df <- allData [[conditionname]]
    
    # outlier removal?
    
    write.csv(df, file = sprintf ('data/VR_data/VR_selectableData/%s.csv',conditionname), quote = FALSE, row.names = FALSE)
    
  }
  
}


#loading data from both abrupt and gradual conditions 

loadAllData <- function(){
  
  allData <- list()
  
  for (perturbation in c('abrupt','gradual')) {
      
      conditionname <- sprintf("%s",perturbation)
      
      allData [[conditionname]] <- read.csv(file = sprintf ('data/processedData/%s/_30_tablet.csv', conditionname), stringsAsFactors = FALSE)
    }
  
  return(allData) 
}



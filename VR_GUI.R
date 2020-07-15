library(data.table)

getFile <- function(participant) {
  
  trial_results_df <- read.csv (file = sprintf ("data/VR_data/VR_rawdata/%02d/S001/trial_results.csv", participant), stringsAsFactors = FALSE)
  
  counter <- 1
  df_list <- list()
  
  for (rownum in c(1:nrow(trial_results_df))) {
    
    filepath <- sprintf('data/VR_data/VR_rawdata/%02d/S001/%s',participant, trial_results_df$example_object_movement_filename [rownum])
    
    trial <- read.csv(filepath)
    
    trial$time_s <- trial$time
    trial$mousex_px <- trial$pos_x
    trial$mousey_px <- trial$pos_z
    trial$task_num <- trial_results_df$block_num [rownum]
    trial$trial_num <- trial_results_df$trial_num [rownum]
    trial$trial_type <- trial_results_df$cursor_visibility [rownum]
    trial$rotation <- trial_results_df$rotation [rownum]
    trial$target_angle <- trial_results_df$target_angle [rownum]
    trial$homex_px <- trial_results_df$home_x [rownum]
    trial$homey_px <- trial_results_df$home_z [rownum]
    
    df_list[[counter]] <- trial
    
    counter <- counter + 1
  }
  
  df_total <- do.call(rbind, df_list)
  
  df_total$trial_type <- ifelse (isTRUE(df_total$trial_type), 0, 1)
  #write.csv(df_total, file = sprintf ("data/VR_data/VR_selectableData/%02d.csv", participant), row.names=FALSE, quote=FALSE)
  
  return(df_total)
}


getTarget <- function() {
  
  x_40 <- cos((40/180)*pi)/10
  y_40 <- sin((40/180)*pi)/10
  
  x_50 <- cos((50/180)*pi)/10
  y_50 <- sin((50/180)*pi)/10
  
  forty <- c(x_40, y_40)
  fifty <- c(x_50, y_50)
  one_thirty <- c(x_50*-1, y_50)
  one_forty <- c(x_40*-1, y_40)
  
  targets <<- list("40" = forty, "50" = fifty, "130" = one_thirty, "140" = one_forty)
  
}



makeTargetX <- function(num) {
  
  if (num == 130) {
    return(targets$"130"[1])
  }
  
  else if (num == 140) {
    return(targets$"140"[1])
  }
    
  else if (num == 40) {
    return(targets$"40"[1])
  }
  
  else if (num == 50) {
    return(targets$"50"[1])
  }
  
}

makeTargetY <- function(num) {
  
  if (num == 130) {
    return(targets$"130"[2])
  }
  
  else if (num == 140) {
    return(targets$"140"[2])
  }
  
  else if (num == 40) {
    return(targets$"40"[2])
  }
  
  else if (num == 50) {
    return(targets$"50"[2])
  }
  
}


test1DF <- function(){
  
df_total <- getFile(1)
getTarget()

df_total$target_x <- lapply(df_total$target_angle, makeTargetX)
df_total$target_y <- lapply(df_total$target_angle, makeTargetY)

return (df_total)
}


getAllData <- function(participants=c(1:32)) {
  
  for (participant in participants) {
    
    df_total <- getFile(participant)
    getTarget()
    
    df_total$targetx_px <- unlist(lapply(df_total$target_angle, makeTargetX))
    df_total$targety_px <- unlist(lapply(df_total$target_angle, makeTargetY))
    
    write.csv(df_total, file = sprintf ("data/VR_data/VR_selectableData/%02d.csv", participant), sep = "\t", row.names=FALSE, quote=FALSE)
  }

}



library(dplyr)

getData <- function(participant) {
  
df <- read.csv(file = sprintf ("data/Tablet_data/60-deg_rawdata/%02d_COMPLETE.csv", participant), stringsAsFactors = FALSE)

new_condition <- c()
for (el in df$task_name) {new_condition <- c(new_condition,as.numeric(list('A'=0, 'G'=1)[[substr(el,1,1)]]))}
df$task_name <- new_condition

df$trial_type <- ifelse (df$trial_type == "cursor", 0, ifelse  (df$trial_type == "error_clamp", 1, ""))
df$terminalfeedback_bool <- ifelse (isTRUE(df$terminalfeedback_bool), 0, 1)


df$trial_type <- as.numeric(df$trial_type)

abrupt <- df %>% filter(df$task_name == 0)
gradual <- df %>% filter(df$task_name == 1)

abrupt <- fixTrialsNtime(abrupt)
gradual <- fixTrialsNtime(gradual)

write.table(abrupt, file = sprintf ('data/Tablet_Data/60-deg_selection/p%02d_abrupt.txt', participant), sep = "\t", row.names=FALSE, col.names = FALSE, quote=FALSE)
write.table(gradual, file = sprintf ('data/Tablet_Data/60-deg_selection/p%02d_gradual.txt', participant), sep = "\t", row.names=FALSE, col.names = FALSE, quote=FALSE)

}


fixTrialsNtime <- function(df) {
  
  # create column for cumulative trial numbers (initialize to 0)
  df$cumulative_trial_no <- 0
  
  # start trial counter to have valid values for cumuative trial number
  trial_count <- 0
  
  # get list if unique tasks:
  task_nums <- unique(df$task_num)
  
  # loop through tasks 
  for (task_num in task_nums) {
    
    # get list of trials within the current task
    trial_nums <- unique(df$trial_num[which(df$task_num == task_num)] ) 
    
    # loop through the trials:
    for (trial_num in trial_nums) {
      
      # get row indices for the current task & trial
      trial_rows <- which(df$task_num == task_num & df$trial_num == trial_num)
      
      # get the start time of the current task & trial:
      trial_start_time <- df$time_s[trial_rows[1]]
      
      # subtract the start time of the current task and trial fro all time samples:
      df$time_s[trial_rows] <- df$time_s[trial_rows] - trial_start_time
      
      trial_count = trial_count + 1
      
      # set the cumulative trial number for the current trial in the current task
      # to be equal to the current cumulative trial number:
      df$cumulative_trial_no[trial_rows] <- trial_count
      
      }

  }
  
  return(df)
  
}


getAllData <- function(participants=c(15:32)) {
  
  for (participant in participants) {
    getData(participant)
  }
  
}

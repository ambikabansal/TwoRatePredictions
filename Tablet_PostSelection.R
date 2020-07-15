
getTable <- function(participant) {
  
 #df <- data.frame(matrix(ncol = 23))
  
  abrupt <- read.table(file = sprintf('data/Tablet_Data/tablet_selected/%01d/p%02d_abrupt_selected.txt', participant, participant))
  gradual <- read.table(file = sprintf('data/Tablet_Data/tablet_selected/%01d/p%02d_gradual_selected.txt', participant, participant))
  
    df <- rbind(abrupt, gradual)
    colnames(df) <- c("task_num", "task_name", "trial_type", "trial_num", "terminalfeedback", "rotation_angle", "targetangle_deg", "targetdistance_percmax", "homex_px", "homey_px", "targetx_px", "targety_px", "time_s", "mousex_px", "mousey_px", "cursorx_px", "cursory_px", "trial_no", "selected", "P1_P2", "NA1", "max_velocity", "NA2")
    
    write.csv(df, file = sprintf('data/Tablet_Data/tablet_postSelection/p%02d.csv', participant))
}


getAllData <- function(participants=c(1:26, 28:31)) {
  
  for (participant in participants) {
    getTable(participant)
  }
  
}
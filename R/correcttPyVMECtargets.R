library("rjson")


getExperimentJSON <- function(experiment) {
  
  # this function reads in a json file for an experiment and makes it useable for R
  
  # construct filename for experiment json:
  json_file <- sprintf('experiments/%s.json',experiment)
  
  # read json, and convert to R format (list):
  json <- fromJSON(paste(readLines(json_file), collapse=""))
  
  return(json)
  
}

createTaskTrialDF <- function(json) {
  
  # this function takes a json-based experiment configuration,
  # and makes a data frame describing: task name, trial type and number of trials for each task
  
  ntasks <- length(json$experiment)
 
  taskname <- c()
  ntrials <- c()
  type <- c()
  
  for (taskno in c(1:ntasks)) {
    
    
    taskcfg <- json$experiment[[taskno]]
    
    if (taskcfg$trial_type != "pause") {
      taskname <- c(taskname, taskcfg$task_name)
      ntrials <- c(ntrials, taskcfg$num_trials)
      type <- c(type, taskcfg$trial_type)
    }
    
  }
  
  return(data.frame(taskname,ntrials,type))
  
}

fixExperiment <- function(experiment) {
  
  # this function fixes the target positions in a whole experiment
  # run it for each of your experiments
  
  # first we get a list of participants in the experiment,
  # this is simpley list of folders in the experiment's data folder
  participants <- list.dirs(path = sprintf('data/%s/',experiment), full.names = TRUE, recursive = FALSE)
  
  # now we get the task data frame from the earlier functions:
  taskDF <- createTaskTrialDF(getExperimentJSON(experiment))
  
  # keep only the tasks where the target position was stored incorrectly:
  taskDF <- taskDF[which(taskDF$type %in% c('no_cursor', 'error_clamp')),]
  
  # loop through participants (= folders)
  for (participant in participants) {
    
    print(participant)
    
    # loop through tasks that need fixing:
    for (taskno in c(1:nrow(taskDF))) {
      
      taskname <- taskDF$taskname[taskno]
      
      # loop through the trials in the task:
      for (trialno in c(0:(taskDF$ntrial[taskno]-1))) {
        
        # name of file for the trial:
        filename <- sprintf('%s/%s_%d.csv',participant,taskname,trialno)
        
        # read trial data:
        trialdf <- read.csv(filename, stringsAsFactors = FALSE)
        
        # fix target y coordinates:
        trialdf$targety_px <- trialdf$targety_px - trialdf$homey_px
        trialdf$homey_px <- 0
        
        # write data back to disk:
        write.csv(trialdf, filename, quote=FALSE, row.names=FALSE)
        
      }
      
    }
    
  }
  
}
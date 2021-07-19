#Header Description

#Libraries Go Here
library(dplyr)
library(VideoTrackingUtilities)
library(ggplot2)
library(purrr)

#Section where I define parameters manually for testing, delete upon completion
pathExample1 <- "examples1.out"
paramExample1 <- 1

#Master Function for File With Parameter Names in Definition
runExampleFunction <- function(pathInputFolder, 
                              pathFolderToMake = "outputtemp", 
                              param1 = 1,
                              param2 = 2){
  #Basic Documentation and Parameter Description
  
  #Initializing Paths and Making Directories
  #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
  start<- Sys.time()
  print("Initializing Paths and Directories...")
  
  #Paths Strings To Create:
  # pathInputFolder                 Relevant Folder Names Defined In Args and Commented Out
  dir <- pathFolderToMake
  pathStatsFolder <- "stats"
  pathParameterRecord <- "parameters"

  
  #Making Output Folder and Files
  if(file.exists(dir)){
    unlink(dir, recursive = TRUE)
  }
  dir.create(dir)
  pathStats <- paste0(dir,"/", pathStatsFolder)
  dir.create(pathStats)
  pathSysTimes <- paste0(dir, "/", pathStatsFolder, "/systemtimes.out")
  pathParams <- paste0(dir, "/", pathStatsFolder, "/", pathParameterRecord, ".txt")
  #Others Go Here
  
  #Recording Parameters
  file.create(pathParams, showWarnings = FALSE)
  conParams <- file(pathParams)
  #Parameters Go Here
  record1 <- paste0("Scale Parameter:  ", paramScale)
  record2 <- paste0("Alpha Parameter:  ", paramAlpha)
  writeLines(c(record1, record2), conParams)
  close(conParams)
  
  #}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
  
  
  
  #Code Block Name
  #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
  #Renaming Relevant Parameters: heads <- paramheadcount
  #-----------------------------------------------------------------------------------------------------------------------------
  #Profiling Output
  print(Sys.time() - start)
  print("Profiling some shit (ProfileName.prof)...")
  Rprof("ProfileName.prof")
  
  Rprof(NULL)
  #}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
  
}
  
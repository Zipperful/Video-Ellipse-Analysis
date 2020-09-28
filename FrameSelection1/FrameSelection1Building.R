#Functions for Frame Grabbing

#Libraries Go Here

library(imager)
library(imagerExtra)
library(tm)
# library(tibble)
library(tidytext)
library(dplyr)
library(tibble)


#' Gets Subtitles Inbetween 2 Times in a Given Subtool Object
#'
#' Explanation
#'
#' @param tbSrt
#' @param mtTimes N by 2 matrix of times with start and end times recorded in
#' @return A list of tbSrt's of strings of size N
#' @export

# timeGrabs <- function(tbSrt, mtTimes){
#   lsSubTibbles <- vector(mode = "list", length=nrow(mtTimes))
#   for(x in 1:nrow(mtTimes)){
#     lsSubTibbles[[x]] <- tbSrt[ tbSrt$Timecode_in >= mtTimes[x,1] & tbSrt$Timecode_out <= mtTimes[x,2], ]
#   }
#   return(lsSubTibbles)
# }
pathVideo = "./Test Video/[CBM]_Gurren_Lagann_-_s1_-_Sit_In_the_Hot_Tub_'Til_You're_Sick!_[720p]_[09E7D28A].mkv"
fileName= "GurrenTestVid.mkv"
fps1 = 1
fps2 = 10
width1 = width2 = 640
height1 = height2 = 360
subOutputName = paste0(fileName, ".srt")
frameOutput1Name = paste0(fileName, "-framegrab1-","%03d.jpeg")
frameOutput2Name = paste0(fileName, "-framegrab2-", "%03d.jpeg")
secondsStart=1000
secondsRun=10

ffSubgrab = paste0("ffmpeg -i ", fileName, " -map 0:s:0 ", subOutputName)
ffFrameGrab1 = paste0("ffmpeg -i ", fileName, " -r ", fps1, " -s ", width1, "x", height1, " -f image2 ", frameOutput1Name)
ffFrameGrab2 = paste0("ffmpeg -ss ", secondsStart, " -t ", secondsRun, " -i ", fileName, " -r ", fps2, " -s ", width2, "x", height2, " -f image2 ", frameOutput2Name)

system.time( system( ffFrameGrab2 ) )



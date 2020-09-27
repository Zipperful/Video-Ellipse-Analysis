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
vidTest <- load.image(pathVideo)
vidFrames <- frames(vidTest)
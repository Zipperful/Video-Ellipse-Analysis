#Functions for subtitle analysis

#Libraries Go Here

library(subtools)
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

timeSubGrabs <- function(tbSrt, mtTimes){
  lsSubTibbles <- vector(mode = "list", length=nrow(mtTimes))
  for(x in 1:nrow(mtTimes)){
    lsSubTibbles[[x]] <- tbSrt[ tbSrt$Timecode_in >= mtTimes[x,1] & tbSrt$Timecode_out <= mtTimes[x,2], ]
  }
  return(lsSubTibbles)
}


tokensFromTibbleSrts <- function(tbSrts, dfStopwords = stop_words){
  seasonTokens <- tbSrts %>% unnest_tokens(output=Tokens, input="Text_content") %>% anti_join(dfStopwords, by=c("Tokens" = "word"))
  return(seasonTokens)
}


#Dtm creation?
#Function to compare freq of excerpts to freq of whole
# To use bindtfidf we need to label each document independently to be examined, needs to be manual
#     To examine sexualization in an episode, we need the documents to be "in TDE timeframe in the episode" and "not"
#     To exampine across seasons, TDE in timeframe and not
#     To examine across shows, TDE in timeframe and not
#  So, generate a function that takes a folder of seasons, seperated on episodes per season folder, and creates column flagging In or Not
#  Then, compare TDE In to ep, season, 
#  Compare TDE In frome one ep, season, show to entire show collection for diff shows looking for similarities, 
#  Compare all TDE In to all TDE out

tidyTokens <- sTokens %>% anti_join(stop_words, by=c("Tokens"="word"))
tidyTop <- tidyTokens%>% count(Tokens, sort = TRUE) %>% top_n(10)

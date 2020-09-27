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

timeGrabs <- function(tbSrt, mtTimes){
  lsSubTibbles <- vector(mode = "list", length=nrow(mtTimes))
  for(x in 1:nrow(mtTimes)){
    lsSubTibbles[[x]] <- tbSrt[ tbSrt$Timecode_in >= mtTimes[x,1] & tbSrt$Timecode_out <= mtTimes[x,2], ]
  }
  return(lsSubTibbles)
}

srtsFromSeasonMKVFolder <- function(pathSeasonFolder){
  
}

srtsFromSeasonFolder <- function(pathSeasonFolder){
  dir <- dir(pathSeasonFolder)
  srts <- dir[ grep(".srt$", dir)]
  seasonSubs <- NULL
  if(length(srts) != 1){
    for(x in 1:length(srts)){
      episodeSubs <- read_subtitles(paste0("./", pathSeasonFolder, "/", srts[x]))
      episodeSubs <- episodeSubs %>% mutate(Episode = x, SrtFileName = srts[x])
      seasonSubs <- bind_rows(seasonSubs, episodeSubs)
    }
  } else{
    episodeSubs <- read_subtitles(paste0("./", pathSeasonFolder, "/", srts))
    episodeSubs <- episodeSubs %>% mutate(Episode = 1, SrtFileName = srts)
    seasonSubs <- bind_rows(seasonSubs, episodeSubs)
    
  }
  return(seasonSubs)
}

tokensFromTibbleSrts <- function(tbSrts, dfStopwords = stop_words){
  seasonTokens <- tbSrts %>% unnest_tokens(output=Tokens, input="Text_content") %>% anti_join(dfStopwords, by=c("Tokens" = "word"))
  return(seasonTokens)
}

srtsFromShowFolder <- function(pathShowFolder){
  dir <- dir(pathShowFolder)
  folders <- dir[ dir.exists(paste0("./", pathShowFolder, "/",dir))]
  showSubs <- NULL
  if(length(folders) !=1){
    for(x in 1:length(folders)){
      seasonSubs <- srtsFromSeasonFolder( paste0("./", pathShowFolder, "/", folders[x]) )
      seasonSubs <- seasonSubs %>% mutate(Season = x, SeasonFolderName = folders[x])
      showSubs <- bind_rows(seasonSubs, showSubs)
    }
  } else {
    seasonSubs <- srtsFromSeasonFolder( paste0("./", pathShowFolder, "/", folders) )
    seasonSubs <- seasonSubs %>% mutate(Season = 1, SeasonFolderName = folders)
    showSubs <- bind_rows(seasonSubs, showSubs)
  }
  return(showSubs)
}

srtsFromMultiShowFolder <- function(pathMultishowFolder){
  dir <- dir(paste0("./", pathMultishowFolder))
  folders <- dir[ dir.exists(paste0("./", pathMultishowFolder, dir))]
  multiShowSubs <- NULL
  if(length(folders) !=1){
    for(x in 1:length(folders)){
      showSubs <- srtsFromShowFolder( paste0("./", pathMultishowFolder, folders[x]) )
      showSubs <- showSubs %>% mutate(Show = x, ShowFolderName = folders[x])
      multiShowSubs <- bind_rows(multiShowSubs, showSubs)
    }
  } else {
    showSubs <- srtsFromShowFolder( paste0("./", pathMultishowFolder, folders) )
    showSubs <- showSubs %>% mutate(Show = 1, ShowFolderName = folders)
    multiShowSubs <- bind_rows(multiShowSubs, showSubs)
  }
  return(multiShowSubs)
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

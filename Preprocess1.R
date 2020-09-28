#Functions for subtitle analysis

fps1 = 1
width1 = 640
height1 = 320

processMultiShows <- function(pathMultiShowsFolder, fps=1, width=640, height=320){
  dir <- dir(pathMultiShowsFolder)
  foldersShows <- dir[ dir.exists(paste0("./", pathMultiShowsFolder, "/",dir))]
  pathShows <- paste0(pathMultiShowsFolder, "/", foldersShows)
  # print("MultiShows")
  # print(pathMultiShowsFolder)
  # print(pathShows)
  sapply(pathShows, processShows, fps=fps, width=width, height=height)
  return(NULL)
}

processShows <- function(pathShowFolder, fps=1, width=640, height=320){
  dir <- dir(pathShowFolder)
  foldersSeasons <- dir[ dir.exists(paste0("./", pathShowFolder, "/",dir))]
  pathSeasons <- paste0(pathShowFolder, "/", foldersSeasons)
  # print("Shows")
  # print(pathShowFolder)
  # print(pathSeasons)
  sapply(pathSeasons, processSeason, fps=fps, width=640, height=320)
  return(NULL)
}

processSeason <- function(pathSeasonFolder, fps=1, width=640, height=320){
  # print("Seasons")
  # print(pathSeasonFolder)
  dir <- dir(pathSeasonFolder)
  mkvs <- dir[ grep(".mkv$", dir)]
  roots <- substr(mkvs, 1, nchar(mkvs)-4)
  subOutputName = paste0(roots, ".srt")
  ffSubGrab = paste0("ffmpeg -i ", pathSeasonFolder, "/", mkvs, " -map 0:s:0 ", pathSeasonFolder, "/", subOutputName)
  frameOutput1Names <-  paste0(pathSeasonFolder, "/", mkvs, "-framegrab1-","%03d.jpeg")
  ffFrameGrabs1 = paste0("ffmpeg -i ", pathSeasonFolder, "/", mkvs, " -r ", fps, " -s ", width, "x", height, " -f image2 ", frameOutput1Names)
  
  sapply(paste0(pathSeasonFolder, "/", roots), FUN=dir.create)
  sapply(ffSubGrab, FUN=system)
  file.rename(from=paste0(pathSeasonFolder, "/", subOutputName), to=paste0(pathSeasonFolder, "/", roots,"/",subOutputName))
  sapply(ffFrameGrabs1, FUN=system)
  dir <- dir(pathSeasonFolder)
  
  if(length(roots) !=1){
    for(x in 1:length(roots)){
      jpegs<- dir[grep(paste0(roots[x], ".*\\.jpeg"), dir)]
      file.rename(from=paste0(pathSeasonFolder, "/", jpegs), to=paste0(pathSeasonFolder, "/", roots[x],"/",jpegs))
    }
  } else {
    jpegs <- dir[grep("\\.jpeg", dir)]
    file.rename(from=paste0(pathSeasonFolder, "/", jpegs), to=paste0(pathSeasonFolder, "/", roots, "/", jpegs))
  }
  return(NULL)
}

processMultiShows("./TestVideoExtraction")

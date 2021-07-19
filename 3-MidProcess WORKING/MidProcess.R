## Same idea as PreProcess1, check it for notes, add ffmpeg documentation 



timeImageProcess <- function(mtPathsTimes, fps = 10, width=720, height=405){
  filePath <- mtPathsTimes[,1]
  fileName <- folderPath <- filePath
  for(x in 1:length(filePath)){
    filePathSplit <- unlist(strsplit(filePath[x],"/"))
    fileName[x] <- filePathSplit[ length(filePathSplit) ]
    fileName[x] <- substr(fileName[x], 1, nchar(fileName[x])-4)
    folderPath[x] <- paste(filePathSplit[1:(length(filePathSplit)-1)], collapse="/")
  }
  print("filePath")
  print(filePath)
  print("fileName")
  print(fileName)
  print("folderPath")
  print(folderPath)
  # filePathSplit <- sapply(filePath, FUN = strsplit, "/")
  # print(filePathSplit)
  # fileName <- filePathSplit[ length(filePathSplit) ]
  # fileName <- substr(fileName, 1, nchar(fileName)-4)
  # print(fileName)
  # folderPath <- paste(filePathSplit[1:(length(filePathSplit)-1)], collapse="/")
  # print(folderPath)
  
  time1 <-as.numeric(mtPathsTimes[,2])
  time2 <- as.numeric(mtPathsTimes[,3])
  time3 <- time2-time1
  
  outputsPath <- paste0(folderPath, "/", fileName, "-MidProcess")
  outputsExist <- sapply(outputsPath, FUN = dir.exists)
  sapply(split(outputsPath, !outputsExist)$`TRUE`, dir.create)
  
  timesPath <- paste0(outputsPath, "/", "time1-", time1, "-time2-", time2, "-fps-", fps, "-width-", width, "-height-", height)
  timesExist <- sapply(timesPath, FUN = dir.exists)
  sapply(split(timesPath, timesExist)$`TRUE`, unlink, recursive=TRUE)
  sapply(timesPath, dir.create)
  
  frameOutput2Names <-  paste0(timesPath, "/framegrab2-","%03d.jpeg")
  ffFrameGrabs2 = paste0("ffmpeg -ss ", time1, " -t ", time3, " -i ", filePath, " -r ", fps, " -s ", width, "x", height, " -f image2 ", frameOutput2Names)
  sapply(ffFrameGrabs2, system)
}


testnames <- c("./TestVideoExtraction/GurrenLaggan/Season1/GurrenTestVid.mkv", "./TestVideoExtraction/GurrenLaggan/Season1/GurrenReductionTest.mkv")
testtime1 <- c(100, 100)
testtime2 <- c(106, 106)

testmttimes <- matrix(c(testnames, testtime1, testtime2), ncol=3)

timeProcess(testmttimes)
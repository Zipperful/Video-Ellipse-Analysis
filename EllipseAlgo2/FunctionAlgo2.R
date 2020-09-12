#Takes constants and an image and generates ellipse fragment images from it, returns speed and counts of arcs for each type

library(imager)
library(dplyr)
library(OpenImageR)
library(VideoTrackingUtilities)

# pathImageToLoad <- "./Test Pics/Danbooru8-6-20-1235pmEST/test13.jpg"
# pathFolderToMake = "SketchDanb2Output"
# paramScale=4
# paramAlpha=0.2
# paramSigma=2
# paramLengthCutoff=16
# paramLineArea=2
  
runJiaFanEllipses <- function(pathImageToLoad, 
                              pathFolderToMake = "outputtemp", 
                              paramScale=2, 
                              paramAlpha=0.2, 
                              paramSigma=1, 
                              paramLengthCutoff=16, 
                              paramLineArea=4,
                              paramCNC=0.2){
  #Documentation:
      #paramScale, paramAlpha, paramSigma, paramLengthCutoff, paramLineArea
  
  #Initializing Paths and Making Directories
  #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
  start<- Sys.time()
  print("Initializing Paths and Directories...")
  
  #Paths Automatically Created:
    # pathImageToLoad                   Initialized In Args
    pathImage <- pathImageToLoad
    # pathFolderToMake                  Initialized In Args
    dir <- pathFolderToMake
    pathStatsFolder <- "stats"        
    pathParameterRecord <- "parameters"
    pathResizeImage <- "resizedImage"
    pathOutlineImage <- "edges"
    pathHighlightImage <- "highlighted"
    pathContourImage <- "contours"
  
  #Making Output Folder and Files
  if(file.exists(dir)){
    unlink(dir, recursive = TRUE)
  }
  dir.create(dir)
  pathStats <- paste0(dir,"/", pathStatsFolder)
  dir.create(pathStats)
  pathSysTimes <- paste0(dir, "/", pathStatsFolder, "/systemtimes.out")
  pathParams <- paste0(dir, "/", pathStatsFolder, "/", pathParameterRecord, ".txt")
  
  #Recording Parameters
  file.create(pathParams, showWarnings = FALSE)
  conParams <- file(pathParams)
  recScale <- paste0("Scale Parameter:  ", paramScale)
  recAlpha <- paste0("Alpha Parameter:  ", paramAlpha)
  recSigma <- paste0("Sigma Parameter:  ", paramSigma)
  recLength <- paste0("Length Cutoff Parameter:  ", paramLengthCutoff)
  recArea <- paste0("Line Area Cutoff Parameter:  ", paramLineArea)
  writeLines(c(recScale, recAlpha, recSigma, recLength, recArea), conParams)
  close(conParams)
  
  #}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
  
  
  
  #Loading Image and Resizing with Scale Parameter
  #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
  scaleFactor <- paramScale
  #-----------------------------------------------------------------------------------------------------------------------------
  #Profiling Image Loading and Rescaling
  print(Sys.time() - start)
  print("Opening Image and Resizing (Initialization.prof)...")
  Rprof("Initialization.prof")
  
  #Loading image and rescaling by scale parameter, can plot here
  imgTest <- load.image(pathImage)
  imgTest <- imresize(imgTest, scale=1/scaleFactor)
  # plot(imgTest)
  
  Rprof(NULL)
  #}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
  
  
  
  #Finding Edges with CannyEdge with parameters alpha and sigma
  #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
  alp <- paramAlpha
  sig <- paramSigma
  
  #-----------------------------------------------------------------------------------------------------------------------------
  #Profiling Edge Detection
  print(Sys.time() - start)
  print("Performing Canny Edge Detection (EdgeDetection.prof)")
  Rprof("EdgeDetection.prof")
  
  #Greyscaling
  grayTest <- grayscale(imgTest, method = "XYZ")
  
  #Canny Edge Detection
  pxEdges <- makeCannyEdge(imgTest, alp=alp, sig=sig)
  
  Rprof(NULL)
  
  #-----------------------------------------------------------------------------------------------------------------------------
  #}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
  
  
  
  #Processing Image In To List of Contours Longer Than edgeLengthCutoff And Curving Threshold lineAreaCutoff
  #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
  threshEdge <- paramLengthCutoff
  threshLine <- paramLineArea
  
  #-----------------------------------------------------------------------------------------------------------------------------
  #Profiling First Contour Separation
  print(Sys.time() - start)
  print("Separating Contours into Two Groups Based on Slope (ContourSeparation1.prof)...")
  Rprof("ContourSeparation1.prof")
  
  #Finds slope of edges to break down in to two categories of contour
  dx <- imgradient(grayTest, "x")
  dy <- imgradient(grayTest, "y")
  tau <- (dy/dx)*pxEdges
  tau[ is.na(tau) ] <- 0
  tau1 <- tau
  tau2 <- tau
  tau1[ !(tau>0) ] <- 0
  tau2[ !(tau<0) ] <- 0
  tau2 <- abs(tau2)
  tau1rot <- rotMatrixCW(tau1[,,1,1])
  
  Rprof(NULL)
  
  #-----------------------------------------------------------------------------------------------------------------------------
  #Profiling Contour Connectivity
  print(Sys.time() - start)
  print("Finding First Contour Groups Connectivity (ContourConnectivity.prof)...")
  Rprof("ContourConnectivity.prof")
  
  #Finds out how contours are connected and stores each seperate contour in a list
  mtCont2 <- storeContours(spreadEdges(tau2[,,1,1]), threshEdge)
  mtCont1 <- storeContours(rotMatrixCCW(spreadEdges(tau1rot)), threshEdge)
  
  Rprof(NULL)
  
  #-----------------------------------------------------------------------------------------------------------------------------
  #Profiling Contour Extrema
  print(Sys.time() - start)
  print("Finding Extrema of First Group Contours (ContourExtrema.prof)...")
  Rprof("ContourExtrema.prof")
  
  #Finds extrema of contours for CNC calculation and threshholding
  mtExtremaPos <- posContPoints(mtCont1)
  mtExtremaNeg <- negContPoints(mtCont2)
  
  Rprof(NULL)
  
  #-----------------------------------------------------------------------------------------------------------------------------
  #Profiling Line Removal
  print(Sys.time() - start)
  print("Removing Straight Contours (ContourLineThresholding.prof)...")
  Rprof("ContourLineThresholding.prof")
  
  #Removing points that describe near-straight lines by finding near-0 areas of triangles between points
  mtExtremaNegAreas <- mtExtremaNeg
  mtExtremaPosAreas <- mtExtremaPos
  findTriangleArea <- function( mtPts ) {
    mtPts <- matrix(c(mtPts, c(1,1,1)), nrow=3)
    return(det(mtPts))
  }
  for(x in 1:length(mtCont2)){
    mtExtremaNegAreas[[x]] <- findTriangleArea(c(mtExtremaNeg[[x]])) / nrow(mtCont2[[x]])
  }
  for(x in 1:length(mtCont1)){
    mtExtremaPosAreas[[x]] <- findTriangleArea(c(mtExtremaPos[[x]])) / nrow(mtCont1[[x]])
  }
  posThreshLines <-abs(unlist(mtExtremaPosAreas))>threshLine
  negThreshLines <-abs(unlist(mtExtremaNegAreas))>threshLine
  mtCont1 <- mtCont1[ posThreshLines]
  mtCont2 <- mtCont2[ negThreshLines]
  mtExtremaPos <- mtExtremaPos[ posThreshLines ]
  mtExtremaNeg <- mtExtremaNeg[ negThreshLines ]
  
  Rprof(NULL)
  
  #-----------------------------------------------------------------------------------------------------------------------------
  #Garbage Collecting
  # rm(grayTest, dx,dy,tau, tau1, tau1rot, tau2, mtExtremaNegAreas, mtExtremaPosAreas)
  #}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
  
  
  
  #Splitting Curves into Two More Sets Based on Concavity Obtained by Summing Y Values in Contours and Plots
  #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
  #-----------------------------------------------------------------------------------------------------------------------------
  #Profiling Second Contour Separation
  print(Sys.time() - start)
  print("Separating Contours and Extrema Down into Two More Groups Based on Concavity (ContourSeparation2.prof)...")
  Rprof("ContourSeparation2.prof")
  
  concavities24 <- concavitySplitter(mtCont1, mtExtremaPos)
  conts2 <- concavities24[[2]]
  conts4 <- concavities24[[1]]
  pts2 <- concavities24[[4]]
  pts4 <- concavities24[[3]]
  
  concavities13 <- concavitySplitter(mtCont2, mtExtremaNeg)
  conts1 <- concavities13[[2]]
  conts3 <- concavities13[[1]]
  pts1 <- concavities13[[4]]
  pts3 <- concavities13[[3]]
  
  
  Rprof(NULL)
  #Garbage Collecting
  # rm(concavities13, concavities24, mtCont1, mtCont2, mtExtremaNeg, mtExtremaPos)
  #-----------------------------------------------------------------------------------------------------------------------------
  #}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
  
  
  
  #Finding Viable Pairs of Contours To Compare
  #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
  #-----------------------------------------------------------------------------------------------------------------------------
  #Profiling Matching Contours to Each Other for Possible Ellipse Detection
  print(Sys.time() - start)
  print("Matching Those Contour Pairs That Could Describe an Ellipse... (ContourMatching.prof)...")
  Rprof("ContourMatching.prof")
  
  #Ordering Points: pts1 CCW: et em es, pts2 CCW: et em es, pts3 CCW: es em et, pts4 CCW: es em et
  #Order Pts1 by decreasing x,y
  for(x in 1:length(pts1)){
    pts1[[x]] <- pts1[[x]][order(pts1[[x]][,1], decreasing=TRUE),]
  }
  #Order Pts2 by decreasing x, increasing y
  for(x in 1:length(pts2)){
    pts2[[x]] <- pts2[[x]][order(pts2[[x]][,2]),]
  }
  #Order Pts3 by increasing x,y
  for(x in 1:length(pts3)){
    pts3[[x]] <- pts3[[x]][order(pts3[[x]][,1]),]
  }
  #Order Pts4 by increasing x, decreasing y
  for(x in 1:length(pts4)){
    pts4[[x]] <- pts4[[x]][order(pts4[[x]][,1]),]
  }
  
  #Matching Pts1 to Pts2- pts1esx > pts2etx
  pairs12 <- vector(mode = "list", length=length(pts1)*length(pts2))
  for(x in 1:length(pts1)){
    for(y in 1:length(pts2)){
      if(pts1[[x]][3,1] >= pts2[[y]][1,1]){
        pairs12[[(x-1)*length(pts2)+y]] <- c(x,y)
      }
    }
  }
  pairs12 <- pairs12[!sapply(pairs12, is.null)]
  
  #Matching Pts1 to Pts4- pts1ety < pts4ety
  pairs14 <- vector(mode = "list", length=length(pts1)*length(pts4))
  for(x in 1:length(pts1)){
    for(y in 1:length(pts4)){
      if(pts1[[x]][1,2] <= pts4[[y]][3,2]){
        pairs14[[(x-1)*length(pts4)+y]] <- c(x,y)
      }
    }
  }
  pairs14 <- pairs14[!sapply(pairs14, is.null)]
  
  #Matching Pts3 to Pts2- pts3esy > pts2esy
  pairs32 <- vector(mode = "list", length=length(pts3)*length(pts2))
  for(x in 1:length(pts3)){
    for(y in 1:length(pts2)){
      if(pts3[[x]][1,2] >= pts2[[y]][3,2]){
        pairs32[[(x-1)*length(pts2)+y]] <- c(x,y)
      }
    }
  }
  pairs32 <- pairs32[!sapply(pairs32, is.null)]
  
  #Matching Pts3 to Pts4- pts3etx < pts4esx
  pairs34 <- vector(mode = "list", length=length(pts3)*length(pts4))
  for(x in 1:length(pts3)){
    for(y in 1:length(pts4)){
      if(pts3[[x]][3,1] <= pts4[[y]][1,1]){
        pairs34[[(x-1)*length(pts4)+y]] <- c(x,y)
      }
    }
  }
  pairs34 <- pairs34[!sapply(pairs34, is.null)]
  
  Rprof(NULL)
  #Garbage Collecting
  #-----------------------------------------------------------------------------------------------------------------------------
  #}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}  
  
  
  
  #Calculating CNC for Each Pair
  #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
  #-----------------------------------------------------------------------------------------------------------------------------
  threshCNC <- paramCNC
  #Calculating Characteristic Number Cutoff (CNC) for Every Possible Pair of Ellipses and Thresholding
  print(Sys.time() - start)
  print("Calculating and Thresholding Possilbe Ellipses Based on CNC ... (CNCThresholding.prof)...")
  Rprof("CNCThresholding.prof")
  
  #Finding CNC Values for each pair in quadrants 1 and 2, and thresholding with paramCNC
  #Internal notes follow:
  #Ordering Points: pts1 CCW: et em es, pts2 CCW: et em es, pts3 CCW: es em et, pts4 CCW: es em et
  #Line1 e2s e2m, Line2 e2t e1s, Line3 e1m e1t
  
  #Making Empty Matrix of 0's to store CNC values in
  CNC12 <- matrix(rep(0, length(pairs12)*3), ncol=3)
  
  for(x in 1:length(pairs12)){
    m <- pairs12[[x]][1]
    n <- pairs12[[x]][2]
    
    #Calculating Intersections of 3 lines made by the six extrema points
    P12 <- findIntersectionfromPts(pts2[[n]][3,], pts2[[n]][2,], pts2[[n]][1,], pts1[[m]][3,])
    P23 <- findIntersectionfromPts(pts2[[n]][1,], pts1[[m]][3,], pts1[[m]][2,], pts1[[m]][1,])
    P31 <- findIntersectionfromPts(pts1[[m]][2,], pts1[[m]][1,], pts2[[n]][3,], pts2[[n]][2,])
    
    #Expressing each of 6 points (Q1-Q6) as linear combinations of two of the intersection points(P12, P23, P31) 
    # that it passes through with format Q = alph* Pa + bet* Pb
    alphbetA <- calcPointLinearCombination(pts2[[n]][3,], P31, P12)
    alphbetB <- calcPointLinearCombination(pts2[[n]][2,], P31, P12)
    alphbetC <- calcPointLinearCombination(pts2[[n]][1,], P12, P23)
    alphbetD <- calcPointLinearCombination(pts1[[m]][3,], P12, P23)
    alphbetE <- calcPointLinearCombination(pts1[[m]][2,], P23, P31)
    alphbetF <- calcPointLinearCombination(pts1[[m]][1,], P23, P31)
    
    #Finding Characteristic Number for Conic CNC by multiplying bets together and dividing by alphs
    CNC <- alphbetA[2]*alphbetB[2]*alphbetC[2]*alphbetD[2]*alphbetE[2]*alphbetF[2]/alphbetA[1]/alphbetB[1]/alphbetC[1]/alphbetD[1]/alphbetE[1]/alphbetF[1]
    
    #Removing those not close enough the the CNC value of 1 for ellipses
    if(abs(1-CNC) <= threshCNC){
      CNC12[x,] <- c(CNC, m, n)
    }
  }
  #Removing Empty Rows of CNC Matrix
  CNC12 <- CNC12[ CNC12[,2] !=0 ,]
  
  #Finding CNC Values for each pair in quadrants 1 and 4, and thresholding with paramCNC
  #Explanation of each part of loop can be found in CNC12 example above
  #Internal notes follow:
  #Ordering Points: pts1 CCW: et em es, pts2 CCW: et em es, pts3 CCW: es em et, pts4 CCW: es em et
  #Line1 e1s e1m, Line2 e1t e4t, Line3 e4m e4s
  CNC14 <- matrix(rep(0, length(pairs14)*3), ncol=3)
  
  for(x in 1:length(pairs14)){
    m <- pairs14[[x]][1]
    n <- pairs14[[x]][2]
    
    P12 <- findIntersectionfromPts(pts1[[m]][3,], pts1[[m]][2,], pts1[[m]][1,], pts4[[n]][3,])
    P23 <- findIntersectionfromPts(pts1[[m]][1,], pts4[[n]][3,], pts4[[n]][2,], pts4[[n]][1,])
    P31 <- findIntersectionfromPts(pts4[[n]][2,], pts4[[n]][1,], pts1[[m]][3,], pts1[[m]][2,])
    alphbetA <- calcPointLinearCombination(pts1[[m]][3,], P31, P12)
    alphbetB <- calcPointLinearCombination(pts1[[m]][2,], P31, P12)
    alphbetC <- calcPointLinearCombination(pts1[[m]][1,], P12, P23)
    alphbetD <- calcPointLinearCombination(pts4[[n]][3,], P12, P23)
    alphbetE <- calcPointLinearCombination(pts4[[n]][2,], P23, P31)
    alphbetF <- calcPointLinearCombination(pts4[[n]][1,], P23, P31)
    
    CNC <- alphbetA[2]*alphbetB[2]*alphbetC[2]*alphbetD[2]*alphbetE[2]*alphbetF[2]/alphbetA[1]/alphbetB[1]/alphbetC[1]/alphbetD[1]/alphbetE[1]/alphbetF[1]
    
    if(abs(1-CNC) <= threshCNC){
      CNC14[x,] <- c(CNC, m, n)
    }
  }
  CNC14 <- CNC14[ CNC14[,2] !=0 ,]
  
  #Finding CNC Values for each pair in quadrants 1 and 4, and thresholding with paramCNC
  #Explanation of each part of loop can be found in CNC12 example above
  #Internal notes follow:
  #Ordering Points: pts1 CCW: et em es, pts2 CCW: et em es, pts3 CCW: es em et, pts4 CCW: es em et
  #Line1 e3t e3m, Line2 e3s e2s, Line3 e2m e2t
  CNC32 <- matrix(rep(0, length(pairs32)*3), ncol=3)
  
  for(x in 1:length(pairs32)){
    m <- pairs32[[x]][1]
    n <- pairs32[[x]][2]
    
    P12 <- findIntersectionfromPts(pts3[[m]][3,], pts3[[m]][2,], pts3[[m]][1,], pts2[[n]][3,])
    P23 <- findIntersectionfromPts(pts3[[m]][1,], pts2[[n]][3,], pts2[[n]][2,], pts2[[n]][1,])
    P31 <- findIntersectionfromPts(pts2[[n]][2,], pts2[[n]][1,], pts3[[m]][3,], pts3[[m]][2,])
    alphbetA <- calcPointLinearCombination(pts3[[m]][3,], P31, P12)
    alphbetB <- calcPointLinearCombination(pts3[[m]][2,], P31, P12)
    alphbetC <- calcPointLinearCombination(pts3[[m]][1,], P12, P23)
    alphbetD <- calcPointLinearCombination(pts2[[n]][3,], P12, P23)
    alphbetE <- calcPointLinearCombination(pts2[[n]][2,], P23, P31)
    alphbetF <- calcPointLinearCombination(pts2[[n]][1,], P23, P31)
    
    CNC <- alphbetA[2]*alphbetB[2]*alphbetC[2]*alphbetD[2]*alphbetE[2]*alphbetF[2]/alphbetA[1]/alphbetB[1]/alphbetC[1]/alphbetD[1]/alphbetE[1]/alphbetF[1]
    
    if(abs(1-CNC) <= threshCNC){
      CNC32[x,] <- c(CNC, m, n)
    }
  }
  CNC32 <- CNC32[ CNC32[,2] !=0 ,]
  
  #Finding CNC Values for each pair in quadrants 1 and 4, and thresholding with paramCNC
  #Explanation of each part of loop can be found in CNC12 example above
  #Internal notes follow:
  #Ordering Points: pts1 CCW: et em es, pts2 CCW: et em es, pts3 CCW: es em et, pts4 CCW: es em et
  #Line1 e3s e3m, Line2 e3t e4s, Line3 e4m e4t
  CNC34 <- matrix(rep(0, length(pairs34)*3), ncol=3)
  
  for(x in 1:length(pairs34)){
    m <- pairs34[[x]][1]
    n <- pairs34[[x]][2]
    P12 <- findIntersectionfromPts(pts3[[m]][1,], pts3[[m]][2,], pts3[[m]][3,], pts4[[n]][1,])
    P23 <- findIntersectionfromPts(pts3[[m]][3,], pts4[[n]][1,], pts4[[n]][2,], pts4[[n]][3,])
    P31 <- findIntersectionfromPts(pts4[[n]][2,], pts4[[n]][3,], pts3[[m]][1,], pts3[[m]][2,])
    alphbetA <- calcPointLinearCombination(pts3[[m]][1,], P31, P12)
    alphbetB <- calcPointLinearCombination(pts3[[m]][2,], P31, P12)
    alphbetC <- calcPointLinearCombination(pts3[[m]][3,], P12, P23)
    alphbetD <- calcPointLinearCombination(pts4[[n]][1,], P12, P23)
    alphbetE <- calcPointLinearCombination(pts4[[n]][2,], P23, P31)
    alphbetF <- calcPointLinearCombination(pts4[[n]][3,], P23, P31)
    
    CNC <- alphbetA[2]*alphbetB[2]*alphbetC[2]*alphbetD[2]*alphbetE[2]*alphbetF[2]/alphbetA[1]/alphbetB[1]/alphbetC[1]/alphbetD[1]/alphbetE[1]/alphbetF[1]
    
    if(abs(1-CNC) <= threshCNC){
      CNC34[x,] <- c(CNC, m, n)
    }
  }
  CNC34 <- CNC34[ CNC34[,2] !=0 ,]
  print(CNC12[1:3,])
  Rprof(NULL)
  #Garbage Collecting
  #-----------------------------------------------------------------------------------------------------------------------------
  #}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}} 
  
  
  
  
  #Estimating Ellipse Centers for Each Valid Contour Pair
  #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
  #-----------------------------------------------------------------------------------------------------------------------------
  #Calculating Centers of Each Probable Ellipse Pair
  print(Sys.time() - start)
  print("Calculating Centers of Possible Ellipses Passing CNC Threshold Check... (CenterCalculation.prof)...")
  Rprof("CenterCalculation.prof")
  
  Rprof(NULL)
  #Garbage Collecting
  #-----------------------------------------------------------------------------------------------------------------------------
  #}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}} 
  
  
  
  #Finding Constants for Each Ellipse
  #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
  #-----------------------------------------------------------------------------------------------------------------------------
  #Calculating Parameters for Each Ellipse Passing CNC Threshold Check
  print(Sys.time() - start)
  print("Matching Those Contour Pairs That Could Describe an Ellipse... (EllipseParameters.prof)...")
  Rprof("EllipseParameters.prof")
  
  Rprof(NULL)
  #Garbage Collecting
  #-----------------------------------------------------------------------------------------------------------------------------
  #}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}} 
  
  
  
  #Ellipse Geometry Checking
  #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
  #-----------------------------------------------------------------------------------------------------------------------------
  #Checking to See if Found Ellipses Meet Search Criteria
  print(Sys.time() - start)
  print("Parsing Ellipse Constants to Find Only Those We Want... (EllipseParsing.prof)...")
  Rprof("EllipseParsing.prof")
  
  Rprof(NULL)
  #Garbage Collecting
  #-----------------------------------------------------------------------------------------------------------------------------
  #}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}} 
  
  
  
  #Writing Images to Files
  #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
  pathResize = paste0(dir, "/", pathResizeImage, ".png")
  pathOutline = paste0(dir, "/", pathOutlineImage, ".png")
  pathHighlight = paste0(dir, "/", pathHighlightImage, ".png")
  pathContour = paste0(dir, "/", pathContourImage, ".png")
  
  #-----------------------------------------------------------------------------------------------------------------------------
  #Profiling Image Saving
  print(Sys.time() - start)
  print("Saving Output Images to File (ImageCreation.prof)...")
  Rprof("ImageCreation.prof")
  
  #Saving Resized Image
  save.image(imgTest, pathResize)
  
  #Saving Black and White Edges
  imgEdge <- as.cimg(pxEdges)
  save.image(imgEdge, pathOutline)
  
  #Saving Resized Image With Edges Highlighted
  png(filename=pathHighlight)
  plot.new()
  plot(imgTest)
  highlight(pxEdges, col="red")
  dev.off()
  
  #Plotting and Saving Contours Separated
  png(filename=pathContour)
  plot.new()
  plot(pxEdges)
  
  purrr::walk(conts1, function(v) lines(v[,1], v[,2], col="cyan"))
  purrr::walk(conts2, function(v) lines(v[,1], v[,2], col="magenta"))
  purrr::walk(conts3, function(v) lines(v[,1], v[,2], col="yellow"))
  purrr::walk(conts4, function(v) lines(v[,1], v[,2], col="red"))
  
  dev.off()
  
  Rprof(NULL)
  
  #-----------------------------------------------------------------------------------------------------------------------------
  
  #Moving Stats Files to Stats Folder
  profs <- c(list.files()[grepl("*.prof", list.files())])
  profs2 <- sapply(profs, FUN = function(v) {paste0(pathStats, "/", v)})
  invisible(file.copy(profs, profs2))
  unlink(profs)
  rm(profs, profs2)
  
  print(Sys.time() - start)
  print("Done!")
  #}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
  
}

saveCurrentOutputFolder <- function(folder,number){
  if(file.exists(paste0("Ellipse2Outputs/",folder,number))){
    print("Error: Folder already exists!")
    break
  }
  dir.create(paste0("Ellipse2Outputs/",folder,number))
  file.copy("./outputtemp/", paste0("Ellipse2Outputs/",folder,number), recursive = TRUE)
}

runJiaFanEllipses("./Test Pics/Danbooru8-6-20-1235pmEST/test13.jpg")
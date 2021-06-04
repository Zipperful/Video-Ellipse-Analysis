#Takes constants and an image and generates ellipse fragment images from it, returns speed and counts of arcs for each type


##TOD Random
# Alter line-spreading logic to break at T junctions rather than follow one
# Line prune more aggressive, thresh length more aggressively?
# Prune Right Angles out in line pruning step by rejecting those with areas too close to maximum, by 90%
#Fix midpoint formula for very diff sizes like the first two quadrants have it

##TWO vs 3 Parameter fitting approach
#  For two:  Find centers via leave-one-out approach on midpoints and find mean to find center, calc best N and p from slopes
#           Consider only looking for more curved segments and splitting in 3 for twofold approach on all
#  For three: Find centers via mean of intersections and calc best N and P from

#Validation
#  Ratio on number of fitted points to points that generated ellipse
#  Better fits generated when length of 2 segments is longer than sum of semimajor axes

#Duplication removal from [19]

library(imager)
library(dplyr)
library(OpenImageR)
library(VideoTrackingUtilities)

pathImageToLoad <- "./Test Pics/Danbooru8-6-20-1235pmEST/test2.jpg"
# pathFolderToMake = "SketchDanb2Output"
# paramScale=4
# paramAlpha=0.2
# paramSigma=2
# paramLengthCutoff=16
# paramLineArea=2


# pathImageToLoad <- "./Test Pics/FoodWarTests/test6.JPG"
paramLineArea = 2
# runJiaFanEllipses <- function(pathImageToLoad, 
#                               pathFolderToMake = "outputtemp", 
#                               paramScale=2, 
#                               paramAlpha=0.2, 
#                               paramSigma=2, 
#                               paramLengthCutoff=16, 
#                               paramLineArea=2
#                               paramCNC = 0.2){
pathFolderToMake = "outputtemp"
paramScale=2
paramAlpha=0.2
paramSigma=1
paramLengthCutoff=16
paramMidpoints=16
# paramLineArea=2
paramCNC = 0.3

  
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
  
  #Testing 8count image split

  eightSpread <- function(tau){
    px <- as.pixset(tau)
    indices <- which(px, arr.ind=TRUE)
    n1 <- c(-1,-1); n2 <- c(0,-1); n3 <- c(1,-1); n4 <- c(-1,0); n5 <- c(1,0); n6 <- c(-1, 1); n7 <- c(0,1); n8 <- c(1,1) 
    neighborhood <- matrix(c(n1,n2,n3,n4,n5,n6,n7,n8), ncol=2, byrow = TRUE)
    cont=0
    contList <- vector(mode = "list", length=nrow(indices))
    while(sum(px>0)){
      #recurse
      cont <- cont + 1
      pts <- list(indices[1,])
      index <- indices[1,]
      points
      neighbors <- neighborhood+index
      
    }
  }
  
  
  
  
    
#   
#   
#   #-----------------------------------------------------------------------------------------------------------------------------
#   #Profiling Contour Connectivity
#   print(Sys.time() - start)
#   print("Finding First Contour Groups Connectivity (ContourConnectivity.prof)...")
#   Rprof("ContourConnectivity.prof")
#   
#   #Finds out how contours are connected and stores each seperate contour in a list
#   mtCont2 <- storeContours(spreadEdges(tau2[,,1,1]), threshEdge)
#   mtCont1 <- storeContours(rotMatrixCCW(spreadEdges(tau1rot)), threshEdge)
#   
#   Rprof(NULL)
#   
#   #-----------------------------------------------------------------------------------------------------------------------------
#   #Profiling Contour Extrema
#   print(Sys.time() - start)
#   print("Finding Extrema of First Group Contours (ContourExtrema.prof)...")
#   Rprof("ContourExtrema.prof")
#   
#   #Finds extrema of contours for CNC calculation and threshholding
#   mtExtremaPos <- posContPoints(mtCont1)
#   mtExtremaNeg <- negContPoints(mtCont2)
#   
#   Rprof(NULL)
#   
#   #-----------------------------------------------------------------------------------------------------------------------------
#   #Profiling Line Removal
#   print(Sys.time() - start)
#   print("Removing Straight Contours (ContourLineThresholding.prof)...")
#   Rprof("ContourLineThresholding.prof")
#   
#   #Removing points that describe near-straight lines by finding near-0 areas of triangles between points
#   mtExtremaNegAreas <- mtExtremaNeg
#   mtExtremaPosAreas <- mtExtremaPos
#   findTriangleArea <- function( mtPts ) {
#     mtPts <- matrix(c(mtPts, c(1,1,1)), nrow=3)
#     return(det(mtPts))
#   }
#   for(x in 1:length(mtCont2)){
#     mtExtremaNegAreas[[x]] <- findTriangleArea(c(mtExtremaNeg[[x]])) / nrow(mtCont2[[x]])
#   }
#   for(x in 1:length(mtCont1)){
#     mtExtremaPosAreas[[x]] <- findTriangleArea(c(mtExtremaPos[[x]])) / nrow(mtCont1[[x]])
#   }
#   posThreshLines <-abs(unlist(mtExtremaPosAreas))>threshLine
#   negThreshLines <-abs(unlist(mtExtremaNegAreas))>threshLine
#   mtCont1 <- mtCont1[ posThreshLines]
#   mtCont2 <- mtCont2[ negThreshLines]
#   mtExtremaPos <- mtExtremaPos[ posThreshLines ]
#   mtExtremaNeg <- mtExtremaNeg[ negThreshLines ]
#   
#   Rprof(NULL)
#   
#   #-----------------------------------------------------------------------------------------------------------------------------
#   #Garbage Collecting
#   # rm(grayTest, dx,dy,tau, tau1, tau1rot, tau2, mtExtremaNegAreas, mtExtremaPosAreas)
#   #}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
#   
#   
#   
#   #Splitting Curves into Two More Sets Based on Concavity Obtained by Summing Y Values in Contours and Plots
#   #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
#   #-----------------------------------------------------------------------------------------------------------------------------
#   #Profiling Second Contour Separation
#   print(Sys.time() - start)
#   print("Separating Contours and Extrema Down into Two More Groups Based on Concavity (ContourSeparation2.prof)...")
#   Rprof("ContourSeparation2.prof")
#   
#   concavities24 <- concavitySplitter(mtCont1, mtExtremaPos)
#   conts2 <- concavities24[[2]]
#   conts4 <- concavities24[[1]]
#   pts2 <- concavities24[[4]]
#   pts4 <- concavities24[[3]]
#   
#   concavities13 <- concavitySplitter(mtCont2, mtExtremaNeg)
#   conts1 <- concavities13[[2]]
#   conts3 <- concavities13[[1]]
#   pts1 <- concavities13[[4]]
#   pts3 <- concavities13[[3]]
#   
#   
#   Rprof(NULL)
#   #Garbage Collecting
#   # rm(concavities13, concavities24, mtCont1, mtCont2, mtExtremaNeg, mtExtremaPos)
#   #-----------------------------------------------------------------------------------------------------------------------------
#   #}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
#   
#   
#   
#   #Finding Viable Pairs of Contours To Compare
#   #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
#   #-----------------------------------------------------------------------------------------------------------------------------
#   #Profiling Matching Contours to Each Other for Possible Ellipse Detection
#   print(Sys.time() - start)
#   print("Matching Those Contour Pairs That Could Describe an Ellipse... (ContourMatching.prof)...")
#   Rprof("ContourMatching.prof")
#   
#   #Ordering Points: pts1 CCW: et em es, pts2 CCW: et em es, pts3 CCW: es em et, pts4 CCW: es em et
#   #Order Pts1 by decreasing x,y
#   for(x in 1:length(pts1)){
#     pts1[[x]] <- pts1[[x]][order(pts1[[x]][,1], decreasing=TRUE),]
#   }
#   #Order Pts2 by decreasing x, increasing y
#   for(x in 1:length(pts2)){
#     pts2[[x]] <- pts2[[x]][order(pts2[[x]][,2]),]
#   }
#   #Order Pts3 by increasing x,y
#   for(x in 1:length(pts3)){
#     pts3[[x]] <- pts3[[x]][order(pts3[[x]][,1]),]
#   }
#   #Order Pts4 by increasing x, decreasing y
#   for(x in 1:length(pts4)){
#     pts4[[x]] <- pts4[[x]][order(pts4[[x]][,1]),]
#   }
#   
#   #Matching Pts1 to Pts2- pts1esx > pts2etx
#   pairs12 <- vector(mode = "list", length=length(pts1)*length(pts2))
#   for(x in 1:length(pts1)){
#     for(y in 1:length(pts2)){
#       if(pts1[[x]][3,1] >= pts2[[y]][1,1]){
#         pairs12[[(x-1)*length(pts2)+y]] <- c(x,y)
#       }
#     }
#   }
#   pairs12 <- pairs12[!sapply(pairs12, is.null)]
#   
#   #Matching Pts1 to Pts4- pts1ety < pts4ety
#   pairs14 <- vector(mode = "list", length=length(pts1)*length(pts4))
#   for(x in 1:length(pts1)){
#     for(y in 1:length(pts4)){
#       if(pts1[[x]][1,2] <= pts4[[y]][3,2]){
#         pairs14[[(x-1)*length(pts4)+y]] <- c(x,y)
#       }
#     }
#   }
#   pairs14 <- pairs14[!sapply(pairs14, is.null)]
#   
#   #Matching Pts3 to Pts2- pts3esy > pts2esy
#   pairs32 <- vector(mode = "list", length=length(pts3)*length(pts2))
#   for(x in 1:length(pts3)){
#     for(y in 1:length(pts2)){
#       if(pts3[[x]][1,2] >= pts2[[y]][3,2]){
#         pairs32[[(x-1)*length(pts2)+y]] <- c(x,y)
#       }
#     }
#   }
#   pairs32 <- pairs32[!sapply(pairs32, is.null)]
#   
#   #Matching Pts3 to Pts4- pts3etx < pts4esx
#   pairs34 <- vector(mode = "list", length=length(pts3)*length(pts4))
#   for(x in 1:length(pts3)){
#     for(y in 1:length(pts4)){
#       if(pts3[[x]][3,1] <= pts4[[y]][1,1]){
#         pairs34[[(x-1)*length(pts4)+y]] <- c(x,y)
#       }
#     }
#   }
#   pairs34 <- pairs34[!sapply(pairs34, is.null)]
#   
#   Rprof(NULL)
#   #Garbage Collecting
#   #-----------------------------------------------------------------------------------------------------------------------------
#   #}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}  
#   
#   
#   
#   #Calculating CNC for Each Pair
#   #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
#   #-----------------------------------------------------------------------------------------------------------------------------
#   threshCNC <- paramCNC
#   #Calculating Characteristic Number Cutoff (CNC) for Every Possible Pair of Ellipses and Thresholding
#   print(Sys.time() - start)
#   print("Calculating and Thresholding Possilbe Ellipses Based on CNC ... (CNCThresholding.prof)...")
#   Rprof("CNCThresholding.prof")
# 
#   #Finding CNC Values for each pair in quadrants 1 and 2, and thresholding with paramCNC
#   #Internal notes follow:
#   #Ordering Points: pts1 CCW: et em es, pts2 CCW: et em es, pts3 CCW: es em et, pts4 CCW: es em et
#   #Line1 e2s e2m, Line2 e2t e1s, Line3 e1m e1t
#   
#   #Making Empty Matrix of 0's to store CNC values in
#   CNC12 <- matrix(rep(0, length(pairs12)*3), ncol=3)
#   
#   for(x in 1:length(pairs12)){
#     m <- pairs12[[x]][1]
#     n <- pairs12[[x]][2]
#     
#     #Calculating Intersections of 3 lines made by the six extrema points
#     P12 <- findIntersectionfromPts(pts2[[n]][3,], pts2[[n]][2,], pts2[[n]][1,], pts1[[m]][3,])
#     P23 <- findIntersectionfromPts(pts2[[n]][1,], pts1[[m]][3,], pts1[[m]][2,], pts1[[m]][1,])
#     P31 <- findIntersectionfromPts(pts1[[m]][2,], pts1[[m]][1,], pts2[[n]][3,], pts2[[n]][2,])
#     
#     #Expressing each of 6 points (Q1-Q6) as linear combinations of two of the intersection points(P12, P23, P31) 
#     # that it passes through with format Q = alph* Pa + bet* Pb
#     alphbetA <- calcPointLinearCombination(pts2[[n]][3,], P31, P12)
#     alphbetB <- calcPointLinearCombination(pts2[[n]][2,], P31, P12)
#     alphbetC <- calcPointLinearCombination(pts2[[n]][1,], P12, P23)
#     alphbetD <- calcPointLinearCombination(pts1[[m]][3,], P12, P23)
#     alphbetE <- calcPointLinearCombination(pts1[[m]][2,], P23, P31)
#     alphbetF <- calcPointLinearCombination(pts1[[m]][1,], P23, P31)
#     
#     #Finding Characteristic Number for Conic CNC by multiplying bets together and dividing by alphs
#     CNC <- alphbetA[2]*alphbetB[2]*alphbetC[2]*alphbetD[2]*alphbetE[2]*alphbetF[2]/alphbetA[1]/alphbetB[1]/alphbetC[1]/alphbetD[1]/alphbetE[1]/alphbetF[1]
# 
#     #Removing those not close enough the the CNC value of 1 for ellipses
#     if(abs(1-CNC) <= threshCNC){
#       CNC12[x,] <- c(CNC, m, n)
#     }
#   }
#   #Removing Empty Rows of CNC Matrix and Saving List of contour pair indexes, contour pairs, and extrema of pairs
#   CNC12 <- CNC12[ CNC12[,2] !=0 ,]
#   pairs12 <- vector(mode = "list", length=nrow(CNC12))
#   pts12 <- vector(mode = "list", length=nrow(CNC12))
#   conts12 <- vector(mode = "list", length=nrow(CNC12))
#   for(x in 1:nrow(CNC12)) {
#     pairs12[[x]] <- CNC12[x,2:3]
#     pts12[[x]] <- list(pts1[[CNC12[x,2]]], pts2[[CNC12[x,3]]])
#     conts12[[x]] <- list(conts1[[CNC12[x,2]]], conts2[[CNC12[x,3]]])
#   }
#   
#   
#   #Finding CNC Values for each pair in quadrants 1 and 4, and thresholding with paramCNC
#   #Explanation of each part of loop can be found in CNC12 example above
#   #Internal notes follow:
#   #Ordering Points: pts1 CCW: et em es, pts2 CCW: et em es, pts3 CCW: es em et, pts4 CCW: es em et
#   #Line1 e1s e1m, Line2 e1t e4t, Line3 e4m e4s
#   CNC14 <- matrix(rep(0, length(pairs14)*3), ncol=3)
#   
#   for(x in 1:length(pairs14)){
#     m <- pairs14[[x]][1]
#     n <- pairs14[[x]][2]
#     
#     P12 <- findIntersectionfromPts(pts1[[m]][3,], pts1[[m]][2,], pts1[[m]][1,], pts4[[n]][3,])
#     P23 <- findIntersectionfromPts(pts1[[m]][1,], pts4[[n]][3,], pts4[[n]][2,], pts4[[n]][1,])
#     P31 <- findIntersectionfromPts(pts4[[n]][2,], pts4[[n]][1,], pts1[[m]][3,], pts1[[m]][2,])
#     alphbetA <- calcPointLinearCombination(pts1[[m]][3,], P31, P12)
#     alphbetB <- calcPointLinearCombination(pts1[[m]][2,], P31, P12)
#     alphbetC <- calcPointLinearCombination(pts1[[m]][1,], P12, P23)
#     alphbetD <- calcPointLinearCombination(pts4[[n]][3,], P12, P23)
#     alphbetE <- calcPointLinearCombination(pts4[[n]][2,], P23, P31)
#     alphbetF <- calcPointLinearCombination(pts4[[n]][1,], P23, P31)
#     
#     CNC <- alphbetA[2]*alphbetB[2]*alphbetC[2]*alphbetD[2]*alphbetE[2]*alphbetF[2]/alphbetA[1]/alphbetB[1]/alphbetC[1]/alphbetD[1]/alphbetE[1]/alphbetF[1]
#     
#     if(abs(1-CNC) <= threshCNC){
#       CNC14[x,] <- c(CNC, m, n)
#     }
#   }
#   CNC14 <- CNC14[ CNC14[,2] !=0 ,]
#   pairs14 <- vector(mode = "list", length=nrow(CNC14))
#   pts14 <- vector(mode = "list", length=nrow(CNC14))
#   conts14 <- vector(mode = "list", length=nrow(CNC14))
#   for(x in 1:nrow(CNC14)) {
#     pairs14[[x]] <- CNC14[x,2:3]
#     pts14[[x]] <- list(pts1[[CNC14[x,2]]], pts4[[CNC14[x,3]]])
#     conts14[[x]] <- list(conts1[[CNC14[x,2]]], conts4[[CNC14[x,3]]])
#   }
#   
#   
#   #Finding CNC Values for each pair in quadrants 1 and 4, and thresholding with paramCNC
#   #Explanation of each part of loop can be found in CNC12 example above
#   #Internal notes follow:
#   #Ordering Points: pts1 CCW: et em es, pts2 CCW: et em es, pts3 CCW: es em et, pts4 CCW: es em et
#   #Line1 e3t e3m, Line2 e3s e2s, Line3 e2m e2t
#   CNC32 <- matrix(rep(0, length(pairs32)*3), ncol=3)
#   
#   for(x in 1:length(pairs32)){
#     m <- pairs32[[x]][1]
#     n <- pairs32[[x]][2]
#     
#     P12 <- findIntersectionfromPts(pts3[[m]][3,], pts3[[m]][2,], pts3[[m]][1,], pts2[[n]][3,])
#     P23 <- findIntersectionfromPts(pts3[[m]][1,], pts2[[n]][3,], pts2[[n]][2,], pts2[[n]][1,])
#     P31 <- findIntersectionfromPts(pts2[[n]][2,], pts2[[n]][1,], pts3[[m]][3,], pts3[[m]][2,])
#     alphbetA <- calcPointLinearCombination(pts3[[m]][3,], P31, P12)
#     alphbetB <- calcPointLinearCombination(pts3[[m]][2,], P31, P12)
#     alphbetC <- calcPointLinearCombination(pts3[[m]][1,], P12, P23)
#     alphbetD <- calcPointLinearCombination(pts2[[n]][3,], P12, P23)
#     alphbetE <- calcPointLinearCombination(pts2[[n]][2,], P23, P31)
#     alphbetF <- calcPointLinearCombination(pts2[[n]][1,], P23, P31)
#     
#     CNC <- alphbetA[2]*alphbetB[2]*alphbetC[2]*alphbetD[2]*alphbetE[2]*alphbetF[2]/alphbetA[1]/alphbetB[1]/alphbetC[1]/alphbetD[1]/alphbetE[1]/alphbetF[1]
#     
#     if(abs(1-CNC) <= threshCNC){
#       CNC32[x,] <- c(CNC, m, n)
#     }
#   }
#   CNC32 <- CNC32[ CNC32[,2] !=0 ,]
#   pairs32 <- vector(mode = "list", length=nrow(CNC32))
#   pts32 <- vector(mode = "list", length=nrow(CNC32))
#   conts32 <- vector(mode = "list", length=nrow(CNC32))
#   for(x in 1:nrow(CNC32)) {
#     pairs32[[x]] <- CNC32[x,2:3]
#     pts32[[x]] <- list(pts3[[CNC32[x,2]]], pts2[[CNC32[x,3]]])
#     conts32[[x]] <- list(conts3[[CNC32[x,2]]], conts2[[CNC32[x,3]]])
#   }
#   
#   
#   #Finding CNC Values for each pair in quadrants 1 and 4, and thresholding with paramCNC
#   #Explanation of each part of loop can be found in CNC12 example above
#   #Internal notes follow:
#   #Ordering Points: pts1 CCW: et em es, pts2 CCW: et em es, pts3 CCW: es em et, pts4 CCW: es em et
#   #Line1 e3s e3m, Line2 e3t e4s, Line3 e4m e4t
#   CNC34 <- matrix(rep(0, length(pairs34)*3), ncol=3)
#   
#   for(x in 1:length(pairs34)){
#     m <- pairs34[[x]][1]
#     n <- pairs34[[x]][2]
#     P12 <- findIntersectionfromPts(pts3[[m]][1,], pts3[[m]][2,], pts3[[m]][3,], pts4[[n]][1,])
#     P23 <- findIntersectionfromPts(pts3[[m]][3,], pts4[[n]][1,], pts4[[n]][2,], pts4[[n]][3,])
#     P31 <- findIntersectionfromPts(pts4[[n]][2,], pts4[[n]][3,], pts3[[m]][1,], pts3[[m]][2,])
#     alphbetA <- calcPointLinearCombination(pts3[[m]][1,], P31, P12)
#     alphbetB <- calcPointLinearCombination(pts3[[m]][2,], P31, P12)
#     alphbetC <- calcPointLinearCombination(pts3[[m]][3,], P12, P23)
#     alphbetD <- calcPointLinearCombination(pts4[[n]][1,], P12, P23)
#     alphbetE <- calcPointLinearCombination(pts4[[n]][2,], P23, P31)
#     alphbetF <- calcPointLinearCombination(pts4[[n]][3,], P23, P31)
#     
#     CNC <- alphbetA[2]*alphbetB[2]*alphbetC[2]*alphbetD[2]*alphbetE[2]*alphbetF[2]/alphbetA[1]/alphbetB[1]/alphbetC[1]/alphbetD[1]/alphbetE[1]/alphbetF[1]
#     
#     if(abs(1-CNC) <= threshCNC){
#       CNC34[x,] <- c(CNC, m, n)
#     }
#   }
#   CNC34 <- CNC34[ CNC34[,2] !=0 ,]
#   pairs34 <- vector(mode = "list", length=nrow(CNC34))
#   pts34 <- vector(mode = "list", length=nrow(CNC34))
#   conts34 <- vector(mode = "list", length=nrow(CNC34))
#   for(x in 1:nrow(CNC34)) {
#     pairs34[[x]] <- CNC34[x,2:3]
#     pts34[[x]] <- list(pts3[[CNC34[x,2]]], pts4[[CNC34[x,3]]])
#     conts34[[x]] <- list(conts3[[CNC34[x,2]]], conts4[[CNC34[x,3]]])
#   }
#   
#   Rprof(NULL)
#   
#   #Test plotting of all successful contours in CNC12
#   plot(pxEdges)
#   purrr::walk(conts1[CNC12[1,2]], function(v) lines(v[,1], v[,2], col="cyan"))
#   purrr::walk(conts2[CNC12[1,3]], function(v) lines(v[,1], v[,2], col="yellow"))
#   #Garbage Collecting
#   #-----------------------------------------------------------------------------------------------------------------------------
#   #}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}} 
#   
# 
#   
#   #Estimating Ellipse Centers for Each Valid Contour Pair
#   #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
#   #-----------------------------------------------------------------------------------------------------------------------------
#   midCount <- paramMidpoints
#   #PCalculating Centers of Each Probable Ellipse Pair
#   print(Sys.time() - start)
#   print("Calculating Centers of Possible Ellipses Passing CNC Threshold Check... (CenterCalculation.prof)...")
#   Rprof("CenterCalculation.prof")
#   
#   #Finding Chords for Pairs in 12 to calculate 
#   # For 12, need bottom of 2 to mid of 1, bottom of 1 to mid of 2
#   #Internal notes follow:
#   #Ordering Points: pts1 CCW: et em es, pts2 CCW: et em es, pts3 CCW: es em et, pts4 CCW: es em et
#   
#   ptsToLine2 <- function(pt1, pt2){
#     y1s <- matrix(c(pt1[2], pt2[2], 1, 1), nrow=2)
#     x1s <- matrix(c(pt1[1], pt2[1], 1, 1), nrow=2)
#     xys <- matrix(c(pt1[1], pt2[1], pt1[2], pt2[2]), nrow=2)
#     return(c(det(y1s), -det(x1s), -det(xys)))
#   }
#   
#   findMidpoint <- function(pt1, pt2){
#     x <- mean( c(pt1[1], pt2[1]) )
#     y <- mean( c(pt1[2], pt2[2]) )
#     return(c( x,y ) )
#   }
#   
#   #-----------------------------------------------------------------------------------------------------------------------------
#   
#   slopes12 <- slopes21 <- midpoints12 <- midpoints21 <- vector(mode = "list", length=length(pairs12))
#   slopes14 <- slopes41 <- midpoints14 <- midpoints41 <- vector(mode = "list", length=length(pairs14))
#   slopes32 <- slopes23 <- midpoints32 <- midpoints23 <- vector(mode = "list", length=length(pairs32))
#   slopes34 <- slopes43 <- midpoints34 <- midpoints43 <- vector(mode = "list", length=length(pairs34))
# 
#   #-----------------------------------------------------------------------------------------------------------------------------
#   #Internal notes follow:
#   #Ordering Points: pts1 CCW: et em es, pts2 CCW: et em es, pts3 CCW: es em et, pts4 CCW: es em et
#   plot(pxEdges)
#   for(x in 1:length(pairs12)){
#   # for(x in 2:2){
#     pt1bot <- pts12[[x]][[1]][1,]
#     pt1mid <- pts12[[x]][[1]][2,]
#     pt1top <- pts12[[x]][[1]][3,]
#     pt2bot <- pts12[[x]][[2]][3,]
#     pt2mid <- pts12[[x]][[2]][2,]
#     pt2top <- pts12[[x]][[2]][1,]
#     slope12 <- (pt1bot[2]-pt2mid[2])/(pt1bot[1]-pt2mid[1])
#     slope21 <- (pt2bot[2]-pt1mid[2])/(pt2bot[1]-pt1mid[1])
#     
#     int12bot <- pt1bot[2]-slope12*pt1bot[1]
#     int12top <- pt2top[2]-slope12*pt2top[1]
#     int21bot <- pt2bot[2]-slope21*pt2bot[1]
#     int21top <- pt1top[2]-slope21*pt1top[1]
#     
#     ints12 <- seq(from=int12bot, to=int12top, length.out=midCount)
#     ints21 <- seq(from=int21bot, to=int21top, length.out=midCount)
#     
#     mids12 <- matrix(0, ncol=2, nrow=midCount)
#     mids21 <- matrix(0, ncol=2, nrow=midCount)
#     
#     for(y in 1:midCount){
#       difs12a <- abs(conts12[[x]][[1]][,2] - (conts12[[x]][[1]][,1] * slope12 + ints12[y]))
#       difs12b <- abs(conts12[[x]][[2]][,2] - (conts12[[x]][[2]][,1] * slope12 + ints12[y]))
# 
#       mids12[y,] <- findMidpoint( conts12[[x]][[1]][ which.min(difs12a) ,] , conts12[[x]][[2]][ which.min(difs12b) ,])
#       
#       difs21a <- abs(conts12[[x]][[1]][,2] - (conts12[[x]][[1]][,1] * slope21 + ints21[y]))
#       difs21b <- abs(conts12[[x]][[2]][,2] - (conts12[[x]][[2]][,1] * slope21 + ints21[y]))
# 
#       mids21[y,] <- findMidpoint( conts12[[x]][[1]][ which.min(difs21a) ,] , conts12[[x]][[2]][ which.min(difs21b) ,])
#     }
#     
#     mids12 <- unique(mids12)
#     mids12 <- mids12[ mids12[,2] >= min(conts12[[x]][[1]][,2]), ]
#     mids21 <- unique(mids21)
#     mids21 <- mids21[ mids21[,2] >= min(conts12[[x]][[2]][,2]), ]
#     
#     slopes12[[x]] <- slope12
#     slopes21[[x]] <- slope21
#     midpoints12[[x]] <- mids12
#     midpoints21[[x]] <- mids21
# 
#     lines(conts12[[x]][[1]][,1], y=conts12[[x]][[1]][,2], col="magenta")
#     lines(conts12[[x]][[2]][,1], y=conts12[[x]][[2]][,2], col="red")
#     abline(int12bot, slope12, col="cyan")
#     abline(int12top, slope12, col="cyan")
#     abline(int21top, slope21, col="yellow")
#     abline(int21bot, slope21, col="yellow")
#     points(x=mids12[,1], y=mids12[,2], col="green")
#     points(x=mids21[,1], y=mids21[,2], col="turquoise")
#   }
#   
#   #-----------------------------------------------------------------------------------------------------------------------------
#   #Internal notes follow:
#   #Ordering Points: pts1 CCW: et em es, pts2 CCW: et em es, pts3 CCW: es em et, pts4 CCW: es em et
#   plot(pxEdges)
#   for(x in 1:length(pairs14)){
#     pt1lef <- pts14[[x]][[1]][3,]
#     pt1mid <- pts14[[x]][[1]][2,]
#     pt1rig <- pts14[[x]][[1]][1,]
#     pt4lef <- pts14[[x]][[2]][1,]
#     pt4mid <- pts14[[x]][[2]][2,]
#     pt4rig <- pts14[[x]][[2]][3,]
#     slope14 <- (pt1lef[2]-pt4mid[2])/(pt1lef[1]-pt4mid[1])
#     slope41 <- (pt4lef[2]-pt1mid[2])/(pt4lef[1]-pt1mid[1])
#     
#     int14lef <- pt1lef[2]-slope14*pt1lef[1]
#     int14rig <- pt4rig[2]-slope14*pt4rig[1]
#     int41lef <- pt4lef[2]-slope41*pt4lef[1]
#     int41rig <- pt1rig[2]-slope41*pt1rig[1]
#     
#     ints14 <- seq(from=int14lef, to=int14rig, length.out=midCount)
#     ints41 <- seq(from=int41lef, to=int41rig, length.out=midCount)
#     
#     mids14 <- matrix(0, ncol=2, nrow=midCount)
#     mids41 <- matrix(0, ncol=2, nrow=midCount)
#     
#     for(y in 1:midCount){
#       difs14a <- abs(conts14[[x]][[1]][,2] - (conts14[[x]][[1]][,1] * slope14 + ints14[y]))
#       difs14b <- abs(conts14[[x]][[2]][,2] - (conts14[[x]][[2]][,1] * slope14 + ints14[y]))
# 
#       mids14[y,] <- findMidpoint( conts14[[x]][[1]][ which.min(difs14a) ,] , conts14[[x]][[2]][ which.min(difs14b) ,])
#       
#       difs41a <- abs(conts14[[x]][[1]][,2] - (conts14[[x]][[1]][,1] * slope41 + ints41[y]))
#       difs41b <- abs(conts14[[x]][[2]][,2] - (conts14[[x]][[2]][,1] * slope41 + ints41[y]))
# 
#       mids41[y,] <- findMidpoint( conts14[[x]][[1]][ which.min(difs41a) ,] , conts14[[x]][[2]][ which.min(difs41b) ,])
#     }
# 
#     mids14 <- unique(mids14)
#     mids14 <- mids14[ mids14[,1] <= max(conts14[[x]][[1]][,1]), ]
#     mids41 <- unique(mids41)
#     mids21 <- mids21[ mids21[,1] <= max(conts14[[x]][[2]][,1]), ]
#     
#     slopes14[[x]] <- slope14
#     slopes41[[x]] <- slope41
#     midpoints14[[x]] <- mids14
#     midpoints41[[x]] <- mids41
#     
#     lines(conts14[[x]][[1]][,1], y=conts14[[x]][[1]][,2], col="magenta")
#     lines(conts14[[x]][[2]][,1], y=conts14[[x]][[2]][,2], col="red")
#     # abline(int14lef, slope14, col="cyan")
#     # abline(int14rig, slope14, col="cyan")
#     # abline(int41lef, slope41, col="yellow")
#     # abline(int41rig, slope41, col="yellow")
#     points(x=mids14[,1], y=mids14[,2], col="green")
#     points(x=mids41[,1], y=mids41[,2], col="turquoise")
#   }
#   
#   #-----------------------------------------------------------------------------------------------------------------------------
#   #Internal notes follow:
#   #Ordering Points: pts1 CCW: et em es, pts2 CCW: et em es, pts3 CCW: es em et, pts4 CCW: es em et
#   plot(pxEdges)
#   for(x in 1:length(pairs32)){
#     print(x)
#     pt3lef <- pts32[[x]][[1]][1,]
#     pt3mid <- pts32[[x]][[1]][2,]
#     pt3rig <- pts32[[x]][[1]][3,]
#     pt2lef <- pts32[[x]][[2]][3,]
#     pt2mid <- pts32[[x]][[2]][2,]
#     pt2rig <- pts32[[x]][[2]][1,]
#     slope32 <- (pt3rig[2]-pt2mid[2])/(pt3rig[1]-pt2mid[1])
#     slope23 <- (pt2rig[2]-pt3mid[2])/(pt2rig[1]-pt3mid[1])
#     
#     int32rig <- pt3rig[2]-slope32*pt3rig[1]
#     int32lef <- pt2lef[2]-slope32*pt2lef[1]
#     int23rig <- pt2rig[2]-slope23*pt2rig[1]
#     int23lef <- pt3lef[2]-slope23*pt3lef[1]
#     
#     ints32 <- seq(from=int32rig, to=int32lef, length.out=midCount)
#     ints23 <- seq(from=int23rig, to=int23lef, length.out=midCount)
#     mids32 <- matrix(0, ncol=2, nrow=midCount)
#     mids23 <- matrix(0, ncol=2, nrow=midCount)
#     
#     for(y in 1:midCount){
#       difs32a <- abs(conts32[[x]][[1]][,2] - (conts32[[x]][[1]][,1] * slope32 + ints32[y]))
#       difs32b <- abs(conts32[[x]][[2]][,2] - (conts32[[x]][[2]][,1] * slope32 + ints32[y]))
#       
#       mids32[y,] <- findMidpoint( conts32[[x]][[1]][ which.min(difs32a) ,] , conts32[[x]][[2]][ which.min(difs32b) ,])
# 
#       difs23a <- abs(conts32[[x]][[1]][,2] - (conts32[[x]][[1]][,1] * slope23 + ints23[y]))
#       difs23b <- abs(conts32[[x]][[2]][,2] - (conts32[[x]][[2]][,1] * slope23 + ints23[y]))
#       
#       mids23[y,] <- findMidpoint( conts32[[x]][[1]][ which.min(difs23a) ,] , conts32[[x]][[2]][ which.min(difs23b) ,])
#     }
#     
#     mids32 <- unique(mids32)
#     mids32 <- mids32[ !is.na(mids32[,1]) & !is.na(mids32[,2]), ]
#     mids23 <- unique(mids23)
#     mids23 <- mids23[ !is.na(mids23[,1]) & !is.na(mids23[,2]), ]
#     
#     slopes32[[x]] <- slope32
#     slopes23[[x]] <- slope23
#     midpoints32[[x]] <- mids32
#     midpoints23[[x]] <- mids23
#     
#     lines(conts32[[x]][[1]][,1], y=conts32[[x]][[1]][,2], col="magenta")
#     lines(conts32[[x]][[2]][,1], y=conts32[[x]][[2]][,2], col="red")
#     # abline(int32rig, slope32, col="cyan")
#     # abline(int32lef, slope32, col="cyan")
#     # abline(int23rig, slope23, col="yellow")
#     # abline(int23lef, slope23, col="yellow")
#     points(x=mids32[,1], y=mids32[,2], col="green")
#     points(x=mids23[,1], y=mids23[,2], col="turquoise")
#   }
#   
#   #-----------------------------------------------------------------------------------------------------------------------------
#   #Internal notes follow:
#   #Ordering Points: pts1 CCW: et em es, pts2 CCW: et em es, pts3 CCW: es em et, pts4 CCW: es em et
#   plot(pxEdges)
#   for(x in 1:length(pairs34)){
#     print(x)
#     pt3top <- pts34[[x]][[1]][1,]
#     pt3mid <- pts34[[x]][[1]][2,]
#     pt3bot <- pts34[[x]][[1]][3,]
#     pt4top <- pts34[[x]][[2]][3,]
#     pt4mid <- pts34[[x]][[2]][2,]
#     pt4bot <- pts34[[x]][[2]][1,]
#     slope34 <- (pt3top[2]-pt4mid[2])/(pt3top[1]-pt4mid[1])
#     slope43 <- (pt4top[2]-pt3mid[2])/(pt4top[1]-pt3mid[1])
#     
#     int34top <- pt3top[2]-slope34*pt3top[1]
#     int34bot <- pt4bot[2]-slope34*pt4bot[1]
#     int43top <- pt4top[2]-slope43*pt4top[1]
#     int43bot <- pt3bot[2]-slope43*pt3bot[1]
#     
#     ints34 <- seq(from=int34top, to=int34bot, length.out=midCount)
#     ints43 <- seq(from=int43top, to=int43bot, length.out=midCount)
#     mids34 <- matrix(0, ncol=2, nrow=midCount)
#     mids43 <- matrix(0, ncol=2, nrow=midCount)
#     print(ints34)
#     for(y in 1:midCount){
#       difs34a <- abs(conts34[[x]][[1]][,2] - (conts34[[x]][[1]][,1] * slope34 + ints34[y]))
#       difs34b <- abs(conts34[[x]][[2]][,2] - (conts34[[x]][[2]][,1] * slope34 + ints34[y]))
#       
#       mids34[y,] <- findMidpoint( conts34[[x]][[1]][ which.min(difs34a) ,] , conts34[[x]][[2]][ which.min(difs34b) ,])
# 
#       difs43a <- abs(conts34[[x]][[1]][,2] - (conts34[[x]][[1]][,1] * slope43 + ints43[y]))
#       difs43b <- abs(conts34[[x]][[2]][,2] - (conts34[[x]][[2]][,1] * slope43 + ints43[y]))
#       
#       mids43[y,] <- findMidpoint( conts34[[x]][[1]][ which.min(difs43a) ,] , conts34[[x]][[2]][ which.min(difs43b) ,])
#     }
#     # print(mids34)
#     mids34 <- unique(mids34)
#     mids34 <- mids34[ !is.na(mids34[,1]) & !is.na(mids34[,2]), ]
#     mids43 <- unique(mids43)
#     mids43 <- mids43[ !is.na(mids43[,1]) & !is.na(mids43[,2]), ]
#     
#     slopes34[[x]] <- slope34
#     slopes43[[x]] <- slope43
#     midpoints34[[x]] <- mids34
#     midpoints43[[x]] <- mids43
#     
#     lines(conts34[[x]][[1]][,1], y=conts34[[x]][[1]][,2], col="magenta")
#     lines(conts34[[x]][[2]][,1], y=conts34[[x]][[2]][,2], col="red")
#     # abline(int34top, slope34, col="cyan")
#     # abline(int34bot, slope34, col="cyan")
#     # abline(int43top, slope43, col="yellow")
#     # abline(int43bot, slope43, col="yellow")
#     points(x=mids34[,1], y=mids34[,2], col="green")
#     points(x=mids43[,1], y=mids43[,2], col="turquoise")
#   }
#   
#   
#   #Fix midpoint formula for very diff sizes like the first two quadrants have it???
#   #Use theilsen on midpoints to generate extra list of slopes
#   #Deal with infinities in theilsen
#   #use slopes and contours to vote on parameters
#   
#   
#   Rprof(NULL)
#   #Garbage Collecting
#   #-----------------------------------------------------------------------------------------------------------------------------
#   #}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}} 
#   
#   
#   
#   #Finding Constants for Each Ellipse
#   #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
#   #-----------------------------------------------------------------------------------------------------------------------------
#   #Calculating Parameters for Each Ellipse Passing CNC Threshold Check
#   print(Sys.time() - start)
#   print("Matching Those Contour Pairs That Could Describe an Ellipse... (EllipseParameters.prof)...")
#   Rprof("EllipseParameters.prof")
#   
#   theilsenSlopes <- function(mdpts){
#     middle <- floor(nrow(mdpts)/2)
#     slopes <- matrix(0, ncol=1, nrow=middle)
#     for(x in 1:middle){
#       x1 <- mdpts[x,1]
#       y1 <- mdpts[x,2]
#       x2 <- mdpts[middle+x,1]
#       y2 <- mdpts[middle+x,2]
#       slope <- (y2-y1)/(x2-x1)
#       slopes[x,1] <- slope
#     }
#     median <- median(slopes)
#     return(list(median, slopes))
#   }
#   
#   centers34 <- matrix(0, nrow=length(pairs34), ncol=2)
#   for(x in 1:length(pairs34)){
#     print("x then midpoints34x")
#     print(x)
#     print(midpoints34[[x]])
#     theilSlopes34 <- theilsenSlopes(midpoints34[[x]])
#     theilSlopes43 <- theilsenSlopes(midpoints43[[x]])
#     medSlope34 <- theilSlopes34[[1]]
#     medSlope43 <- theilSlopes43[[1]]
#     theilSlopes34 <- theilSlopes34[[2]]
#     theilSlopes43 <- theilSlopes43[[2]]
#     centers34[x,] <- findCenterFromMidpoints(midpoints34[[x]], midpoints43[[x]], medSlope34, medSlope43)
#     
#     #Using Centers, theilslopes, and interarc slopes to find parameters
#     q1 <- slopes34[[x]]
#     q3 <- slopes43[[x]]
#     q2 <- theilSlopes34
#     q4 <- theilSlopes43
#     gs34 <- findGVals(q1, q2, q3, q4)
#     bs34 <- findBVals(q1, q2, q3, q4)
#     ks34 <- findKVals(gs34, bs34)
#     ns34 <- findNVals(ks34, q1, q2)
#     ps34 <- findPVals(ks34, ns34)
#     # findGVals
#     # findBVals
#     # findKVals
#     # findNVals
#     # findX0Vals
#     # findY0Vals
#     # findAVals
#     # findBVals
#   }
#   findGVals <- function(slope1, slopes2, slope3, slopes4){
#     G <- slope1*slopes2 - slope3*slopes4
#     return(G)
#   }
#   findBVals <- function(slope1, slopes2, slope3, slopes4){
#     B <- (slope3*slopes4 + 1)*(slope1+slopes2) - (slope1*slopes2+1)*(slope3+slopes4)
#     return( B )
#   }
#   findKVals <- function(G, B){
#     K <- (-B + sqrt(B*B+4*G*G))/(2*G)
#     return( K )
#   }
#   findNVals <- function(K, slope1, slopes2){
#     NPlus <- sqrt(  (slope1-K)*(slopes2-K)/(1+slope1*K)/(1+slopes2*K)  )
#     # if(NPlus <= 1){
#     #   return(NPlus)
#     # } else {
#     #   return(1/NPlus)
#     # }
#     NPlus <- NPlus[ !is.na(NPlus) ]
#     NFlipped <- 1/NPlus[ NPlus <= 1 ]
#     NPlus[ NPlus <=1 ] <- 1/ NPlus[ NPlus <= 1 ]
#   }
#   findPVals <- function(K, N){
#     if(N<=1){
#       return(atan(K))
#     } else {
#       return(atan(K)+pi/2)
#     }
#   }
#   
#   accumulateNP <- function(N, P, center, arc1, arc2){
#     
#   }
#   
#   
#   accumulateA1 <- function(N, K, center, arc1, arc2){
#     
#   }
#   
#   findA2Val <- function(A1, N){
#     return(A1*N)
#   }
#   
#   findCenterFromMidpoints <- function(ctPairMidpts1, ctPairMidpts2, theilSlope1, theilSlope2){
#     med1x <- median(ctPairMidpts1[,1])
#     med1y <- median(ctPairMidpts1[,2])
#     med2x <- median(ctPairMidpts2[,1])
#     med2y <- median(ctPairMidpts2[,2])
#     slope1 <- theilSlope1
#     slope2 <- theilSlope2
#     
#     b1 <- med1y - slope1 * med1x
#     b2 <- med2y - slope2 * med2y
#     
#     centerx <- (b1-b2)/(slope2-slope1)
#     centery <- (b2*slope1 - b1*slope2) / (slope1-slope2)
#     
#     return(c(centerx, centery))
#   }
#   
#   centers34 <- matrix(0, ncol=2, nrow=length(pairs34))
#   #needs midpoints34, theil34
#   theilSlopes34 <-  vector(mode = "list", length=length(pairs34))
#   theilSlopes43 <-  vector(mode = "list", length=length(pairs34))
#   
#   for(x in 1:length(pairs34)){
#     medmids34x <- median(midpoints34[[x]][,1])
#     medmids34y <- median(midpoints34[[x]][,2])
#     medmids43x <- median(midpoints43[[x]][,1])
#     medmids43y <- median(midpoints43[[x]][,2])
#     
#     theil34 <- theilsenSlopes(midpoints34[[x]])
#     theil43 <- theilsenSlopes(midpoints43[[x]])
#     # print(theil34)
#     # print(theil43)
#     slope34 <- theil34[[1]]
#     slope43 <- theil43[[1]]
#     theilSlopes34[[x]] <- theil34[[2]]
#     theilSlopes43[[x]] <- theil43[[2]]
#     b34 <- medmids34y - slope34*medmids34x
#     b43 <- medmids43y - slope43*medmids43x
# 
#     abline(b34, slope34, col="yellow")
#     abline(b43, slope43, col="yellow")
#     center34x <- (b34-b43)/(slope43-slope34)
#     center34y <- (b43*slope34-b34*slope43)/(slope34-slope43)
#     centers34[x,] <- c(center34x, center34y)
#   }
#   centers34 <- centers34[ !is.nan(centers34[,1]) & !is.nan(centers34[,2]), ]
#   points(centers34[,1], y=centers34[,2], cex=2, col="yellow")
# 
#   
#   Rprof(NULL)
#   #Garbage Collecting
#   #-----------------------------------------------------------------------------------------------------------------------------
#   #}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}} 
#   
#   
#   
#   #Ellipse Geometry Checking
#   #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
#   #-----------------------------------------------------------------------------------------------------------------------------
#   #Checking to See if Found Ellipses Meet Search Criteria
#   print(Sys.time() - start)
#   print("Parsing Ellipse Constants to Find Only Those We Want... (EllipseParsing.prof)...")
#   Rprof("EllipseParsing.prof")
#   
#   
#   
#   
#   Rprof(NULL)
#   #Garbage Collecting
#   #-----------------------------------------------------------------------------------------------------------------------------
#   #}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}} 
#   
#   
#   
#   #Writing Images to Files
#   #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
#   pathResize = paste0(dir, "/", pathResizeImage, ".png")
#   pathOutline = paste0(dir, "/", pathOutlineImage, ".png")
#   pathHighlight = paste0(dir, "/", pathHighlightImage, ".png")
#   pathContour = paste0(dir, "/", pathContourImage, ".png")
#   
#   #-----------------------------------------------------------------------------------------------------------------------------
#   #Profiling Image Saving
#   print(Sys.time() - start)
#   print("Saving Output Images to File (ImageCreation.prof)...")
#   Rprof("ImageCreation.prof")
#   
#   #Saving Resized Image
#   save.image(imgTest, pathResize)
#   
#   #Saving Black and White Edges
#   imgEdge <- as.cimg(pxEdges)
#   save.image(imgEdge, pathOutline)
#   
#   #Saving Resized Image With Edges Highlighted
#   png(filename=pathHighlight)
#   plot.new()
#   plot(imgTest)
#   highlight(pxEdges, col="red")
#   dev.off()
#   
#   #Plotting and Saving Contours Separated
#   png(filename=pathContour)
#   plot.new()
#   plot(pxEdges)
#   
#   purrr::walk(conts1, function(v) lines(v[,1], v[,2], col="cyan"))
#   purrr::walk(conts2, function(v) lines(v[,1], v[,2], col="magenta"))
#   purrr::walk(conts3, function(v) lines(v[,1], v[,2], col="yellow"))
#   purrr::walk(conts4, function(v) lines(v[,1], v[,2], col="red"))
#   
#   dev.off()
#   
#   Rprof(NULL)
#   
#   #-----------------------------------------------------------------------------------------------------------------------------
#   
#   #Moving Stats Files to Stats Folder
#   profs <- c(list.files()[grepl("*.prof", list.files())])
#   profs2 <- sapply(profs, FUN = function(v) {paste0(pathStats, "/", v)})
#   invisible(file.copy(profs, profs2))
#   unlink(profs)
#   rm(profs, profs2)
#   
#   print(Sys.time() - start)
#   print("Done!")
#   #}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
#   
# # }
# 
# saveCurrentOutputFolder <- function(folder,number){
#   if(file.exists(paste0("EllipseAlgoOutputs/",folder,number))){
#     print("Error: Folder already exists!")
#     break
#   }
#   dir.create(paste0("EllipseAlgoOutputs/",folder,number))
#   file.copy("./outputtemp/", paste0("EllipseAlgoOutputs/",folder,number), recursive = TRUE)
# }
# 
# # runJiaFanEllipses("./Test Pics/Danbooru8-6-20-1235pmEST/test2.jpg", paramLineArea = 0.5)
# # summaryRprof(paste0(pathStats, "/ContourConnectivity.prof"))
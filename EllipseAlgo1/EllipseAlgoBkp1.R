# Converts image file to canny edge detected file
library(imager)
library(dplyr)


#Utility Graphing Functions
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{

graphCircles <- function(img, pxEdges, pxCircles, range=1){
  if(img=="none"){
    plot(pxEdges)
    with(pxCircles[1:range,1:3], circles(x,y,pxCircles[,4], fg="magenta", lwd=2))
  } else{
    plot(img)
    highlight(pxEdges)
    with(pxCircles[1:range,1:3], circles(x,y,pxCircles[,4], fg="magenta", lwd=2))
  }
}

graphAxisPts <- function(mtPairs, col="magenta"){
  pt1 <- matrix(c(mtPairs[,1], mtPairs[,3]), ncol=2)
  pt2 <- matrix(c(mtPairs[,2], mtPairs[,4]), ncol=2)
  lines(x=pt1, y=pt2, col=col)
  return(NULL)
}


#}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}


#Edge Detection- parameters alp, sig
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{

makeCannyEdge <- function(img, alp=.2, sig=5) {
  #alp prefers .1 to 2, .2-.3 needed to grab shading in visual clutter. higher is quicker
  #sig prefers 1.5-20. 10 good in eliminating visual clutter, but 5 retains more shape. Higher is quicker
  img <- grayscale(img, method="XYZ")
  img <- cannyEdges(img, alpha=alp, sigma=sig)
  return(img)
}

#Investigate various greyscales?
#investigate median vs. isoblur


#}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}



#Trial Hough Paired Circle Detection- parameters lower, upper, step, thresh, d, theta
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{

findBestCircles <- function(pxEdged, lower=50, upper=150, step= 10, top=10, topea=2, thresh=.7){
  largestSum <- 0
  topRad <- lower
  pxTop <- data.frame(x=1, y=1, value=0, rad=1)

    for(rads in seq(lower,upper,step)) {
      print(rads)
      print(system.time({
        #Execute Hough
        pxHough <- hough_circle(pxEdged, rads)
        #Convert to Dataframe
        dfHough<-as.data.frame(pxHough)
        #Grab points circles on the canvas only
        dfHough<-dfHough[( (rads<dfHough$x) & (dfHough$x<(max(dfHough$x)-rads)) ) & ( (rads<dfHough$y) & (dfHough$y<(max(dfHough$y)-rads)) ), ] %>% dplyr::arrange(desc(value))
        #Grab topea best circles
        dfHough<-head(dfHough, topea)
        #Keep only those with values above thresh
        dfHough<-dfHough[dfHough[,3]>thresh,]
        #Add radii to dataframe
        dfHough<-cbind(dfHough, rad=rep(rads, length(dfHough[,1])))
        #Add circles to list
        pxTop<-rbind(pxTop, dfHough)
      }))
    }

  
  #investigate median vs. isoblur
  
  return(pxTop %>% dplyr::arrange(desc(value)))
}

#}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}


#Insert Function to parameterize pair cutoff- theta, d

#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{

#}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}


#Trial Modified Hough Test for Ellipses Via Xie-Ji

#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{

findHoughEllipse <- function(pxEdges, sepMin, sepMax){
  dfEdges<-as.matrix(as.data.frame(pxEdges))[,1:2]
  #Find axis A points in range of max and min seperation, returns point pairs and a for each pair
  mtPtsPairsA <- findPtsSep(dfEdges, sepMin, sepMax)
  #Compute Centers
  
  #Compute 
  #Compute allowed axis B points
  
}

# Finds all points in a pxset or df inbetween or equal to two radii from an ellipse center
findPtsSep <- function(pxdfEdges, sepMin, sepMax){
  #reomve line after testing
  dfEdges<-as.matrix(as.data.frame(pxdfEdges))[,1:2]
  
  countEdges<-length(dfEdges[,1])

  # Matrix Check for Pairwise Distances (from center)
  mtTestPtsDist <- matrix(0, nrow=choose(countEdges,2), ncol=5)
  counter=1
  for(i in 1:(countEdges-1)){
    # print(i)
    for(j in (i+1):countEdges){
      ptDist <- sqrt((dfEdges[i,1]-dfEdges[j,1])^2 + (dfEdges[i,2]-dfEdges[j,2])^2)/2
      if((sepMin/2 <= ptDist) & (ptDist <= sepMax/2)){
        mtTestPtsDist[counter,] <- matrix(c(dfEdges[i,], dfEdges[j,], ptDist), nrow=1)
        counter<-counter+1
      }
    }
  }
  mtTestPtsDist <- mtTestPtsDist[ mtTestPtsDist[,1] != 0,]
  return(mtTestPtsDist)
}




####################################################################################################################

# # Recursive Permutation of Pairs of points passed in as an N by M Matrix and returned as a P by 2M+1 Matrix
# # This was a mistake.  It works, but slowly, and is a mistake
# permutePoints <- function(mtPoints, counter, testDistMin, testDistMax){
#   #make 2 last elements last row of output matrix, if appropriate distance
#   if(length(mtPoints[,1]) == 2){
#     dist <- sqrt((mtPoints[1,1]-mtPoints[2,1])^2+(mtPoints[1,2]-mtPoints[2,2])^2)
#     if((testDistMin<dist) & (testDistMax>dist)){
#       mtPermuteOut[length(mtPermuteOut[,1]),] <<-  matrix(c(unlist(mtPoints[1,]), unlist(mtPoints[2,]), dist), nrow=1)
#     }
#     return(NULL)
#   } else {
#     #if not the last two points, find dist between 1st point in current list and all remaining points, and if it's in boundaries, record it in output matrix
#     for(x in 2:length(mtPoints[,1])){
#       print(counter)
#       dist <- sqrt((mtPoints[1,1]-mtPoints[x,1])^2+(mtPoints[1,2]-mtPoints[x,2])^2)
#       if((testDistMin<dist) & (testDistMax>dist)){
#         mtPermuteOut[(counter+x-2),] <<- matrix(c(unlist(mtPoints[1,]), unlist(mtPoints[x,]), dist), nrow=1)
#       }
#       counter<-counter+1
#     }
#   }
#   #Apply permute function to the rest of the points, passing in the new counter, output, and list of points save the one we calculated for
#   return(permutePoints(mtPoints[-1,], counter, testDistMin, testDistMax))
# }
# 
# executePermute <- function(mtPoints, testDistMin, testDistMax){
#   #generates an output matrix of point pairs and distances of the largest possible size to fill
#   mtPermuteOut <- matrix(0, nrow=choose(length(mtPoints[,1]),2), ncol=5)
#   mtPermuteOut <<- mtPermuteOut
#   rm(mtPermuteOut)
#   # print(ls(R_GlobalEnv))
#   #introduces counter for tracking which row of output to fill
#   counter=0
#   #apply recursive point pair finder that mutates mtPermuteOut
#   permutePoints(mtPoints[,1:2], counter, testDistMin, testDistMax)
#   #clean unused values from mtpermute with logical vector
#   zeros<-mtPermuteOut[,1]==0
#   return(mtPermuteOut[!zeros,])
# }
# 
# # Rprof("profileRecursive.out")
# recursed<-executePermute(head(pxTest,100), 25,50); rm(mtPermuteOut)
# # system.time(recursed<-executePermute(head(pxTest), 25,50))
# head(pxTest)
# # Rprof(NULL)
# # summaryRprof("profileRecursive.out")

#}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}



#Resizing Functions to minimize computation- parameterize nx, ny, scale
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{

# imgTest <- resize(imgTest, 500,500)
# imgTest <- imresize(imgTest, scale=0.5)
# imgTest <- crop.borders(imgTest, nx=180, ny=310)

#}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}


#TRIAL RUNS
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{



pathTest <- "./Test Pics/PaintTests/testPaint1.jpg"
imgTest <- load.image(pathTest)
plot(imgTest)

imgTest <- resize(imgTest, 300,300)
# imgTest <- imresize(imgTest, scale=0.1)
imgTest <- crop.borders(imgTest, ny=50, nx=25)

pxEdges <- makeCannyEdge(imgTest, .2, 2)
plot(pxEdges)

system.time(testAxisPts <- findPtsSep(pxEdges, 25, 50))
graphAxisPts(testAxisPts)


# Rprof("profile2.out")
# pxTopCircles <- findBestCircles(pxEdges, lower=50, upper=100, step=5, top=10, topea=2, thresh=0.7)
# Rprof(NULL)

# graphCircles("none", pxEdges, pxTopCircles, range=100)

#}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}


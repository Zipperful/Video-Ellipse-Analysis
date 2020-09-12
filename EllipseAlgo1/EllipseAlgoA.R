#TODO

#Optimizing:  Change matrix passing
#  findAxisAVals needs to search non-edge points, and be vectorized

#   Normalize intro pic
#   Fan Lou Song Curve finding, esp background normalizing
#   Optimizing out uneeded splitting and copying of Xie-Ji then clean


library(imager)
library(dplyr)
library(OpenImageR)
library(VideoTrackingUtilities)
#remove after testing
library(microbenchmark)
library(profvis)


#Trial Modified Hough Test for Ellipses Via Xie-Ji

#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{



findHoughEllipses <- function(pxEdges, aMin, aMax, e, imgX, imgY){
  #Cast pixset as matrix of points
  mtEdges<-as.matrix(as.data.frame(pxEdges))[,1:2]
  
  mtEdges <- mtPxSample(pxEdges, 2)[,1:2]
  
  #Find smallest and largest B semiaxis lengths from eccentricity, and range of them to calc ellipses for
  axisMin <- bFind(aMin,e)
  axisMax <- bFind(aMax, 2-e)
  #creates bvalues from axes, short one pixel 
  bTest <- ceiling(axisMin):(floor(axisMax)-1)
  
  #Find axis A points in range of max and min seperation, returns point pairs and a for each pair
  #Format: x1, y1, x2, y2, A, x0, y0, theta, best b, b vote, max possible bvote
  mtAxisVals <- findAxisAVals(mtEdges, axisMin, axisMax, imgX, imgY)

  
  # mtAxisVals <- listAxes[[2]]
  # mtPoints <- listAxes[[1]]
  #turn pixset image into logical array
  lgEdges <- as.logical(pxEdges)
  
  #Make iterator of number of "layers" for each pair of points to consider
  #w/1 layer for each ellipse for b value - the semiminor axis length- with each 
  layers <- length(bTest)
  
  #matrix storing best b value for each pair of points with its vote number and max possible vote number for that ellipse
  bs <- matrix(0, nrow=nrow(mtAxisVals), ncol=3)
  
  #find ellipses for each possible b value for each combo of a axes, convert to a logical array to find overlap with image, and store overlap pixel count
  #as votes in a b matrix

  for(pair in 1:nrow(mtAxisVals)){
    bVotes <- rep(0, layers)
    bVoteMax <- rep(1,layers)
    for(b in 1:length(bTest)){
      #Finds ellipse points for the values for each pair of points from mtAxisAVals and possible b values and turns each in to a logical array of points
      # arAccumulator[b, , ,1,1] <- do.call(findEllipsePoints, as.list(c(b, mtAxisAVals[pair,5:8], imgX, imgY))) %>% makeMtPxLogi(imgX,imgY)
      #Multiplies each logical array of ellipse points by the edge points logical array to find overlap and sums them to find votes for each ellipse
      # creates an array
      # bVotes[counter,] <- lapply(1:layers, function(x){sum(arAccumulator[x,,1,1]*lgEdges)})
      # bVote <- sum(arAccumulator[b,,1,1]*lgEdges)
      #Stores the b value, vote count, and max number of possible votes(pixels in the perfectly matched ellipse) in mtAxisAVals
      mtEllipse <- findEllipsePoints(bTest[b], mtAxisVals[pair,])
      mtEllipse<- mtEllipse[ 0<mtEllipse[,1] & mtEllipse[,1] < imgX & 0<mtEllipse[,2] & mtEllipse[,2]<imgY,  ]

      if(!(is.null(dim(mtEllipse))) ){
        if(nrow(mtEllipse)!=0){
          bVoteMax[b] <- nrow(mtEllipse)
          lgEllipse <- makeMtPxLogi(mtEllipse, imgX, imgY)
          lgOverlap <- lgEllipse*lgEdges
          bVotes[b] <- sum(lgOverlap)
          
        }
      } else {
        bVotes[b] <- -1
        bVoteMax[b] <- -1
      }
    }
    indexBest <- which.max(bVotes)
    bs[pair, ] <- c(bTest[indexBest], bVotes[indexBest], bVoteMax[indexBest])
  }
  
  return(list(mtAxisVals, bs))
}


#Return matrix of a (distance between pts), x0, y0, and theta values between all possible pairs of a set of points
#As these will be used as a major or minor axis of an ellipse on the image, only those in acceptable ranges for our ellipses will be used
findAxisAVals <- function(dfEdges, aMin, aMax, imgX, imgY){
  #Find all combinations of points in dfEdges as pairs, and make a list for each set of points to combine
  indices <- t(combn( x= nrow(x=dfEdges), m = 2))
  pts1 <- matrix(0, ncol=2, nrow=nrow(indices))
  pts2 <- matrix(0, ncol=2, nrow=nrow(indices))
  for(x in 1:nrow(indices)){
    i <- indices[x,1]; j<-indices[x,2]
    pts1[x,] <- dfEdges[i,]
    pts2[x,] <- dfEdges[j,]
  }

  #Calculate difference of sets of points, and use these to find distances between points
  diffs <- pts2-pts1
  dists <- sqrt(rowSums(diffs^2))

  #only look at points an appropriate distance apart from nowon
  goodDists <- aMin <= dists & dists <= aMax
  pts1<-pts1[ goodDists ]; pts2 <- pts2 [ goodDists ]; dists <- dists[ goodDists ]; diffs<-diffs[ goodDists, ]

  #Store distances in a matrix of values about this distance, which is taken to be a major or minor axis of an ellipse.
  #First column is A value, columns 2 and 3 are the midpoints, giving centers of ellipse as x0 and y0
  #Fourth column is angle betweent them, theta
  mtAOut <- matrix(0, ncol=4, nrow=length(dists), byrow=TRUE)
  mtAOut[,1] <- dists
  mtAOut[,2:3] <- (pts1+pts2)/2
  mtAOut[,4] <- atan2(y=diffs[,2], diffs[,1])

  #Rule out points so close to edge that all ellipses drawn around center would be off of the image
  lgCloseToEdge <- mtAOut[,2]+aMin > imgX | mtAOut[,2]-aMin < 0 | mtAOut[,3]+aMin > imgY | mtAOut[,3]-aMin <0
  mtAout<- mtAOut[ !lgCloseToEdge, ]

  #Return matrix of a, x0, y0, and theta values for all possible pairs of points in acceptable ranges
  return(mtAOut)
}

#}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}



#Resizing Functions to minimize computation- parameterize nx, ny, scale
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{

# imgTest <- resize(imgTest, 500,500)
# imgTest <- imresize(imgTest, scale=0.5)
# imgTest <- crop.borders(imgTest, nx=180, ny=310)

#}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}


#TRIAL RUNS
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{



pathTest <- "./Test Pics/PaintTests/testPaint4.jpg"
imgTest <- load.image(pathTest)
plot(imgTest)

# imgTest <- resize(imgTest, 100,100)
# imgTest <- imresize(imgTest, scale=0.1)
# imgTest <- crop.borders(imgTest, ny=80, nx=50)

pxEdges <- makeCannyEdge(imgTest, .3, 1)
plot(pxEdges)
pxEdges

set.seed(6969)
# Rprof("smalltest1.out")
# testAxisPts <- findHoughEllipses(pxEdges, aMin=3, aMax=10, e=.8, imgX=20, imgY=20)
# write.csv(testAxisPts, file="out.csv")
# Rprof(NULL)
# summaryRprof("smalltest1.out")

profvis(findHoughEllipses(pxEdges, aMin=3, aMax=10, e=.8, imgX=20, imgY=20))





# graphAxisPts(testAxisPts, 10000)

#}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}


#TODO
#   Normalize intro pic
#   Fan Lou Song Curve finding, esp background normalizing
#   Optimizing out uneeded splitting and copying of Xie-Ji then clean





library(imager)
library(dplyr)
#remove after testing
library(microbenchmark)
library(profvis)

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

graphAxisPts <- function(mtPairs, num=nrow(mtPairs), col="magenta"){
  pt1 <- matrix(c(mtPairs[(1:num),1], mtPairs[(1:num),3]), ncol=2)
  pt2 <- matrix(c(mtPairs[(1:num),2], mtPairs[(1:num),4]), ncol=2)
  lines(x=pt1, y=pt2, col=col)
  return(NULL)
}

graphEllipse <- function(b, a, x0, y0, theta, imgX, imgY){
  #obtains list of angles between (0-2pi] with as many elements as perimeter for maximum angular resolution
  phis <- seq(0, 2*pi, by=(2*pi)/(2*(imgX+imgY)))[-1]
  #obtains viable x and y values from parametric equations of a rotated ellipse, floor
  xs <- floor(a*cos(phis)*cos(theta)-b*sin(phis)*sin(theta)+x0)
  ys <- floor(a*cos(phis)*sin(theta)+b*sin(phis)*cos(theta)+y0)
  #only returns non duplicate point pairs as rounding will sort many viable values in to the same points
  mtPoints <- matrix(c(xs, ys), ncol=2)
  mtPoints <- mtPoints[ !duplicated(mtPoints), ]
  print(nrow(mtPoints))
  pxPoints<-makeMtPxset(mtPoints, imgX,imgY)
  lines(mtPoints[,1], mtPoints[,2], col="magenta", lwd=10, lty=1)
}

#SUPER temporary fn, delete upon reading
makeOutputNeat <- function(outputRow) {
  b <- outputRow[9]
  a <- outputRow[5]
  x0 <- outputRow[6]
  y0 <- outputRow[7]
  theta <- outputRow[8]
  return(c(b,a,x0,y0,theta))
}

makeMtPxset <- function(mtPairs, imgX, imgY){
  pxSet <- array(FALSE, dim=c(imgX,imgY,1,1))
  mtPairs<- mtPairs[ 0<mtPairs[,1] & mtPairs[,1] < imgX & 0<mtPairs[,2] & mtPairs[,2]<imgY,  ]
  for(x in 1:nrow(mtPairs)){ pxSet[ mtPairs[x,1], mtPairs[x,2], 1, 1] <- TRUE}
  return(as.pixset(pxSet))
}

makeMtPxLogi <- function(mtPairs, imgX, imgY){
  lgPixels <- array(FALSE, dim=c(imgX,imgY,1,1))
  # print(max(mtPairs[,1]))
  # print(max(mtPairs[,2]))
  mtPairs<- mtPairs[ 0<mtPairs[,1] & mtPairs[,1] < imgX & 0<mtPairs[,2] & mtPairs[,2]<imgY,  ]
  for(x in 1:nrow(mtPairs)){ lgPixels[ mtPairs[x,1], mtPairs[x,2], 1, 1] <- TRUE}
  return(lgPixels)
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
        dfHough<-cbind(dfHough, rad=rep(rads, nrow(dfHough)))
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



findHoughEllipses <- function(pxEdges, aMin, aMax, e, imgX, imgY){
  #Cast pixset as matrix of points
  # mtEdges<-as.matrix(as.data.frame(pxEdges))[,1:2]
  
  mtEdges <- mtPxSample(pxEdges, 2)[,1:2]
  
  #Find smallest and largest B semiaxis lengths from eccentricity, and range of them to calc ellipses for
  axisMin <- bFind(aMin,e)
  axisMax <- bFind(aMax, 2-e)
  #creates bvalues from axes, short one pixel 
  bTest <- ceiling(axisMin):(floor(axisMax)-1)
  
  #Find axis A points in range of max and min seperation, returns point pairs and a for each pair
  #Format: x1, y1, x2, y2, A, x0, y0, theta, best b, b vote, max possible bvote
  mtAxisVals <- findAxisAVals(mtEdges, axisMin, axisMax, imgX, imgY)
  
  #turn pixset image into logical array
  lgEdges <- as.logical(pxEdges)
  
  #Make accumulator w/1 layer for each ellipse
  layers <- length(bTest)
  
  # arAccumulator <- array(FALSE, dim = c(layers, imgX, imgY, 1, 1))
  
  bVotes<- matrix(0, nrow=nrow(mtAxisVals), ncol=3)
  
  #fill accumulator with valid ellipses in logical form
  ellipseXErrors<-0
  ellipseYErrors<-0
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
      #Stores the b value, vote count, and max number of possible votes(pixels in the perfect ellipse) in mtAxisAVals
      # print(dim(mtAxisVals))
      mtEllipse <- do.call(findEllipsePoints, as.list(c(bTest[b], mtAxisVals[pair,5:8])))
      bVoteMax[b] <- nrow(mtEllipse)
      if(max(mtEllipse[,1])>imgX ){ellipseXErrors<- ellipseXErrors+1}
      if(max(mtEllipse[,2])>imgY ){ellipseYErrors<- ellipseXErrors+1}

      # print(dim(mtEllipse))
      # print(head(mtEllipse))
      # print("In to function")
      lgEllipse <- makeMtPxLogi(mtEllipse, imgX, imgY)
      lgOverlap <- lgEllipse*lgEdges
      bVotes[b] <- sum(lgOverlap)
    }
    # print("pair")
    # print(pair)
    # print("max")
    # print(nrow(mtAxisVals))
    # print("Errors")
    # print(ellipseYErrors+ellipseXErrors)
    indexBest <- which.max(bVotes)

    mtAxisVals[pair, 9:11] <- c(bTest[indexBest], bVotes[indexBest], bVoteMax[indexBest])
  }

  
  
  
  
  #Compute Points Close enough to centers
  return(mtAxisVals)
}

# Finds all points in a pxset or df inbetween or equal to two radii from an ellipse center
findAxisAVals <- function(dfEdges, aMin, aMax, imgX, imgY){
  #Ignore edge points too close to edge
  # lgTooClose <- dfEdges[,1]<=aMin | dfEdges[,1]>=imgX-aMin | dfEdges[,2]<=aMin | dfEdges[,2]>=imgY-aMin
  # dfEdges<-dfEdges[!lgTooClose,]
  countEdges<-nrow(dfEdges)

  # Matrix Check for Pairwise Distances (from center)
  # Find a, x0, y0, theta, and returns three blank cols to be filled with b, bvote values, and max possible bvote
  mtAOut <- matrix(0, nrow=choose(countEdges,2), ncol=11)
  counter=1
  for(i in 1:(countEdges-1)){
    for(j in (i+1):countEdges){
      dy <- dfEdges[i,2]-dfEdges[j,2]
      dx <- dfEdges[i,1]-dfEdges[j,1]
      a <- sqrt(dx^2 + dy^2)/2
      if((aMin <= a) & (a <= aMax)){
        theta<-atan2(dy,dx)
        x0 <- sum(dfEdges[i,1],dfEdges[j,1])/2
        y0 <- sum(dfEdges[i,2],dfEdges[j,2])/2
        mtAOut[counter,] <- matrix(c(dfEdges[i,], dfEdges[j,], a, x0, y0, theta, 0, 0, 0), nrow=1)
        counter<-counter+1
      }
    }
  }
  mtAOut <- mtAOut[ mtAOut[,1] != 0,]
  return(mtAOut)
}


#Find b given a and e
bFind<- function(a,e){
  bSq<-a*a*(1-e)
  if(bSq>0){
    b<-sqrt(bSq)
  } else{
    b<-sqrt(a*a)/(e-1)
  }
  return(b)
}



#Find valid ellipse points given a,b,x0,y0, and theta, with resolution taken from image size
findEllipsePoints <- function(b, a, x0, y0, theta){
  #chooses a set of phi values around center to pick, of similar number to the amount a circle the size of the larger of the A or B values would make
  #max pixels to describe a circumfrence is 2pi*r, min is sqrt(2)*pi*r, we err in the max direction
  #Exclude first pixel as it and last are repeats
  phiCount <- ceiling(max(a,b)*2*pi)
  phis <- seq(0, 2*pi, by=(2*pi)/phiCount)[-1]
  #obtains viable x and y values from parametric equations of a rotated ellipse, floor
  xs <- floor(a*cos(phis)*cos(theta)-b*sin(phis)*sin(theta)+x0)
  ys <- floor(a*cos(phis)*sin(theta)+b*sin(phis)*cos(theta)+y0)
  #only returns non duplicate point pairs as rounding will sort many viable values in to the same points
  points <- matrix(c(xs, ys), ncol=2)
  return(points[ !duplicated(points), ])
}



#Samples a pixset to reduce in size by a factor of Factor
mtPxSample <- function(pxSet, factor){
  mtPxSet <- as.matrix(as.data.frame(pxSet))
  countPoints <- nrow(mtPxSet)
  samps <- sample(countPoints, size=round(countPoints/factor))
  return(mtPxSet[ samps,])
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



pathTest <- "./Test Pics/PaintTests/testPaint4.jpg"
imgTest <- load.image(pathTest)
plot(imgTest)

# imgTest <- resize(imgTest, 300,300)
# imgTest <- imresize(imgTest, scale=0.1)
# imgTest <- crop.borders(imgTest, ny=80, nx=50)

pxEdges <- makeCannyEdge(imgTest, .3, 1)
plot(pxEdges)
pxEdges

set.seed(6969)
# Rprof("smalltest0.out")

# testAxisPts <- findHoughEllipses(pxEdges, aMin=3, aMax=10, e=.8, imgX=20, imgY=20)
# write.csv(testAxisPts, file="out.csv")
# Rprof(NULL)
# summaryRprof("smalltest0.out")

profvis(findHoughEllipses(pxEdges, aMin=3, aMax=10, e=.8, imgX=20, imgY=20))






# graphAxisPts(testAxisPts, 10000)


# Rprof("profile2.out")
# pxTopCircles <- findBestCircles(pxEdges, lower=50, upper=100, step=5, top=10, topea=2, thresh=0.7)
# Rprof(NULL)

# graphCircles("none", pxEdges, pxTopCircles, range=100)
# summaryRprof("profileRecursive.out")
#}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}


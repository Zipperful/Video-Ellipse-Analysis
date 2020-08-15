#TODO

#Optimizing:  Change matrix passing
#  findAxisAVals needs to search non-edge points, and be vectorized

#   Normalize intro pic
#   Fan Lou Song Curve finding, esp background normalizing
#   Optimizing out uneeded splitting and copying of Xie-Ji then clean


library(imager)
library(dplyr)
library(OpenImageR)
library(SuperpixelImageSegmentation)

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


makeMtPxset <- function(mtPairs, imgX, imgY){
  pxSet <- array(FALSE, dim=c(imgX,imgY,1,1))
  mtPairs<- mtPairs[ 0<mtPairs[,1] & mtPairs[,1] < imgX & 0<mtPairs[,2] & mtPairs[,2]<imgY,  ]
  for(x in 1:nrow(mtPairs)){ pxSet[ mtPairs[x,1], mtPairs[x,2], 1, 1] <- TRUE}
  return(as.pixset(pxSet))
}

makeLogiPxset <- function(mtLogi){
  if(length(dim(mtLogi))==2){
    mtLogi <- array(mtLogi, dim=c(dim(mtLogi), 1, 1))
  }
  return(as.pixset(mtLogi))
}

makeMt3Cimg <- function(mtLogi){
  mtLogi <- array(mtLogi, dim=c(dim(mtLogi), 1))
  mtLogi <- aperm(mtLogi, perm=c(2,1,4,3))
  print(dim(mtLogi))
  return(as.cimg(mtLogi))
}


makeMtPxLogi <- function(mtPairs, imgX, imgY){
  lgPixels <- array(FALSE, dim=c(imgX,imgY,1,1))
  #line below not used in code, commented out.  Add back in to use function generally
  # mtPairs<- mtPairs[ 0<mtPairs[,1] & mtPairs[,1] < imgX & 0<mtPairs[,2] & mtPairs[,2]<imgY,  ]
  for(x in 1:nrow(mtPairs)){ lgPixels[ mtPairs[x,1], mtPairs[x,2], 1, 1] <- TRUE}
  return(lgPixels)
}

makePxsetMt <- function(pxSet) {
  as.matrix(as.data.frame(pxSet))[,1:2]
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

#Samples a pixset to reduce in size by a factor of Factor
mtPxSample <- function(pxSet, factor){
  mtPxSet <- as.matrix(as.data.frame(pxSet))
  countPoints <- nrow(mtPxSet)
  samps <- sample(countPoints, size=round(countPoints/factor))
  return(mtPxSet[ samps,])
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

#}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}



#Resizing Functions to minimize computation- parameterize nx, ny, scale
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{

# imgTest <- resize(imgTest, 500,500)
# imgTest <- imresize(imgTest, scale=0.5)
# imgTest <- crop.borders(imgTest, nx=180, ny=310)

#}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}



#Processing images into background and foreground with superpixels
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{

# imgTest <- resize(imgTest, 500,500)
# imgTest <- imresize(imgTest, scale=0.5)
# imgTest <- crop.borders(imgTest, nx=180, ny=310)

#}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}



#TRIAL RUNS
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
# Rprof("./EllipseAlgo2/superpixel1.out")
# set.seed(6969)

pathTest <- "./Test Pics/FoodWarTests/VLCSnaps/vlcsnap-2020-08-15-17h06m02s553.png"

# # Imager loading
# imgTest <- load.image(pathTest)
# imgTest <- imresize(imgTest, scale=0.25)
# plot(imgTest)

#OpenImageR loading
imgLoad <- readImage(pathTest)
imgLoad<-imgLoad*255
str(imgLoad)

#OpenImageR resizing
imgY <- length(imgLoad[1,,1])
imgX <- length(imgLoad[,1,1])
imgLoad <- resizeImage(imgLoad, floor(imgX/2), floor(imgY/2))

#OpenImageR superpixels plotted- 180 looks good
superIm <- superpixels(imgLoad, method="slic", superpixel=300, compactness = 20,  return_slic_data = TRUE, return_labels = TRUE)
plot_slic = OpenImageR::NormalizeObject(superIm$slic_data)
plot_slic = grDevices::as.raster(plot_slic)
graphics::plot(plot_slic)

#Superpixel to Segmentation with SuperpixelImageSegmentation
init = Image_Segmentation$new()
spx = init$spixel_segmentation(input_image = imgLoad,
                               superpixel = 300,
                               AP_data = TRUE,
                               use_median = TRUE,
                               sim_wL = 3,
                               sim_wA = 10,
                               sim_wB = 10,
                               sim_color_radius = 20,
                               verbose = TRUE)
imageShow(spx$AP_image_data)

# summaryRprof("./EllipseAlgo2/superpixel1.out")
#}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}


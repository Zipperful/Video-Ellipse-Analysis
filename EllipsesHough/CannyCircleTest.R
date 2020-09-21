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
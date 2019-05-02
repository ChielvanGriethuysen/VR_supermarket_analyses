skippoints<-function(data, distance){
  curent.dist<-0
  filltered.points<-c()
  
  for(j in 1:nrow(data)){
    curent.dist<- curent.dist+data$dist[j]
    if(curent.dist>distance){
      curent.dist<-0
      filltered.points<-c(filltered.points,j-1)
    }
  }
  data[filltered.points,1:4]
  
}

subdistance<-function(FootPosition, start, stop){
  curent.dist<-0
  x.change <- diff(FootPosition$x, 1)
  y.change <- diff(FootPosition$z, 1)
  distance.between.points<-sqrt(x.change^2 + y.change^2)
  
  sum(distance.between.points[start:stop])
  
}

speedfeature<- function(input.data, break.time, radius, skippoints=c()){
  FootPosition<-input.data[,2:4]
  time<-input.data[,1]
  
  if(length(skippoints)==0){
    skippoints<-rep(FALSE,nrow(input.data))
  }
  
  final.step <- first(which(time > max(time) - break.time)) - 1
  
  stop <- data.frame(begin = numeric(length(time)), end = numeric(length(time)))
  
  #calculate distance between two points for every point
  radius.squared<-radius^2
  
  distance.between.points<-input.data$dist
  
  k <- dis <- final.step.in.stop <- 0
  for(s in 1 : final.step){
    if(s %% 1000 == 0){
      print(paste0('Calculating STOP of time point ', s, ' of ', final.step, 
                   ' of file ', i))
    }
    
    if(final.step.in.stop > s || skippoints[s]){ #Don't check points that are already part of previous stop
      next
    }
    
    dis <- 0
    k <- s
    #skip points that can't be on the border of the stop.radius, skips distance calculations for uninteresting points
    helpDis<-0
    while (helpDis<radius&& !skippoints[k] &&k < length(time)) {
      helpDis<-helpDis+distance.between.points[k]
      k<-k+1
    }
    dis <- (FootPosition$x[s] - FootPosition$x[k]) ^ 2 +
      (FootPosition$z[s] - FootPosition$z[k]) ^ 2
    
    #look at the points that are byond stop.radius walking distance
    while(dis < radius.squared && !skippoints[k] &&k < length(time)){
      k <- k + 1
      dis <- (FootPosition$x[s] - FootPosition$x[k]) ^ 2 +
        (FootPosition$z[s] - FootPosition$z[k]) ^ 2
    }
    # if point on distance stop.radius and time more than stop/slow time than it is a stop
    if(time[k - 1] - time[s] > break.time){
      #stop[s : (k - 1), 1] <- TRUE
      stop[s, 1] <- s
      stop[s, 2] <- final.step.in.stop <- k - 1
    }
  }
  stop[stop[,1]!=0,]
}
#give indexed liste for each datapoint if it is part of a stop
expandstops<-function(data,n.datapoints){
  stop<-rep(FALSE,n.datapoints)
  for (i in 1:nrow(data)) {
    stop[data$begin[i] : data$end[i]] <- TRUE
  }
  stop
}
# check if stop is in front of a product box
Checkstopbeforeitem <- function(position, productsbox){
  position[1] > productsbox$xmin & position[1] < productsbox$xmax &
    -position[2] > productsbox$zmin & -position[2] < productsbox$zmax
}
# add time info to stop/slow
add.times<- function(points, input.data){
  points$start.time<-input.data$time[points$begin]
  points$stop.time<- input.data$time[points$end]
  points$time.spend<-points$stop.time- points$start.time
  return(points)
}


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
skippoints<-function(FootPositions, Time, distance){
  curent.dist<-0
  x.change <- diff(res$FootPosition$x, 1)
  y.change <- diff(res$FootPosition$z, 1)
  distance.between.points<-sqrt(x.change^2 + y.change^2)
  filltered.points<-c()
  
  for(j in 1:length(distance.between.points)){
    curent.dist<- curent.dist+distance.between.points[j]
    if(curent.dist>distance){
      curent.dist<-0
      filltered.points<-c(filltered.points,j-1)
    }
  }
  
  f<-res$FootPosition[filltered.points,] 
  t<-res$time[filltered.points]
  data.frame(f,t)
  
}
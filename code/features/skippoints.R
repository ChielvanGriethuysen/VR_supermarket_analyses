skippoints<-function(FootPosition, Time, distance){
  curent.dist<-0
  x.change <- diff(FootPosition$x, 1)
  y.change <- diff(FootPosition$z, 1)
  distance.between.points<-sqrt(x.change^2 + y.change^2)
  filltered.points<-c()
  
  for(j in 1:length(distance.between.points)){
    curent.dist<- curent.dist+distance.between.points[j]
    if(curent.dist>distance){
      curent.dist<-0
      filltered.points<-c(filltered.points,j-1)
    }
  }
  
  f<-FootPosition[filltered.points,] 
  t<-Time[filltered.points]
  data.frame(f,t)
  
}

subdistance<-function(FootPosition, start, stop){
  curent.dist<-0
  x.change <- diff(FootPosition$x, 1)
  y.change <- diff(FootPosition$z, 1)
  distance.between.points<-sqrt(x.change^2 + y.change^2)
  
  sum(distance.between.points[start:stop])
  
}
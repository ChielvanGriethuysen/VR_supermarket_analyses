# Last edited 2019-11-11 by Chiel van Griethuijsen (m.a.vangriethuijsen@students.uu.nl)


# finds out the longest cerie of zerro's in the data, to detect the lagest gap in the data  
missing.data.length<- function(data){
  n<-start<- stop<- res.stop<-i<-0
  length<- time<-res.start<- c()
  while (i<=nrow(data)) {
    i<-i+1
    if(i<= nrow(data) &&is.na(data$x[i])){
      start<- i
      while (i<= nrow(data) &&is.na(data$x[i])) {
        i<-i+1
      }
      stop<- i
      length<- c(length, stop-start)
      time<- c(time, if(start== stop) 0 else round(data$time[stop-1]-data$time[start],3))
      res.start<- c(res.start, start)
      if(i >= nrow(data)){
        break
      }
    }
  }
  return(data.frame(mean= if(is.null(length))0 else mean(length), 
                    max=  if(is.null(length))0 else max(length),
                    n= if(is.null(length))0 else length(length),
                    missing= sum(is.na(data$x))/nrow(data)))
}

#for each point finds what the first point is afther a sertain distance
find.first.point.on.dist<- function(data, dist){
  d<- 0
  data$point.on.dist<-0
  j<-0
  for (i in 1: nrow(data)) {
    
    while (d<dist && j< nrow(data)) {
      j<-j+1
      d<-d+data$dist[j]
    }
    if (j>= nrow(data))
      break
    data$point.on.dist[i]<- j
    d<- d-data$dist[i]
  }
  data$point.on.dist[i:j]<-j
  return(data[data$point.on.dist,])
}
##calculate the angles between twoo datasets op points and the side of the map 
calculate.direction<- function(data.o,data.t){
  data.o$angles<-0
  for (i in 1:nrow(data.o)){
    A<- c(data.t$x[i], data.t$z[i])
    B<- c(data.o$x[i], data.o$z[i])
    C<- c(data.o$x[i], 0)
    if(any(is.na(A)))
      data.o$angles[i]<- NA
    else
      if(A[1]>B[1]){
        data.o$angles[i]<- Angle(A,B,C)
      }else{
        data.o$angles[i]<- 360-Angle(A,B,C)
      }
    
  }
  return(data.o$angles)
}
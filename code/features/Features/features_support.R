# Last edited 2019-11-11 by Chiel van Griethuijsen (m.a.vangriethuijsen@students.uu.nl)

#give a subset of the data where the points are a certain distance apart
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
speed.dist.add<-function(input.data){
  x.change <- diff(input.data$x, 1)
  y.change <- diff(input.data$z, 1)
  distance.between.points<-sqrt(x.change^2 + y.change^2)
  speed<-c(0,distance.between.points/diff(input.data$t,1))
  input.data<- data.frame(input.data, speed,dist=c(mean(distance.between.points),distance.between.points))
  return(input.data)
}



#add the label to eache datapoint based on a log file
datapoint.add.label<-function(input.data, log){
  
  label<-rep("none",nrow(input.data))
  
  for (i in 1:nrow(log)) {
    label[log$start[i]:log$stop[i]]<-log$label[i]
  }
  data.frame(input.data,label)
  
}



#returns a subset of the data, which is part of the log subsets
log.subset<- function(data, log, rev= FALSE){
  subset<-rep(FALSE, nrow(data))
  if(nrow(log)>0){
    for (i in 1:nrow(log)) {
      subset[log$start[i]:log$stop[i]]<-TRUE
    }
  }
  
  if(rev){
    data[!subset,]
  }
  else{
    data[subset,]
  }
  
}



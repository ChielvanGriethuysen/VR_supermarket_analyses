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
# calculate distance covert between two data points
subdistance<-function(FootPosition, start, stop){
  curent.dist<-0
  x.change <- diff(FootPosition$x, 1)
  y.change <- diff(FootPosition$z, 1)
  distance.between.points<-sqrt(x.change^2 + y.change^2)
  
  sum(distance.between.points[start:stop])
  
}
#old stop function
speedfeature<- function(input.data, break.time, radius, skippoints=c()){
  FootPosition<-input.data[,2:4]
  time<-input.data[,1]
  
  if(length(skippoints)==0){
    skippoints<-rep(FALSE,nrow(input.data))
  }
  
  final.step <- first(which(time > max(time) - break.time)) - 1
  
  stop <- data.frame(start = numeric(length(time)), stop = numeric(length(time)))
  
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
  if(nrow(data)>0){
    for (i in 1:nrow(data)) {
      stop[data$start[i] : data$stop[i]] <- TRUE
    }
  }
  stop
}
# check if stop is in front of a product box
Checkstopbeforeitem <- function(position, productsbox){
  position[1] > productsbox$xmin & position[1] < productsbox$xmax &
    -position[2] > productsbox$zmin & -position[2] < productsbox$zmax
}
# add time info to stop/slow
add.times.location<- function(points, input.data){
  points$start.time<-input.data$time[points$start]
  points$stop.time<- input.data$time[points$stop]
  points$time.spend<-points$stop.time- points$start.time
  points$x.start<-input.data$x[points$start]
  points$z.start<-input.data$z[points$start]
  points$x.stop<-input.data$x[points$stop]
  points$z.stop<-input.data$z[points$stop]
  points$absolute.dist<- 0
  points$relative.dist<-sqrt((points$x.start-points$x.stop)^2+(points$z.start-points$z.stop)^2)

  
  for(i in 1:nrow(points)){
    points$absolute.dist[i]<- sum(input.data$dist[points$start[i]:points$stop[i]])
  }
  points$absolute.speed<-points$absolute.dist/points$time.spend
  points$relative.speed<-points$relative.dist/points$time.spend
  return(points)
}
# make parts thate are above or below a certain speed, merge these parts if they are close to each other to deal with data errors and small movements
calc.speed.discretisation<-function(input.data, cuttoff, merge.dist, lowerinequations=TRUE){
  candidates <-output<- data.frame(start = numeric(), stop = numeric())
  j<-1
  dist<-0
  i<-1
  #find all parts below or above certain speed with minimum duration
  if(lowerinequations){
    while(i < nrow(input.data)){
      if(input.data$speed[i]<cuttoff){
        t<-0
        j<-i
        while (input.data$speed[j]<cuttoff && j< nrow(input.data)) {
          j<-j+1
        }
        if(input.data$time[j]-input.data$time[i]>0.05){
          candidates<-rbind(candidates,data.frame(start= i,stop=j))
          i<-j
        }
      }
      i<-i+1
    }
  } else{
    while(i < nrow(input.data)){
      if(input.data$speed[i]>cuttoff){
        t<-0
        j<-i
        while (input.data$speed[j]>cuttoff && j<nrow(input.data)) {
          j<-j+1
        }
        if(input.data$time[j]-input.data$time[i]>0.05){
          candidates<-rbind(candidates,data.frame(start= i,stop=j))
          i<-j
        }
      }
      i<-i+1
    }
  }
  #if parts are to short afther each other then combine them
  k<-1
  while (k<nrow(candidates)) {
    l<-k
    while (l<nrow(candidates) && 
           input.data$time[candidates$start[l+1]]-input.data$time[candidates$stop[l]]<merge.dist) {
      l<-l+1
    }
    output<-rbind(output,data.frame(start= candidates$start[k],stop=candidates$stop[l]))
    k<-l+1
  }
  # add last element if not mearged with previous one
  if(last(candidates$stop)!=last(output$stop))
  {
    output<-rbind(output,data.frame(start= candidates$start[k],stop=candidates$stop[k]))
  }
  output
}
#add the label to eache datapoint based on a log file
datapoint.add.label<-function(input.data, log){
  
  label<-rep("none",nrow(input.data))
  
  for (i in 1:nrow(log)) {
    label[log$start[i]:log$stop[i]]<-log$label[i]
  }
  data.frame(input.data,label)
  
}

#calculate box around products, used to determine if person is doing somting close to the product
calc.productbox<- function(products){
  data.frame(xmin = rep(NA, nrow(products)),
             xmax = rep(NA, nrow(products)),
             zmin = rep(NA, nrow(products)),
             zmax = rep(NA, nrow(products)),
             up.down.side= products$up.down.side,
             announced = products$announced,
             productnumber =  products$productnumber,
             x = products$x,
             z = products$z,
             colour = products$colour) %>%
    mutate(xmin = ifelse(up.down.side == "up", products$x-products$height,
                         ifelse(up.down.side =="down", products$x,
                                products$x-.5*products$height))) %>% 
    mutate(xmax = ifelse(up.down.side == "up", products$x,
                         ifelse(up.down.side == "down",products$x+products$height,
                                products$x+.5*products$height)))%>%
    mutate(zmin = ifelse(up.down.side == "sideleft", products$z-products$width, 
                         ifelse(up.down.side == "sideright",  products$z,
                                products$z-.5*products$width))) %>%
    mutate(zmax = ifelse(up.down.side == "sideleft", products$z, 
                         ifelse(up.down.side == "sideright",  products$z+products$width,
                                products$z+.5*products$width)))
}
# check if a position is in one of the boxes of a box list
box.check<-function(position, box.list){
  position[1] > box.list$xmin & position[1] < box.list$xmax &
    -position[2] > box.list$zmin & -position[2] < box.list$zmax
}
# calculates for stops in which aisles they where made
calc.stop.box<- function(log, box){
  
  a<-apply(data.frame(log$x.start,log$z.start), 1, box.check, box.list=box)
  t <- which(a, arr.ind = TRUE)
  t<- data.frame(t)

  res.type<- rep("none", nrow(log))
  res.name<- rep("none", nrow(log))
  
  for (i in 1:nrow(t)) {
    res.type[t[i,2]]<- as.character(box$type[ t[i,1]])
    res.name[t[i,2]]<- as.character(box$aisle.names[t[i,1]])
  }
  data.frame(aisles.type=res.type, aisles.name= res.name)
}
#calculate the moments somone enters end exitst a box( aisles or product box), add information on time and location
calc.box.feature<-function(input.data, box){
  
  # a is a matrix with [n.aisles, n.time points] with T or F.
  # T indicating that timepoint was spent in that aisle
  a <- apply(input.data[, c(2, 4)], 1, box.check, box.list = box)
  # t is a matrix with 2 columns. [, 1] = aisle number and [, 2] = time point
  t <- which(a, arr.ind = TRUE)
  t<-data.frame(t)
  # order.of.visiting is a list containging the entering points of an aisles
  start.points<-t[t[,1] != lag(t[,1], default = !t[1,1]),]
  stop.points<-t[t[,1] != lead(t[,1], default = !t[1,1]),]
  
  order.of.visiting<-data.frame(id= start.points$row, start=start.points$col,stop=stop.points$col)
  order.of.visiting<- add.times.location(order.of.visiting,input.data)
  return(order.of.visiting)
}
# add productbox name to product box log
productbox.label.add<-function(data, input.data,box){
  
  
  #add labels and aisles names
  data$name<-box$productnumber[data$id]
  
  data$name<-factor(data$name, levels = box$productnumber)
  return(data)
}

# add the type of walking behaviour in an aisles and the aisles name
aisles.label.add<-function(order.of.visiting, input.data, aisles){

  #add label
  if(aisles$type[order.of.visiting[1,1]]== "main"){
    label.list<- c("main")
  }else{
    label.list<- c("random start")
  }
  
  for (j in 2:(nrow(order.of.visiting)-1)) {
    if(aisles$type[order.of.visiting[j,1]]== "main"){
      label.list<-c(label.list,"main")
    }else if(order.of.visiting[j-1,1]!= order.of.visiting[j+1,1]){
      label.list<-c(label.list,"walk through")
    }else{
      label.list<-c(label.list,"same side in out")
    }
  }
  if(aisles$type[order.of.visiting[nrow(order.of.visiting),1]]== "main"){
    label.list<- c(label.list,"main")
  }else{
    label.list<- c(label.list,"random stop")
  }
  #add labels and aisles names
  order.of.visiting$label<-label.list
  order.of.visiting$aisles.name<-aisles$aisle.names[order.of.visiting$id]
  
  order.of.visiting$aisles.name<-factor(order.of.visiting$aisles.name, levels = aisles$aisle.names)
  return(order.of.visiting)
}
#returns a subset of the data, which is part of the log subsets
log.subset<- function(data, log, rev= FALSE){
  subset<-rep(FALSE, nrow(data))
  
  for (i in 1:nrow(log)) {
    subset[log$start[i]:log$stop[i]]<-TRUE
  }
  
  if(rev){
    data[!subset,]
  }
  else{
    data[subset,]
  }

}












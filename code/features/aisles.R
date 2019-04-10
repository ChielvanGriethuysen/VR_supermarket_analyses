# VR Supermarkt: function to extract how much time was spent in different aisles.
#
# Last edited 2018-10-18 by Laurent Smeets (l.s.m.smeets@uu.nl)



getAisleTimes <- function(data, FootPosition, time, gg,
                          aisles, full.images, save.data, i){
  
  if(full.images){
    gg.aisles <- gg
    
    # Create rectangles of aisles. append and $layers is used to plot
    # the rectangles and names UNDER the path
    suppressWarnings(
      gg.aisles  <- gg.aisles+
                                 geom_rect(data = aisles,
                                           mapping = aes(xmin = xmin, xmax = xmax,
                                                         ymin = zmin, ymax = zmax,
                                                         colour = colour, x = x, y = y, fill=factor(type)), 
                                           alpha = .3)+
        geom_label(data = aisles,
                   mapping = aes(x = xmin, y = zmin, label = aisles$aisle.names),
                    label.size = .32))
  } else {
    gg.aisles <- gg
  }
  
  
  #Function to check whether a coordinate is in one of the aisles.
  CheckPathInAisle <- function(position, aisles){
    position[1] > aisles$xmin & position[1] < aisles$xmax &
      -position[2] > aisles$zmin & -position[2] < aisles$zmax
  }
  
  
  
  # a is a matrix with [n.aisles, n.time points] with T or F.
  # T indicating that timepoint was spent in that aisle
  a <- apply(FootPosition[, c(1, 3)], 1, CheckPathInAisle, aisles = aisles)
  # t is a matrix with 2 columns. [, 1] = aisle number and [, 2] = time point
  t <- which(a, arr.ind = TRUE)
  aisle.time.points <- t[, 2]
  
  # find order in wich the aisles are visited to determine if a aisles is walked thru or if there was a turnaround 
  # also calculate the distance used to determine efficiency
  order.of.visiting<-t[t[,1] != lag(t[,1], default = !t[1,1]),]
  same.side.in.out<-walk.thru<-0
  dist.same.side.in.out<-dist.walk.thru<-c()
  
  for (i in 1:(nrow(order.of.visiting)-2)) {
    if(aisles$type[order.of.visiting[i,1]]== "main"){
      if(order.of.visiting[i,1]!= order.of.visiting[i+2,1]){
        walk.thru<-walk.thru+1
        dist.walk.thru<-c(dist.walk.thru,subdistance(FootPosition,order.of.visiting[i+1,2], order.of.visiting[i+2,2]))
      }
      else{
        same.side.in.out<-same.side.in.out+1
        dist.same.side.in.out<-c(dist.same.side.in.out,subdistance(FootPosition,order.of.visiting[i+1,2], order.of.visiting[i+2,2]))
      }
    }
  }
  
  
  if(any(a) &  save.data){
    time.in.aisle <- time[t[c(diff(t[,1]) != 0, T), 2]] - time[t[c(T, diff(t[,1]) != 0), 2]] #time spent in each aisle (which aisle is speficied later)
    times.through.aisle <- t[c(TRUE, diff(t[,1]) != 0), ] # dit klopt niet
    times.through.aisle <- data.frame(aisles$aisle.names[times.through.aisle[, 1]], times.through.aisle, time.in.aisle)
    colnames(times.through.aisle)[1:3] <- c("aisle.names", "aisle", "Entry.time.point")
    times.through.aisle$aisle.names <- factor(times.through.aisle$aisle.names, levels = aisles$aisle.names)
    #time.through.aisle is now a data frame with aisle names (as a factor to include all aisle),
    #aisle, numeric, number corresponding to aisle name
    #entry time point, when someone entered that aisle
    #time.in.aisle, time spent in each aisle
    
    #Count number of times in each aisle
    #data[i, 4:17] <- table(times.through.aisle$aisle.names) 
    table.times.through<-table(times.through.aisle$aisle.names)
    data[i,]<-mutate(data[i,],
                     n.aisle.1A= table.times.through[1],
                     n.aisle.2A= table.times.through[2],
                     n.aisle.3A= table.times.through[3],
                     n.aisle.4A= table.times.through[4],
                     n.aisle.5A= table.times.through[5],
                     n.aisle.1B= table.times.through[6],
                     n.aisle.2B= table.times.through[7],
                     n.aisle.3B= table.times.through[8],
                     n.aisle.4B= table.times.through[9],
                     n.aisle.5B= table.times.through[10],
                     n.aisle.6B= table.times.through[11],
                     n.aisle.M1= table.times.through[12],
                     n.aisle.M2= table.times.through[13],
                     n.aisle.M3= table.times.through[14])
    
    
    #Calculate total time in each aisle
    aisle.times <- tapply(times.through.aisle$time.in.aisle, 
                          list(Category = times.through.aisle$aisle.names), 
                          FUN = sum)
    aisle.times[is.na(aisle.times)] <- 0
    
    
    data[i,]<-mutate(data[i,],
                     time.aisle.1A= aisle.times[1],
                     time.aisle.2A= aisle.times[2],
                     time.aisle.3A= aisle.times[3],
                     time.aisle.4A= aisle.times[4],
                     time.aisle.5A= aisle.times[5],
                     time.aisle.1B= aisle.times[6],
                     time.aisle.2B= aisle.times[7],
                     time.aisle.3B= aisle.times[8],
                     time.aisle.4B= aisle.times[9],
                     time.aisle.5B= aisle.times[10],
                     time.aisle.6B= aisle.times[11],
                     time.aisle.M1= aisle.times[12],
                     time.aisle.M2= aisle.times[13],
                     time.aisle.M3= aisle.times[14])
    
    
  } 
  # check, per coordinate whether they are in an aisle or not.
  a.shoppingaisles <- apply(FootPosition[, c(1, 3)], 1, CheckPathInAisle, aisles = filter(aisles, type=="shopping"))
  t.shoppingaisles <- which(a.shoppingaisles, arr.ind = TRUE)
  shopping.aisle.time.points <- t.shoppingaisles[, 2]
  
  
  
  res.aisles <- list(gg.aisles = gg.aisles,
                     data = data,
                     aisle.time.points = aisle.time.points,
                     shopping.aisle.time.points=shopping.aisle.time.points)
  return(res.aisles)
}




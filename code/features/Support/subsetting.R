# Last edited 2019-11-11 by Chiel van Griethuijsen (m.a.vangriethuijsen@students.uu.nl)

##
## Code for speed subsetting
##


# make parts that are above or below a certain speed, merge these parts if they are close to each other to deal with data errors and small movements
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
        if(input.data$time[j]-input.data$time[i]>0.25){
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
        if(input.data$time[j]-input.data$time[i]>0.25){
          candidates<-rbind(candidates,data.frame(start= i,stop=j))
          i<-j
        }
      }
      i<-i+1
    }
  }
  #if parts are to short afther each other then combine them
  #to do: only do this when the the inbetween part is smaller than the two parts that can be combined
  #output<-speed.discretisation.merge.induction(candidates,input.data, merge.dist)
  #output
  return(candidates)
  
}
speed.discretisation.merge.induction<-function(candidates, input.data, merge.dist){
  if(nrow(candidates)>1){
    for(l in 1:(nrow(candidates)-1)){
      if((input.data$time[candidates$stop[l]]-input.data$time[candidates$start[l]]>input.data$time[candidates$start[l+1]]-input.data$time[candidates$stop[l]] ||
          input.data$time[candidates$stop[l+1]]-input.data$time[candidates$start[l+1]]>input.data$time[candidates$start[l+1]]-input.data$time[candidates$stop[l]])&&
         input.data$time[candidates$start[l+1]]-input.data$time[candidates$stop[l]]<merge.dist){
        
        candidates$stop[l]<-candidates$stop[l+1]
        candidates<- candidates[-(l+1),]
        return(speed.discretisation.merge.induction(candidates,input.data,merge.dist))
      }
    }
  }
  return(candidates)
}

##
## code for box subsetting
##
# check if a position is in one of the boxes of a box list
box.check<-function(position, box.list){
  position[1] > box.list$xmin & position[1] < box.list$xmax &
    position[2] > box.list$zmin & position[2] < box.list$zmax
}
# calculates for stops in which aisles they where made
calc.spot.event.in.box<- function(log, box){
  
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
#calculate the moments somone enters end exitst a box( aisles or product box), boxes may overlap
calc.box.feature<-function(input.data, box){
  
  # a is a matrix with [n.aisles, n.time points] with T or F.
  # T indicating that timepoint was spent in that aisle
  a <- apply(input.data[, c(2, 4)], 1, box.check, box.list = box)
  # t is a matrix with 2 columns. [, 1] = aisle number and [, 2] = time point
  t <- which(a, arr.ind = TRUE)
  t<-data.frame(t)
  
  # use max en min point as start en stop and reorder the data
  order.of.visiting <-t %>% group_by(row) %>% summarise(start= min(col), stop = max(col)) %>% 
    rename(box.id= row) %>% arrange(start)
  
  return(order.of.visiting)
}




# Last edited 2019-11-11 by Chiel van Griethuijsen (m.a.vangriethuijsen@students.uu.nl)

##
## Code for speed subsetting
##


# make parts that are above or below a certain speed, merge these parts if they are close to each other to deal with data errors and small movements
calc.speed.discretisation<-function(input.data, cuttoff, lowerinequations=TRUE){
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
  return(candidates)
}
#if parts are to short afther each other then combine them
#to do: only do this when the the inbetween part is smaller than the two parts that can be combined
speed.discretisation.merge.induction<-function(candidates, input.data, merge.dist){
  if(nrow(candidates)>1){
    for(l in 1:(nrow(candidates)-1)){
      # if((input.data$time[candidates$stop[l]]-input.data$time[candidates$start[l]]>input.data$time[candidates$start[l+1]]-input.data$time[candidates$stop[l]] ||
      #     input.data$time[candidates$stop[l+1]]-input.data$time[candidates$start[l+1]]>input.data$time[candidates$start[l+1]]-input.data$time[candidates$stop[l]])&&
      #    input.data$time[candidates$start[l+1]]-input.data$time[candidates$stop[l]]<merge.dist){
      start.a<- candidates$start[l]
      start.b<- candidates$start[l+1]
      stop.a<- candidates$stop[l]
      stop.b<- candidates$stop[l+1]
      
      if(dist(input.data[c(start.b,stop.a),]%>% select(x,z))<merge.dist &&(
         diff(input.data[c(stop.a,start.b),]%>% pull(time))<diff(input.data[c(start.a,stop.a),] %>% pull(time))||
         diff(input.data[c(stop.a,start.b),]%>% pull(time))<diff(input.data[c(start.b,stop.b),] %>% pull(time)))){
        candidates$stop[l]<-candidates$stop[l+1]
        candidates$parts[l]<- candidates$parts[l]+1
        candidates$walk.time[l]<-candidates$walk.time[l]+ input.data$time[start.b]-input.data$time[stop.a]
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
  position[1] > box.list[1] & position[1] < box.list[2] &
    position[2] > box.list[3] & position[2] < box.list[4]
}
#
box.check.list<-function(points, box.list){
  # a is a matrix with [n.aisles, n.time points] with T or F.
  # T indicating that timepoint was spent in that aisle
  a<-future_apply(data.frame(points$x,points$z), 1, box.check, box.list=box.list)
  # t is a matrix with 2 columns. [, 1] = aisle number and [, 2] = time point
  t <- which(a, arr.ind = TRUE)
  t<- data.frame(t)
  return(t)
}


# calculates for stops in which aisles they where made
calc.spot.event.in.box<- function(spot.log, box){
  if(nrow(spot.log)<1){
    return(data.frame(row=numeric(), col=numeric()))
  }
  
  t<-box.check.list(data.frame(x=spot.log$x,z=spot.log$z),box)
  return(t)
}
#   res.type<- rep("none", nrow(spot.log))
#   res.name<- rep("none", nrow(spot.log))
#   
#   res.type[t$col]<- as.character(box$type[t$row])
#   res.name[t$col]<- as.character(box$names[t$row])
#   
#   data.frame(type=res.type, names= res.name)
# }
#calculate the moments somone enters end exitst a box( aisles or product box), boxes may overlap
calc.box.feature<-function(input.data, box){
  t<- box.check.list(data.frame(x=input.data$x,z=input.data$z), box)
  
  if(nrow(t)==0){
    return(data.frame(box.id= numeric(),
                      start=numeric(),
                      stop= numeric()))
  }
  
  # use max en min point as start en stop and reorder the data
  # but first divide the data in the individual segments by finding a gap, done by looking at the change in two series
  order.of.visiting <-t %>% group_by(row) %>% mutate(id = 1:length(col), gap.id= id-col) %>% group_by(row, gap.id) %>% 
    summarise(start= min(col), stop = max(col)) %>% 
    rename(box.id= row) %>% arrange(start) %>% select(-gap.id) %>% data.frame()
  
  #merge when gap between exiting and entering the same box is small, making the box more robuste
  order.of.visiting<- merge.box.feature(order.of.visiting %>% arrange(box.id, start), input.data) %>% arrange(start)
  
  
  return(order.of.visiting)
}
#merge when gap between exiting and entering the same box is small, making the box more robuste
merge.box.feature<- function(boxen, input.data){
  if (nrow(boxen)<2) {
    return(boxen)
  }
  
  
  for(i in 1:(nrow(boxen)-1)){
    if(boxen$box.id[i]==boxen$box.id[i+1]){
      gap<- sum(input.data$dist[boxen$start[i+1]:boxen$stop[i]])
      if(gap<5)
      {
        boxen$stop[i]<- boxen$stop[i+1]
        boxen<-boxen[-(i+1),]
        boxen<-merge.box.feature(boxen,input.data)
        break
      }
    }
  }
  return(boxen)
}

###
### time based subsetting
###

time.before.subset<- function(stop, time.before, input.data){
  if(length(stop)==0){
    return(data.frame(start= numeric(), stop=numeric()))
  }
  
  intervals<-data.frame(start= stop, stop)
  for(i in 1:length(stop)){
    j<- stop[i]
    time<-0
    while (time.before>time) {
      time<-input.data$time[stop[i]] -input.data$time[j]
      j<-j-1
    }
    
    intervals$start[i]<- j
  }
  return(intervals)
}


# Last edited 2019-11-11 by Chiel van Griethuijsen (m.a.vangriethuijsen@students.uu.nl)


add.basic.features<- function(points, input.data){
  points$start.time.data<-input.data$time[points$start]
  points$stop.time.data<- input.data$time[points$stop]
  points$start.time<- points$start.time.data-first(input.data$time)
  points$stop.time<- points$stop.time.data-first(input.data$time)
  points$time.spent<-points$stop.time- points$start.time
  points$moment.in.time.start<- points$start.time/(last(input.data$time)-first(input.data$time))
  points$moment.in.time.stop<- points$stop.time/(last(input.data$time)-first(input.data$time))
  points$x.start<-input.data$x[points$start]
  points$z.start<-input.data$z[points$start]
  points$x.stop<-input.data$x[points$stop]
  points$z.stop<-input.data$z[points$stop]
  points$absolute.dist<- numeric(nrow(points))
  points$relative.dist<-sqrt((points$x.start-points$x.stop)^2+(points$z.start-points$z.stop)^2)
  points$var.speed<- numeric(nrow(points))
  
  if(nrow(points)>0){
    for(i in 1:nrow(points)){
      points$absolute.dist[i]<- sum(input.data$dist[points$start[i]:points$stop[i]])
      points$var.speed[i]<- var(input.data$dist[points$start[i]:points$stop[i]])
    }
  }
  points$absolute.speed<-points$absolute.dist/points$time.spent
  points$relative.speed<-points$relative.dist/points$time.spent
  points$dist.frac<-points$absolute.dist/points$relative.dist
  return(points)
}

add.quality.features<- function(points, input.data){
  points$nzero<- numeric(nrow(points))
  points$errorfrac<-numeric(nrow(points))
  
  if(nrow(points)>0){
    for(i in 1:nrow(points)){
      points$nzero[i]<- sum(input.data$dist[points$start[i]:points$stop[i]]==0)
    }
  }
  points$errorfrac<- points$nzero/(points$stop-points$start)
  return(points)
}
add.view.quality.features<- function(points, input.data, input.look= res$input.look){
  points$missing.view.mean<- numeric(nrow(points))
  points$missing.view.max<- numeric(nrow(points))
  points$missing.view.frac<- numeric(nrow(points))
  points$missing.view.n<- numeric(nrow(points))
  
  if(nrow(points)>0){
    for(i in 1:nrow(points)){
      missing.stats<-missing.data.length(input.look[points$start[i]:points$stop[i],])
      points$missing.view.mean[i]<- missing.stats$mean
      points$missing.view.max[i]<-missing.stats$max
      points$missing.view.frac[i]<-missing.stats$missing
      points$missing.view.n[i]<-missing.stats$n
    }
  }
  return(points)
}


# add the type of walking behaviour in an aisles and the aisles name
# aisles.label.add<-function(order.of.visiting, input.data, aisles){
#   
#   #add label
#   if(aisles$type[order.of.visiting$box.id[1]]== "main"){
#     label.list<- c("main")
#   }else{
#     label.list<- c("random start")
#   }
#   if(nrow(order.of.visiting)>2){
#     for (j in 2:(nrow(order.of.visiting)-1)) {
#       if(aisles$type[order.of.visiting$box.id[j]]== "main"){
#         label.list<-c(label.list,"main")
#       }else if(order.of.visiting$box.id[j-1]!= order.of.visiting$box.id[j+1]){
#         label.list<-c(label.list,"walk through")
#       }else{
#         label.list<-c(label.list,"same side in out")
#       }
#     }
#   }
#   if(aisles$type[order.of.visiting$box.id[nrow(order.of.visiting)]]== "main"){
#     label.list<- c(label.list,"main")
#   }else{
#     label.list<- c(label.list,"random stop")
#   }
#   #add labels and aisles names
#   order.of.visiting$label<-label.list
#   order.of.visiting$aisles.name<-aisles$names[order.of.visiting$box.id]
#   
#   order.of.visiting$aisles.name<-factor(order.of.visiting$aisles.name, levels = aisles$names)
#   return(order.of.visiting)
# }
# add the type of walking behaviour in an aisles and the aisles name
aisles.label.add<- function(log, input.data, aisles){
  
  start<- box.check.list(data.frame(x=input.data$x[log$start], z=input.data$z[log$start]),aisles)
  stop <- box.check.list(data.frame(x=input.data$x[log$stop], z=input.data$z[log$stop]),aisles)
  
  for (i in 1:nrow(log)) {

    a<- start %>% filter(col==i)
    b<- stop %>% filter(col==i)
    
    if(nrow(a)==1 &&nrow(b)==1){
      log$label[i]<-"random part"
    }else if(nrow(a)==1){
      log$label[i]<-"start"
    }else if(nrow(b)==1){
      log$label[i]<-"stop"
    }else if(all(a==b)){
      log$label[i]<-"same side in out"
    }else{
      log$label[i]<-"walk through"
    }
  }
  return(log)
}

#add the nr of stops to the aisles log
add.stops.to.aisles.log<- function(stops, aisles){
  aisles$n.stops<- numeric(nrow(aisles))
  
  for(i in 1:nrow(aisles)){
    if(nrow(stops)>0){
      for(j in 1:nrow(stops)){
        if(aisles$start[i]<stops$start[j]&& aisles$stop[i]>stops$stop[j]){
          aisles$n.stops[i]<- aisles$n.stops[i]+1
        }
      }
    }
  }
  return(aisles)
}
#add standing position to hit log
add.product.hit.position<-function(product.hits, position){
  # product.hits$x.pos<- numeric(nrow(product.hits))
  # product.hits$z.pos<- numeric(nrow(product.hits))
  # product.hits$time.id<-numeric(nrow(product.hits))
  if(nrow(product.hits)>0)
    for (i in 1:nrow(product.hits)) {
      index<-first(which(position$time> product.hits$time[i]))
      product.hits$x.pos[i]<- position$x[index]
      product.hits$z.pos[i]<- position$z[index]
      product.hits$time.index[i]<- index
    }
  return(product.hits)
}

# add hitted product id to stops, if a product was hit during the stop
hit.stop<- function(hits, stops){
  stops$hit_2<-stops$hit_1<- "none"
  stops$n_hit<-0
  for (i in 1:nrow(stops)) {
    item<-which(between(hits$time, stops$start.time.data[i],stops$stop.time.data[i]))
    if (length(item)==1){
      stops$hit_1[i]<- hits$prod_id[item]
      stops$n_hit[i]<-1
    }else if(length(item)>1){
      stops$hit_1[i]<- hits$prod_id[item[1]]
      stops$hit_2[i]<- hits$prod_id[item[2]]
      stops$n_hit[i]<- length(item)
    }
  }
  return(stops)
}

stop.to.walk<- function(log, input.data){
  res<- data.frame(start= numeric(), stop=numeric())
  start<-1
  stop<-log$start[1]-1
  
  for(i in 1:nrow(log)){
    res<-rbind(res, data.frame(start=start,stop=stop))
    start<-log$stop[i]+1
    stop<-log$start[i+1]-1
  }
  rbind(res, data.frame(start=start,stop=nrow(input.data)))
}

# add productbox name to product box log
productbox.label.add<-function(data, input.data,box){
  
  
  #add labels and aisles names
  data$name<-box$productnumber[data$box.id]
  
  data$name<-factor(data$name, levels = box$productnumber)
  return(data)
}
# for a vector of angles calculates a summary of a subpart
direction.sub.summary<- function(directions, start, stop){
  corection.factor<- 0
  if(any(directions[start:stop] %>% na.omit() >300) && any(directions[start:stop] %>% na.omit()<60)){
    directions<- (directions+180)%%360
    corection.factor<-180
  }
  
  max<-max(directions[start:stop], na.rm = TRUE)
  min<-min(directions[start:stop], na.rm = TRUE)
  
  return(data.frame(mean= (mean(directions[start:stop], na.rm = TRUE)-corection.factor)%%360,
                    max= (max-corection.factor)%%360,
                    min= (min-corection.factor)%%360,
                    max.diff= abs(max-min),
                    var= var(directions[start:stop], na.rm = TRUE)))
}
direction.sub.summary.feed<- function(directions, intervals,type){
  if(nrow(intervals)>0){
    angles.summary <- mapply(direction.sub.summary, 
                          start= intervals$start, 
                          stop= intervals$stop, 
                          MoreArgs = list(directions= directions)) %>% 
                          t() %>% as.data.frame() %>% setNames(paste0(type,".angles.", names(.))) %>% mutate_all(as.numeric)
    
   
  }else{
    angles.summary <- data.frame(mean= numeric(),
                        max= numeric(),
                        min= numeric(),
                        max.diff= numeric(),
                        var= numeric()) %>% setNames(paste0(type,".angles.", names(.))) %>% mutate_all(as.numeric)
  }
  return(angles.summary)
}
dist.sub.summary<- function(dist, start, stop){
  
  max<-max(dist[start:stop], na.rm = TRUE)
  min<-min(dist[start:stop], na.rm = TRUE)
  
  return(data.frame(mean= mean(dist[start:stop], na.rm = TRUE),
                    max= max,
                    min= min,
                    max.diff= abs(max-min),
                    var= var(dist[start:stop], na.rm = TRUE)))
}
dist.sub.summary.feed<- function(dist, intervals, type){
  if(nrow(intervals)>0){
    summary <- mapply(dist.sub.summary, 
                             start= intervals$start, 
                             stop= intervals$stop, 
                             MoreArgs = list(dist= dist)) %>% 
      t() %>% as.data.frame() %>% setNames(paste0(type,".dist.", names(.))) %>% mutate_all(as.numeric)
    
    
  }else{
    summary <- data.frame(mean= numeric(),
                                 max= numeric(),
                                 min= numeric(),
                                 max.diff= numeric(),
                                 var= numeric()) %>% setNames(paste0(type,".dist.", names(.))) %>% mutate_all(as.numeric)
  }
  return(summary)
}



add.view.area<- function(log, input.data, input.look, params){
  log$areas.up<- log$areas.down<-log$areas.2<- log$areas.1<- log$view.sides<- numeric(nrow(log))
  if(nrow(log)>0)
  for(i in 1: nrow(log)){
    areas<- input.look[log$start[i]:log$stop[i],] %>% group_by(aisle.name, aisle.side, hight, width) %>% summarise(n=n())
    log$areas.1[i]<- areas %>% filter(n>=params$features$view$min.points.1 & !is.na(aisle.name)) %>% nrow()
    log$areas.2[i]<- areas %>% filter(n>=params$features$view$min.points.2 & !is.na(aisle.name)) %>% nrow()
    log$areas.up[i]<- areas %>% filter(n>=params$features$view$min.points.1& aisle.side== "up") %>% nrow()
    log$areas.down[i]<- areas %>% filter(n>=params$features$view$min.points.1& aisle.side== "down") %>% nrow()
    
    sides<- input.look[log$start[i]:log$stop[i],] %>% group_by(aisle.name, aisle.side) %>% summarise(n=n()) %>% filter(!is.na(aisle.name))
    log$view.sides[i]<- sides %>% nrow()
  }
  return(log)
}

sways<-function(input.data, start, stop){
  direction<- input.data[start:stop,]$walk.direction
  direction[input.data[start:stop,]$stop]<-NA 
  if(any(direction %>% na.omit() >300) && any(direction %>% na.omit()<60)){
    direction<- (direction+180)%%360
  }
  direction<- cut(direction, c(0,180,360))
  switch.points<- which(direction!= lag(direction,1,direction[1]))
  if(length(switch.points)>1){
    return(data.frame(n.switch.points= length(switch.points),
                      max.dif.x= input.data$x[switch.points+start] %>% diff() %>% abs() %>% max()))
  }else{
    return(data.frame(n.switch.points= NA,
                      max.dif.x= NA))
  }
}
sway.summary.feed<- function(input.data, intervals){
  if(nrow(intervals)>0){
    angles.summary <- mapply(sways, 
                             start= intervals$start, 
                             stop= intervals$stop, 
                             MoreArgs = list(input.data= input.data)) %>% 
      t() %>% as.data.frame() %>% mutate_all(as.numeric)
    
    
  }else{
    angles.summary <- data.frame(n.switch.points= numeric(),
                                 max.diff.x= numeric()) %>% mutate_all(as.numeric)
  }
  return(angles.summary)
}



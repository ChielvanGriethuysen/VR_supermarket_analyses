# Last edited 2019-11-11 by Chiel van Griethuijsen (m.a.vangriethuijsen@students.uu.nl)


add.basic.features<- function(points, input.data, input.look.left= res$input.look.left, input.look.right= res$input.look.right){
  points$start.time<-input.data$time[points$start]
  points$stop.time<- input.data$time[points$stop]
  points$time.spend<-points$stop.time- points$start.time
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
  points$absolute.speed<-points$absolute.dist/points$time.spend
  points$relative.speed<-points$relative.dist/points$time.spend
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
add.view.quality.features<- function(points, input.data, input.look.left= res$input.look.left, input.look.right= res$input.look.right){
  points$missing.view.mean<- numeric(nrow(points))
  points$missing.view.max<- numeric(nrow(points))
  points$missing.view.frac<- numeric(nrow(points))
  points$missing.view.n<- numeric(nrow(points))
  
  input.look<- if(sum(input.look.left$x==0)< sum(input.look.right$x==0)) input.look.left else input.look.right
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
aisles.label.add<-function(order.of.visiting, input.data, aisles){
  
  #add label
  if(aisles$type[order.of.visiting$box.id[1]]== "main"){
    label.list<- c("main")
  }else{
    label.list<- c("random start")
  }
  if(nrow(order.of.visiting)>2){
    for (j in 2:(nrow(order.of.visiting)-1)) {
      if(aisles$type[order.of.visiting$box.id[j]]== "main"){
        label.list<-c(label.list,"main")
      }else if(order.of.visiting$box.id[j-1]!= order.of.visiting$box.id[j+1]){
        label.list<-c(label.list,"walk through")
      }else{
        label.list<-c(label.list,"same side in out")
      }
    }
  }
  if(aisles$type[order.of.visiting$box.id[nrow(order.of.visiting)]]== "main"){
    label.list<- c(label.list,"main")
  }else{
    label.list<- c(label.list,"random stop")
  }
  #add labels and aisles names
  order.of.visiting$label<-label.list
  order.of.visiting$aisles.name<-aisles$aisle.names[order.of.visiting$box.id]
  
  order.of.visiting$aisles.name<-factor(order.of.visiting$aisles.name, levels = aisles$aisle.names)
  return(order.of.visiting)
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
    item<-which(between(hits$time, stops$start.time[i],stops$stop.time[i]))
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

calc.target.aisles<- function(product, aisles){
  
  a<-apply(data.frame(product$x,product$z), 1, box.check, box.list=aisles)
  t <- which(a, arr.ind = TRUE)
  t<- data.frame(t)
  
  res.type<- rep("none", nrow(product))
  res.name<- rep("none", nrow(product))
  
  for (i in 1:nrow(t)) {
    res.type[t[i,2]]<- as.character(aisles$type[ t[i,1]])
    res.name[t[i,2]]<- as.character(aisles$aisle.names[t[i,1]])
  }
  data.frame(aisles.type=res.type, aisles.name= res.name)
}
stop.to.walk<- function(log, input.data){
  res<- data.frame(start= numeric(), stop=numeric())
  start<-1
  stop<-log[1,1]-1
  
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



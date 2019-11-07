# Set of functions used to help analysis and hide the more complex functionalities, makes it easyer to plug in other implementations
#
# Last edited 2019-07-03 by Chiel van Griethuijsen (m.a.vangriethuijsen@students.uu.nl)

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
  input.data<- data.frame(input.data, speed,c(mean(distance.between.points),distance.between.points))
  names(input.data)[6]<- "dist"
  return(input.data)
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
add.times.location<- function(points, input.data, input.look.left= res$input.look.left, input.look.right= res$input.look.right){
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
  points$nzero<- numeric(nrow(points))
  points$errorfrac<-numeric(nrow(points))
  points$n.cross.straight<- numeric(nrow(points))
  points$max.from.straight<- numeric(nrow(points))
  points$mean.from.straight<- numeric(nrow(points))
  points$var.from.straight<- numeric(nrow(points))
  points$missing.view.mean<- numeric(nrow(points))
  points$missing.view.max<- numeric(nrow(points))
  points$missing.view.frac<- numeric(nrow(points))
  points$missing.view.n<- numeric(nrow(points))
  input.look<- if(sum(input.look.left$x==0)< sum(input.look.right$x==0)) input.look.left else input.look.right
  if(nrow(points)>0){
    for(i in 1:nrow(points)){
      points$absolute.dist[i]<- sum(input.data$dist[points$start[i]:points$stop[i]])
      points$var.speed[i]<- var(input.data$dist[points$start[i]:points$stop[i]])
      points$nzero[i]<- sum(input.data$dist[points$start[i]:points$stop[i]]==0)
      
      points$n.cross.straight[i]<- determine.n.sway(points$x.start[i],points$x.stop[i],
                                                    points$z.start[i],points$z.stop[i],input.data$x[points$start[i]:points$stop[i]],
                                                    input.data$z[points$start[i]:points$stop[i]])
      
      
      
      missing.stats<-missing.data.length(input.look[points$start[i]:points$stop[i],])
      points$missing.view.mean[i]<- missing.stats$mean
      points$missing.view.max[i]<-missing.stats$max
      points$missing.view.frac[i]<-missing.stats$missing
      points$missing.view.n[i]<-missing.stats$n
      mean.max.var<- determine.max.mean.var.from.straight(points$x.start[i],points$x.stop[i],
                                                    points$z.start[i],points$z.stop[i],input.data$x[points$start[i]:points$stop[i]],
                                                    input.data$z[points$start[i]:points$stop[i]])
      points$max.from.straight[i]<- mean.max.var$max
      points$mean.from.straight[i]<- mean.max.var$mean
      points$var.from.straight[i]<- mean.max.var$var
    }
  }
  points$errorfrac<- points$nzero/(points$stop-points$start)
  points$absolute.speed<-points$absolute.dist/points$time.spend
  points$relative.speed<-points$relative.dist/points$time.spend
  points$dist.frac<-points$absolute.dist/points$relative.dist
  return(points)
}
#determines max dist from straight line
determine.max.mean.var.from.straight<- function(x.start, x.stop, y.start, y.stop, input.data.x, input.data.y){
  distances<-apply(data.frame(input.data.x, input.data.y), 1, dist2d, b= c(x.start,y.start), c= c(x.stop,y.stop))
  return(list(mean= mean(distances),max =max(distances), var= var(distances)))
}
#calculate distance between point and line
dist2d <- function(a,b,c) {
  v1 <- b - c
  v2 <- a - b
  m <- cbind(v1,v2)
  d <- abs(det(m))/sqrt(sum(v1*v1))
} 
#determines if the nr of sways a person makes
determine.n.sway<- function(x.start, x.stop, y.start, y.stop, input.data.x, input.data.y){
  position <- sign((x.stop - x.start) * (input.data.y - y.start) - (y.stop - y.start) * (input.data.x - x.start))
  position <- position[position!=0]
  n<- sum(position != lag(position,default = position[1]))
  return(n)

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
             productnumber =  products$productnumber
             ) %>%
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
  if(nrow(order.of.visiting)>2){
    for (j in 2:(nrow(order.of.visiting)-1)) {
      if(aisles$type[order.of.visiting[j,1]]== "main"){
        label.list<-c(label.list,"main")
      }else if(order.of.visiting[j-1,1]!= order.of.visiting[j+1,1]){
        label.list<-c(label.list,"walk through")
      }else{
        label.list<-c(label.list,"same side in out")
      }
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
#analyse productlog, put products in a dataframe
product.hit.log<- function(logs){
  logs<-logs  %>% filter(apply(logs,2, str_detect, pattern= "HIT"))%>% 
    separate(SesionLog, c("time","Product"), sep = "- HIT product #")%>% 
    separate(Product, c("product", "rest"), sep = c("\\|\\| X:"),extra = "merge")%>% 
    separate(rest, c("x", "rest"), sep = c("\\|\\| Y:"),extra = "merge")%>% 
    separate(rest, c("y", "z"), sep = c("\\|\\| Z:"),extra = "merge")
  
  hour<- logs$time %>% str_sub(2,3)%>% as.numeric()
  minutes<- logs$time %>% str_sub(5,6)%>% as.numeric()
  seconds<- logs$time %>% str_sub(8,9)%>% as.numeric()
  rest<- logs$time %>% str_sub(11,13)%>% as.numeric()
  
  time<- hour*60*60+minutes*60+seconds+rest/1000
  logs$time<- time
  
  logs[,3:5]<- sapply (logs[,3:5], as.numeric)
  return(logs)
  
}
#check if product is hit in the expacted place
#to do, use product range location instead of one point
check.product.hit<- function(hit.log, products){
  
  products$z<- -products$z
  res<- data.frame(prod_id= as.numeric(),name.product= as.character(), name.hit= as.character(),
                   x.product=as.numeric(),z.product=as.numeric(), 
                   x.hit=as.numeric(),z.hit=as.numeric(),time=numeric(), d= numeric())
  if(nrow(hit.log)<1){
    return(res)
  }
  
  for (i in 1:nrow(hit.log)) {
    pos.log<- hit.log[i,c(3,5)]
    for(j in 1:nrow(products)){
      pos.product<- products[j,1:2]
      d<- dist(rbind(pos.log, pos.product))
      if(d<0.5)
      {
        res<-rbind(res,data.frame(prod_id= j,name.product= products$productnumber[j], name.hit= hit.log$product[i],
                                  x.product=pos.product$x,z.product=pos.product$z,
                                  x.hit=pos.log$x,z.hit=pos.log$z,time= hit.log$time[i], d=as.numeric(d)))
      }
    }
  }
  return(res)
  
}
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


#remove crossings that are to close to eachather
# crossings.filter.close<- function(crossings, dist){
#   if(nrow(crossings)<2)
#     return(crossings)
#   remove.list<- c()
#   for(i in 2:nrow(crossings))
#   {
#     to.close<-FALSE
#     for (j in (i-1):1) {
#       if(dist(crossings[c(i,j),6:7])<dist){
#         to.close<-TRUE
#       }
# 
#     }
#     if(to.close)
#       remove.list<- c(remove.list,i)
#   }
#   if(length(remove.list>0)){
#     return(crossings[-remove.list,])
#   }
#   return(crossings)
# }

crossings.filter.close<- function(crossings, dist, start){
  change<-FALSE
  for(i in start:(nrow(crossings))){
    for(j in (i-1):1)
      if(dist(crossings[c(i,j),6:7])<dist){
      change<-TRUE
      crossings<- crossings[-i,]
      if(nrow(crossings)>=i){
        return(crossings.filter.close(crossings, dist, i))
      }
      else{
        return(crossings)
      }
    }
  }
  return(crossings)
}

calc.target.aisles<- function(product, aisles){
  
  a<-apply(data.frame(product$x,-product$z), 1, box.check, box.list=aisles)
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

calc.short.dist<- function(aisles, product.hits, input.data){
  product.hits$short.dist<- numeric(nrow(product.hits))
  product.hits$walk.dist<- numeric(nrow(product.hits))
  main.aisles<- aisles %>% filter(type == "main") %>% mutate(zmin= -zmin, zmax= -zmax) 
  mid.points<- (main.aisles$zmax+main.aisles$zmin)/2
  if(nrow(product.hits)>1)
  for(i in 1:(nrow(product.hits)-1)){
    if(product.hits$aisles.name[i]==product.hits$aisles.name[i+1]){
      product.hits$short.dist[i]<-abs(product.hits$z[i]-product.hits$z[i+1])
      product.hits$walk.dist[i]<- sum(input.data$dist[product.hits$time.index[i]:product.hits$time.index[i+1]])
    }else
    {
      dist.to.midle.1<-mid.points-product.hits$z.pos[i]
      dist.to.midle.2<-mid.points-product.hits$z.pos[i+1]
      product.hits$short.dist[i]<- min(abs(dist.to.midle.1)+abs(dist.to.midle.2))+
        abs(product.hits$x.pos[i]-product.hits$x.pos[i+1])
      product.hits$walk.dist[i]<- sum(input.data$dist[product.hits$time.index[i]:product.hits$time.index[i+1]])
      
    }
    
    
  }
  product.hits$dist.frac<- product.hits$walk.dist/product.hits$short.dist
  return(product.hits)
  
}
filter.product.hits<- function(targets, hits){
  # give indexes of hitted items that are also targets
  hit.target<- lapply(hits$product, function(x) str_detect(x,targets$productname)) %>% lapply(function(x)any(x))%>% 
    unlist()
  
  false.hits<- hits%>% filter(!hit.target)
  # give logical indexes of related products when a product was not correct
  hit.related<- lapply(false.hits$product, function(x) lapply( targets$alternativename, function(y) any(str_detect(x,y))))%>% 
    lapply(function(x)any(unlist(x))) %>% 
    unlist()
  
  return(list(hit.target= hits %>% filter(hit.target),
              hit.related= false.hits %>% filter(hit.related)))
  
}

# finds out the longest cerie of zerro's in the data, to detect the lagest gap in the data  
missing.data.length<- function(data){
  n<-start<- stop<- res.stop<-i<-0
  length<- time<-res.start<- c()
  while (i<=nrow(data)) {
    i<-i+1
    if(i<= nrow(data) &&data$x[i]==0){
      start<- i
      while (i<= nrow(data) &&data$x[i]==0) {
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
                    missing= sum(data$x==0)/nrow(data)))
}
missing.view.data.fill<- function(data){
  n<-start<- stop<- i<-0
  while (i <= nrow(data)) {
    i<-i+1
    if(i<= nrow(data) &&data$x[i]==0){
      start<- i
      while (i<= nrow(data) &&data$x[i]==0) {
        i<-i+1
      }
      stop<- i-1
      #fill in gap when time between points is les than 0.1 sec
      if(data$time[stop]-data$time[start]<0.1)
      {
        for (j in start:stop) {
          if(start==1){
            data$x[j]<- data$x[stop+1]
            data$y[j]<- data$y[stop+1]
            data$z[j]<- data$z[stop+1]
          } else if(stop == nrow(data)){
            data$x[j]<- data$x[start-1]
            data$y[j]<- data$y[start-1]
            data$z[j]<- data$z[start-1]
            
          }else{
            data$x[j]<- mean(data$x[start-1],data$x[stop+1])
            data$y[j]<- mean(data$y[start-1],data$y[stop+1])
            data$z[j]<- mean(data$z[start-1],data$z[stop+1])
          }
        }
      }
      if(i >= nrow(data)){
        break
      }
    }
  }
  return(data)
}

#for each point finds what the first point is afther a sertain distance
find.first.point.on.dist<- function(data, dist)
{
  d<- 0
  data$point.on.dist<-0
  j<-0
  for (i in 1: nrow(data)) {
    
    while (d<dist) {
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
    if(A== c(0,0))
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


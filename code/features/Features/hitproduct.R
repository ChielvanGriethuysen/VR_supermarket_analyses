picked.products<- function(input.data, view, products,params){
  
  intervals<- time.before.subset(products$time.index, params$features$pick$pick.time.before, input.data)
  intervals<- add.basic.features(intervals,input.data)
  #add hit distance
  intervals$hit.dist<- sqrt((products$x-products$x.pos)^2+(products$z-products$z.pos)^2)
  #add angle of hit
  angles<-calculate.direction(products%>% select(x,z), products%>% select(x.pos,z.pos)) %%180
  intervals$hit.direction<- mapply(function(a,b) min(c(a,b)), a=angles, b= 180-angles )
  
  #add pre angular moving behaviour
  walk.angles.summary<- direction.sub.summary.feed(input.data$walk.direction, intervals,"walk")
  view.angles.summary<- direction.sub.summary.feed(view$angle, intervals,"view")
  intervals<- cbind(intervals, walk.angles.summary, view.angles.summary)
  
  #add view quality statistics
  intervals<- add.view.quality.features(intervals, input.data)
  
  intervals<- add.view.area(intervals, input.data,view, params)
  
  
  
  
  
  return(list(log= intervals))
}


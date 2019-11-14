picked.products<- function(input.data, products,params){
  
  intervals<- time.before.subset(products$time.index, params$features$pick$pick.time.before, input.data)
  intervals<- add.basic.features(intervals,input.data)
  #add hit distance
  intervals$hit.dist<- sqrt((products$x-products$x.pos)^2+(products$z-products$z.pos)^2)
  #add angle of hit
  angles<-calculate.direction(products%>% select(x,z), products%>% select(x.pos,z.pos)) %%180
  intervals$hit.direction<- mapply(function(a,b) min(c(a,b)), a=angles, b= 180-angles )
  
  return(list(log= intervals))
}
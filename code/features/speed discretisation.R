# VR Supermarkt: function to extract the stops.
#
# Last edited 31-5-2019 by Chiel van Griethuijsen (m.a.vangriethuijsen@students.uu.nl)

speeddiscretisation<-function(input.data, aisles.log,hits.log, products,
                              stop.params, walk.params,
                              aisles, i){
  #get all mearged discretisations of the speed
  c.stops<-calc.speed.discretisation(input.data = input.data, merge.dist = stop.params$stop.merge.distance, 
                                   cuttoff = stop.params$stop.max.speed,lowerinequations = TRUE)
  # walks<-calc.speed.discretisation(input.data = input.data, merge.dist = walk.params$walk.merge.distance,
  #                                  cuttoff = walk.params$walk.min.speed,lowerinequations = FALSE)
  
  #find real stops
  p.stops<- speed.discretisation.merge.induction(c.stops,input.data,  stop.params$stop.merge.distance)
  #add info
  p.stops<-add.times.location(p.stops,input.data)
  
  # r.stops<-p.stops%>% filter( absolute.speed<=0.06 )
  # slows<-p.stops%>% filter( absolute.speed>0.06)
  r.stops<-p.stops%>% filter( relative.dist<=0.5)
  slows<-p.stops%>% filter( relative.dist>0.5)
  
  # r.stops<- speed.discretisation.merge.induction(r.stops,input.data, 1.5)
  # slows<- speed.discretisation.merge.induction(slows,input.data, 2.5)
  
  #update info
  slows<- add.times.location(slows,input.data)
  r.stops<- add.times.location(r.stops,input.data)
  
  
  #filters
  r.stops<-r.stops %>% filter(time.spend>stop.params$stop.minimum.duration)
  slows<-slows %>% filter(time.spend>stop.params$stop.minimum.duration)
  r.stops$label<-rep("stop with no movement", nrow(r.stops))
  slows$label<-rep("stop with some movement", nrow(slows))
  
  log<-rbind(slows,r.stops)
  log<- log %>% arrange(start)
  
  #add info
  walks<-stop.to.walk(log,input.data)
  walks<-add.times.location(walks,input.data)
  walks<-walks %>% filter(time.spend>walk.params$walk.minimum.duration)
  walks$label<-rep("walk", nrow(walks))
  log<-rbind(log,walks)

  
  # p.stops<- p.stops %>% filter(time.spend>stop.params$stop.minimum.duration)
  # stops.slow<-p.stops%>% filter( absolute.speed<=0.05)
  # stops.fast<-p.stops%>% filter( absolute.speed>0.05)
  

  
  #add labels
  #slows$label<-rep("slow", nrow(slows))


  
  #combine to one dataframe
  #log<-rbind(slows,r.stops,walks)
  #log<- rbind(r.stops,slows)
  log<- log %>% arrange(start)
  #add location of stop and slow
  log<- cbind(log, calc.spot.event.in.box(log, aisles))
  
  #add if it is in a target aisles
  log$target<- log$aisles.name %in% calc.target.aisles(products, aisles)[,2]
  
  # add stops to ailes log
  aisles.log<- add.stops.to.aisles.log(r.stops, aisles.log)
  
  # add if a product was hit, if so, which(first and second) one and the ammount of hits for the case that there are more
  log<- hit.stop(hits.log,log)
  
  speed.res<-list(speed.log = log,
                  aisles.log= aisles.log
                  )
  
  return(speed.res)
}


  


# VR Supermarkt: function to extract the stops.
#
# Last edited 31-5-2019 by Chiel van Griethuijsen (m.a.vangriethuijsen@students.uu.nl)

speeddiscretisation<-function(input.data, aisles.log,hits.log, products, params){
  stop.params<- params$features$stops
  walk.params<- params$features$walk
  
  #get all mearged discretisations of the speed
  c.stops<-calc.speed.discretisation(input.data = input.data, merge.dist = stop.params$stop.merge.distance, 
                                   cuttoff = stop.params$stop.max.speed,lowerinequations = TRUE)
  
  #find real stops
  p.stops<- speed.discretisation.merge.induction(c.stops,input.data,  stop.params$stop.merge.distance)
  #add info
  p.stops<-add.times.location(p.stops,input.data)
  
  r.stops<-p.stops%>% filter( relative.dist<=0.5)
  slows<-p.stops%>% filter( relative.dist>0.5)
  
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
  
  #combine to one dataframe
  log<- log %>% arrange(start)
  #add location of stop and slow
  log<- cbind(log, calc.spot.event.in.box(log, params$features$aisles))
  
  #add if it is in a target aisles
  log$target<- log$aisles.name %in% calc.target.aisles(products, params$features$aisles)[,2]
  
  # add stops to ailes log
  aisles.log<- add.stops.to.aisles.log(r.stops, aisles.log)
  
  # add if a product was hit, if so, which(first and second) one and the ammount of hits for the case that there are more
  log<- hit.stop(hits.log,log)
  
  speed.res<-list(speed.log = log,
                  aisles.log= aisles.log
                  )
  
  return(speed.res)
}


  


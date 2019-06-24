# VR Supermarkt: function to extract the stops.
#
# Last edited 31-5-2019 by Chiel van Griethuijsen (m.a.vangriethuijsen@students.uu.nl)

speeddiscretisation<-function(input.data, aisles.log,hits.log, gg,
                              stop.params, walk.params,
                              aisles, i){
  #get all mearged discretisations of the speed
  p.stops<-calc.speed.discretisation(input.data = input.data, merge.dist = stop.params$stop.merge.distance, 
                                   cuttoff = stop.params$stop.max.speed,lowerinequations = TRUE)
  walks<-calc.speed.discretisation(input.data = input.data, merge.dist = walk.params$walk.merge.distance,
                                   cuttoff = walk.params$walk.min.speed,lowerinequations = FALSE)
  
  #add info
  p.stops<-add.times.location(p.stops,input.data)
  walks<-add.times.location(walks,input.data)
  
  #filters
  p.stops<- p.stops %>% filter(time.spend>stop.params$stop.minimum.duration)
  stops.slow<-p.stops%>% filter( absolute.speed<=0.05)
  stops.fast<-p.stops%>% filter( absolute.speed>0.05)
  
  walks<-walks %>% filter(time.spend>walk.params$walk.minimum.duration)
  
  #add labels
  #slows$label<-rep("slow", nrow(slows))
  stops.slow$label<-rep("stop with no movement", nrow(stops.slow))
  stops.fast$label<-rep("stop with some movement", nrow(stops.fast))

  
  walks$label<-rep("walk", nrow(walks))
  
  #combine to one dataframe
  #log<-rbind(slows,stops,walks)
  log<- rbind(stops.slow,stops.fast,walks)
  log<- log %>% arrange(start)
  log<- cbind(log, calc.spot.event.in.box(log, aisles))
  
  # add stops to ailes log
  aisles.log<- add.stops.to.aisles.log(p.stops, aisles.log)
  
  # add if a product was hit, if so, which(first and second) one and the ammount of hits for the case that there are more
  log<- hit.stop(hits.log,log)
  
  speed.res<-list(speed.log = log,
                  aisles.log= aisles.log
                  )
  
  return(speed.res)
}


  


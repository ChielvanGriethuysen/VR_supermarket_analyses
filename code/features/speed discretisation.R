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
  stops.1<-p.stops%>% filter(time.spend>2, time.spend<6, absolute.speed<0.075)
  stops.2<-p.stops%>% filter(time.spend>2, time.spend<6, absolute.speed>0.075)
  stops.3<-p.stops%>% filter(time.spend>6, absolute.speed<0.075)
  stops.4<-p.stops%>% filter(time.spend>6, absolute.speed>0.075)
  
  walks<-walks %>% filter(time.spend>walk.params$walk.minimum.duration)
  
  #add labels
  #slows$label<-rep("slow", nrow(slows))
  stops.1$label<-rep("stop between 2 and 6 sec speed smaller than 0.075", nrow(stops.1))
  stops.2$label<-rep("stop between 2 and 6 sec speed greater than 0.075", nrow(stops.2))
  stops.3$label<-rep("stop greater than 6 sec speed smaller than 0.075", nrow(stops.3))
  stops.4$label<-rep("stop greater than 6 sec speed greater than 0.075", nrow(stops.4))
  
  walks$label<-rep("walk", nrow(walks))
  
  #combine to one dataframe
  #log<-rbind(slows,stops,walks)
  log<- rbind(stops.1,stops.2,stops.3,stops.4,walks)
  log<- log %>% arrange(start)
  log<- cbind(log, calc.spot.event.in.box(log, aisles))
  
  # add stops to ailes log
  aisles.log<- add.stops.to.aisles.log(stops.3, aisles.log)
  
  # add if a product was hit, if so, which(first and second) one and the ammount of hits for the case that there are more
  log<- hit.stop(hits.log,log)
  
  speed.res<-list(speed.log = log,
                  aisles.log= aisles.log
                  )
  
  return(speed.res)
}


  


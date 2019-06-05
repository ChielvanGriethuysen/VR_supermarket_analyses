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
  #slows<-p.stops%>% filter(absolute.speed>0.1, time.spend>3)
  #stops<-p.stops%>% filter(absolute.speed<0.1)
  stops<-p.stops
  walks<-walks %>% filter(time.spend>walk.params$walk.minimum.duration)
  
  #add labels
  #slows$label<-rep("slow", nrow(slows))
  stops$label<-rep("stop", nrow(stops))
  walks$label<-rep("walk", nrow(walks))
  
  #combine to one dataframe
  #log<-rbind(slows,stops,walks)
  log<- rbind(stops,walks)
  log<- log %>% arrange(start)
  log<- cbind(log, calc.spot.event.in.box(log, aisles))
  
  # add stops to ailes log
  aisles.log<- add.stops.to.aisles.log(stops, aisles.log)
  
  # add if a product was hit, if so, which(first and second) one and the ammount of hits for the case that there are more
  log<- hit.stop(hits.log,log)
  
  speed.res<-list(speed.log = log,
                  aisles.log= aisles.log
                  )
  
  return(speed.res)
}

speed.plot<-function(data,log_speed, log_place, start1=0, stop1=nrow(data)){
  
  
  log_speed<- log_speed %>% filter(start>=start1,stop<=stop1)
  log_place<- log_place %>% filter(start>=start1,stop<=stop1)
  

  log_speed$start.time<- log_speed$start.time-data$time[1]
  log_speed$stop.time<- log_speed$stop.time-data$time[1]
  
  log_place$start.time<- log_place$start.time-data$time[1]
  log_place$stop.time<- log_place$stop.time-data$time[1]
  data$time<- data$time-data$time[1]
  
  
  plot<-ggplot()+
    geom_point(data[start1:stop1,], mapping =  aes(time, speed),size=0.75, alpha=0.1)+
    ylim(-.1,0.6)
  
  if(nrow(log_speed)>0){
    plot<-plot+
      geom_rect(log_speed,mapping =  aes(xmin=start.time,xmax=stop.time,ymin=0,ymax=0.6, fill= label),alpha=0.35)
  }
  if(nrow(log_place)>0){
    plot<-plot+
      geom_rect(log_place,mapping =  aes(xmin=start.time,xmax=stop.time,ymin=-0.1,ymax=0, fill= label))+
      geom_label(data = log_place,
                 mapping = aes(x = (start.time+stop.time)/2, y = -0.05, label = log_place$aisles.name),
                 label.size = .32)
  }
  plot
}
  


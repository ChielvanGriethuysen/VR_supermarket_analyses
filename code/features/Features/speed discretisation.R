# VR Supermarkt: function to extract the stops.
#
# Last edited 31-5-2019 by Chiel van Griethuijsen (m.a.vangriethuijsen@students.uu.nl)

speeddiscretisation<-function(input.data,input.look, aisles.log,hits.log, products, params){
  stop.params<- params$features$stops
  
  #get all discretisations of the speed below cuttoff
  c.stops<-calc.speed.discretisation(input.data = input.data, 
                                   cuttoff = stop.params$stop.max.speed,lowerinequations = TRUE)
  #find real stops by removing outliers smaller than merge.time
  p.stops<- speed.discretisation.merge.induction(c.stops,input.data,  stop.params$stop.merge.time.1)
  p.stops$parts<-1
  p.stops<- speed.discretisation.merge.induction(p.stops,input.data,  stop.params$stop.merge.time.2)
  
  #add info
  p.stops<-add.basic.features(p.stops,input.data)
  
  #filters
  log <- p.stops %>% filter(time.spent>stop.params$stop.minimum.duration)
  
  #add features
  log<- add.quality.features(log, input.data)
  log<- add.view.quality.features(log, input.data)
  log<- add.view.area(log,input.data, input.look,params )
  log$id<-if(nrow(log)>0) 1:nrow(log) else numeric()
  
  #add location of stop and slow
  log<- merge(log, calc.spot.event.in.box(data.frame(x=log$x.start, z=log$z.start), params$features$aisles) %>% 
                transmute(aisles.type= params$features$aisles$type[row],
                          aisles.name= params$features$aisles$names[row],
                          id= col), by="id", all= TRUE)
  
  #add if it is in a target aisles
  log$target<- log$aisles.name %in% calc.spot.event.in.box(products, params$features$aisles)[,2]
  
  # add if a product was hit, if so, which(first and second) one and the ammount of hits for the case that there are more
  log<- hit.stop(hits.log,log)
  
  speed.res<-list(log = log
                  )
  
  return(speed.res)
}
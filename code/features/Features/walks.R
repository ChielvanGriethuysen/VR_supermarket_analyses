walks<- function(input.data, input.look, params, stops, products){
  
  walks<-stop.to.walk(stops %>% group_by(id) %>% 
                        summarise(start=first(start), stop= first(stop)) %>% 
                        select(-id),input.data)
  walks<-add.basic.features(walks,input.data)
  walks<-walks %>% filter(absolute.dist>params$features$walk$walk.minimum.dist)
  
  walks<- add.quality.features(walks, input.data)
  walks<- add.view.quality.features(walks, input.data)
  walks<- add.view.area(walks,input.data, input.look,params )
  walks$id<-if(nrow(walks)>0) 1:nrow(walks) else numeric()
  
  #add location of stop and slow
  walks.start<- calc.spot.event.in.box(data.frame(x=walks$x.start, z=walks$z.start), params$features$aisles) %>% 
                transmute(aisles.type.start= params$features$aisles$type[row],
                          aisles.name.start= params$features$aisles$names[row],
                          id= col)
  walks.stop<- calc.spot.event.in.box(data.frame(x=walks$x.stop, z=walks$z.stop), params$features$aisles) %>% 
                  transmute(aisles.type.stop= params$features$aisles$type[row],
                            aisles.name.stop= params$features$aisles$names[row],
                            id= col)
  walks.startstop<- merge(walks.start, walks.stop,by="id")
  walks<- merge(walks, walks.startstop,by= "id", all=TRUE)
  
  walks$target.start<- walks$aisles.name.start %in% calc.spot.event.in.box(products, params$features$aisles)[,2]
  walks$target.stop<- walks$aisles.name.stop %in% calc.spot.event.in.box(products, params$features$aisles)[,2]
  
  return(list(log= walks))
}
# VR Supermarkt: function to extract how much time was spent in different aisles.
#
# Last edited 2019-07-03 by Chiel van Griethuijsen (m.a.vangriethuijsen@students.uu.nl)



getAisleTimes <- function(input.data, input.look, products,
                          params, stops){
  
  
  aisles.data<-calc.box.feature(input.data,params$features$aisles)
  aisles.data<- add.basic.features(aisles.data,input.data)
  aisles.data<- add.quality.features(aisles.data,input.data)
  aisles.data<- add.view.area(aisles.data, input.data,input.look, params)
  aisles.data<- add.view.quality.features(aisles.data, input.data)
  input.data$walk.direction[input.data$stop]<-NA
  walk.angles.summary<- direction.sub.summary.feed(input.data$walk.direction, aisles.data,"walk")
  view.angles.summary<- direction.sub.summary.feed(input.look$angle, aisles.data,"view")
  sway.summary<- sway.summary.feed(input.data,aisles.data)
  aisles.data<- cbind(aisles.data, walk.angles.summary,view.angles.summary,sway.summary)
  aisles.data<-aisles.label.add(aisles.data,input.data,params$features$aisles)
  aisles.data$aisles.name<- params$features$aisles$names[aisles.data$box.id]
  aisles.data$aisles.type<- params$features$aisles$type[aisles.data$box.id]
  aisles.data$target<- aisles.data$aisles.name %in% (calc.spot.event.in.box(products, params$features$aisles) %>% 
                                                       transmute(aislesl.name= params$features$aisles$names[row]))[,1]
  aisles.data<-add.stops.to.aisles.log(stops %>% group_by(id) %>% summarise(start=first(start),
                                                                            stop=first(stop)),aisles.data)

  
  if(nrow(aisles.data)>0){
    #Count number of times in each aisle can be used for output
    # aisles.summarised<-aisles.data %>% group_by(aisles.name) %>%
    #   summarise(time=sum(time.spend),n=n()) %>%
    #   complete(aisles.name,fill = list(time=0,n=0))
    
  } 
  res.aisles <- list(log= aisles.data)
  return(res.aisles)
}




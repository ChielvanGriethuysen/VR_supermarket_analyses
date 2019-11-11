# VR Supermarkt: function to extract how much time was spent in different aisles.
#
# Last edited 2019-07-03 by Chiel van Griethuijsen (m.a.vangriethuijsen@students.uu.nl)



getAisleTimes <- function(input.data, products,
                          aisles){
  
  
  aisles.data<-calc.box.feature(input.data,aisles)
  aisles.data<- add.basic.features(aisles.data,input.data)
  aisles.data<- add.quality.features(aisles.data,input.data)
  aisles.data<- add.view.quality.features(aisles.data, input.data)
  aisles.data<-aisles.label.add(aisles.data,input.data,aisles)
  aisles.data$target<- aisles.data$aisles.name %in% calc.spot.event.in.box(products, params$features$aisles)[,2]
  
  if(nrow(aisles.data)>0){

    
    #Count number of times in each aisle can be used for output
    # aisles.summarised<-aisles.data %>% group_by(aisles.name) %>%
    #   summarise(time=sum(time.spend),n=n()) %>%
    #   complete(aisles.name,fill = list(time=0,n=0))
    
  } 
  res.aisles <- list(log= aisles.data)
  return(res.aisles)
}




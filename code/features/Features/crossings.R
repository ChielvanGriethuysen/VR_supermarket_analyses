# VR Supermarkt: function to extract crossings
#
# Last edited 2019-07-03 by Chiel van Griethuijsen (m.a.vangriethuijsen@students.uu.nl)

getCrossings = function(input.data, params,products){

  
  #skip points, if to close to each other, to speed op crossing finding
  move_data<-skippoints(input.data,distance = 0.05)
  move_data<- speed.dist.add(move_data)
  move_data$angles<- calculate.direction(move_data, find.first.point.on.dist(move_data,1))
  
  crossings <- anyIntersects(move_data$x, move_data$z, move_data$time, params$features$cross$cross.lag1)
  #A crossings consists of 4 time points (a, a+1, b, b+1), both a and b ares saved.
  #Only a is used to calculate n crossings and plot them.
  crossings <- data.frame(t(unname(as.data.frame(crossings))))
  #colnames(crossings) <- c("First Time point", "Second Time point")
  colnames(crossings) <- c("start", "stop")
  if (crossings[1,1]==0) {
    crossings<-crossings[-1,]
  }
  
  #remove crossings if the two points are to close
  if(nrow(crossings)>0){
    crossings<- add.basic.features(crossings, move_data) %>% rename("time.between"="time.spent")
    # apply distance filters, loop size
    crossings<- crossings%>% filter(absolute.dist>params$features$cross$cross.dist1)
    if(nrow(crossings)>1){
      #check if to close to each other
      crossings<- crossings.filter.close(crossings,params$features$cross$cross.dist2,2)
    }
    # add location
    crossings$id<- if(nrow(crossings)>0) 1:nrow(crossings) else numeric()
    crossings<- merge(crossings, calc.spot.event.in.box(data.frame(x= crossings$x.start, z= crossings$z.start), params$features$aisles) %>%
                        transmute(aisles.type= params$features$aisles$type[row],
                                  aisles.name= params$features$aisles$names[row],
                                  id= col), by= "id", all= TRUE)
  }
  else{
    crossings$id<- numeric(0)
    crossings$aisles.type<- numeric(0)
    crossings$aisles.name<-numeric(0)
  }
  #add if crossing is in a target aisles
  crossings$target<- crossings$aisles.name %in% calc.spot.event.in.box(products, params$features$aisles)[,2]
  #calculate angle between two segments, take smalest one so max is 90
  angles<-(move_data$angles[crossings$start]-move_data$angles[crossings$stop])%% 180
  crossings$angle<- mapply(function(a,b) min(c(a,b)), a=angles, b= 180-angles )

  res.cross <- list(log= crossings,
                    cross.points.all=crossings)
  
  return(res.cross)
}

# filters away crossings that are to close to each other
# loop over all crossings and look back if a previous one is to close, if so remove it
crossings.filter.close<- function(crossings, dist, start){
  for(i in start:(nrow(crossings))){
    for(j in (i-1):1) #look back
      if(dist(crossings[c(i,j),] %>% select(x.start, z.start))<dist){
        crossings<- crossings[-i,]
        if(nrow(crossings)>=i){
          return(crossings.filter.close(crossings, dist, i))
        }
        else{
          return(crossings)
        }
      }
  }
  return(crossings)
}

# VR Supermarkt: function to extract crossings
#
# Last edited 2019-07-03 by Chiel van Griethuijsen (m.a.vangriethuijsen@students.uu.nl)

getCrossings = function(input.data, params,products){

  
  #skip points, if to close to each other, to speed op crossing finding
  move_data<-skippoints(input.data,distance = 0.05)
  move_data<- speed.dist.add(move_data)
  FootPosition<- move_data[,2:4]
  time<- move_data[,1]
  
  crossings <- anyIntersects(FootPosition$x, -FootPosition$z, time, params$features$cross$cross.lag1)
  #A crossings consists of 4 time points (a, a+1, b, b+1), both a and b ares saved.
  #Only a is used to calculate n crossings and plot them.
  #b can be used in the future to calculate the angle.
  crossings <- data.frame(t(unname(as.data.frame(crossings))))
  #colnames(crossings) <- c("First Time point", "Second Time point")
  colnames(crossings) <- c("start", "stop")
  if (crossings[1,1]==0) {
    crossings<-crossings[-1,]
  }
  
  
  #remove crossings if the two points are to close
  if(nrow(crossings)>0){
    r<-c()
    #remove crossings if the next one is to close in time
    k<-1
    while (k<nrow(crossings)) {
      l<-k+1
      while (l<=nrow(crossings) &&
                    time[crossings[l,1]]-time[crossings[k,1]]<params$features$cross$cross.lag2) {
        l<-l+1
      }
      r<-c(r,k)
      k<-l
    }
    # add last element, can't be to close to the next one, but only if it is not to close to a earlyer one
    if(k== nrow(crossings)){
      r<-c(r,k)
    }
    
    # crossings<-crossings[r,]
    crossings<- add.times.location(crossings, move_data)
    colnames(crossings)[5]<-"time.between"
    crossings<- cbind(crossings, calc.spot.event.in.box(crossings, params$features$aisles))
    
    crossings<- crossings%>% filter(absolute.dist>params$features$cross$cross.dist1)
    if(nrow(crossings)>1){
      crossings<- crossings.filter.close(crossings,params$features$cross$cross.dist2,2)
    }
    
    n.crossings<- crossings %>% filter(aisles.type!= "none") %>% nrow()
    n.crossings.shopping<- crossings %>% filter(aisles.type== "shopping") %>%nrow()
  }
  else{
    n.crossings<- 0
    n.crossings.shopping<- 0
  }
  
  crossings$target<- crossings$aisles.name %in% calc.target.aisles(products, params$features$aisles)[,2]

  res.cross <- list(log= crossings,
                    n.crossings = n.crossings,
                    cross.points.all=crossings)
  
  return(res.cross)
}

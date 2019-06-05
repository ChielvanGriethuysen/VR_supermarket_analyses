# VR Supermarkt: function to extract how much time was spent in different aisles.
#
# Last edited 2018-10-18 by Laurent Smeets (l.s.m.smeets@uu.nl)



getAisleTimes <- function(input.data,
                          aisles, full.images, save.data, i){
  
  
  aisles.data<-calc.box.feature(input.data,aisles)
  aisles.data<-aisles.label.add(aisles.data,input.data,aisles)
  
  # data[i,]$n.walked.through.aisles<-nrow(walk.through)
  # data[i,]$n.walked.in.out.aisles<-nrow(same.side.in.out)
  
  if(nrow(aisles.data)>0){

    
    #Count number of times in each aisle
    #data[i, 4:17] <- table(times.through.aisle$aisle.names) 
    aisles.summarised<-aisles.data %>% group_by(aisles.name) %>%
      summarise(time=sum(time.spend),n=n()) %>%
      complete(aisles.name,fill = list(time=0,n=0))
    
    # data[i,]<-mutate(data[i,],
    #                  n.aisle.1A= aisles.summarised$n[1],
    #                  n.aisle.2A= aisles.summarised$n[2],
    #                  n.aisle.3A= aisles.summarised$n[3],
    #                  n.aisle.4A= aisles.summarised$n[4],
    #                  n.aisle.5A= aisles.summarised$n[5],
    #                  n.aisle.1B= aisles.summarised$n[6],
    #                  n.aisle.2B= aisles.summarised$n[7],
    #                  n.aisle.3B= aisles.summarised$n[8],
    #                  n.aisle.4B= aisles.summarised$n[9],
    #                  n.aisle.5B= aisles.summarised$n[10],
    #                  n.aisle.6B= aisles.summarised$n[11],
    #                  n.aisle.M1= aisles.summarised$n[12],
    #                  n.aisle.M2= aisles.summarised$n[13],
    #                  n.aisle.M3= aisles.summarised$n[14])
    # 
    # data[i,]<-mutate(data[i,],
    #                  time.aisle.1A= aisles.summarised$time[1],
    #                  time.aisle.2A= aisles.summarised$time[2],
    #                  time.aisle.3A= aisles.summarised$time[3],
    #                  time.aisle.4A= aisles.summarised$time[4],
    #                  time.aisle.5A= aisles.summarised$time[5],
    #                  time.aisle.1B= aisles.summarised$time[6],
    #                  time.aisle.2B= aisles.summarised$time[7],
    #                  time.aisle.3B= aisles.summarised$time[8],
    #                  time.aisle.4B= aisles.summarised$time[9],
    #                  time.aisle.5B= aisles.summarised$time[10],
    #                  time.aisle.6B= aisles.summarised$time[11],
    #                  time.aisle.M1= aisles.summarised$time[12],
    #                  time.aisle.M2= aisles.summarised$time[13],
    #                  time.aisle.M3= aisles.summarised$time[14])
    
    
  } 
  # check, per coordinate whether they are in an aisle or not.
  a.shoppingaisles <- apply(input.data[, c(2, 4)], 1, box.check, box.list = filter(aisles, type=="shopping"))
  t.shoppingaisles <- which(a.shoppingaisles, arr.ind = TRUE)
  shopping.aisle.time.points <- t.shoppingaisles[, 2]
  
  
  
  res.aisles <- list(log= aisles.data,
                     shopping.aisle.time.points=shopping.aisle.time.points)
  return(res.aisles)
}




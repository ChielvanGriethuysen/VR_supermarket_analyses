# VR Supermarkt: function to extract the stops.
#
# Last edited 2018-10-18 by Laurent Smeets (l.s.m.smeets@uu.nl)

getStops <- function(data, input.data, gg,
                     stop.time, stop.radius, productsbox,
                     save.data, full.images, i){
  
  FootPosition<-input.data[,2:4]
  time<-input.data[,1]
  
  stop<-speedfeature(input.data,stop.time,stop.radius)
  
  stop.points<- expandstops(stop, nrow(input.data))

  stop<- add.times(stop,input.data)
  
  
  
  
  #####
  # stop.begin.stop<-stop$begin
  # stop.end.stop<-stop$end
  
  
  # frame1<-tibble(stop.end.stop=stop.end.stop,obs=1:length(stop.end.stop), time.beginning=time) %>%
  #   filter(stop.end.stop!=0)
  # 
  # frame2<-tibble(time.end=time, observation=1:length(time))%>%
  #   filter(observation%in%frame1$stop.end.stop)%>%
  #   bind_cols(frame1) %>%
  #   select(-observation) %>%
  #   mutate(stopduration=time.end-time.beginning)
  # 
  # data$total.stoping.time[i]<-sum(frame2$stopduration)
  
  #####
  
  n.stops <- nrow(stop)
  stop.pos <- FootPosition[stop$begin, ]
  
  
  a <- apply(stop.pos[, c(1, 3)], 1, Checkstopbeforeitem, productsbox = productsbox)
  t <- which(a, arr.ind = TRUE)
  n.stops.before.item<-nrow(t)
  
  stops.elsewhere<- n.stops-n.stops.before.item
  
  data$n.stops[i] <- n.stops
  
  if(any(n.stops.before.item)){
  data$n.stops.item[i] <- n.stops.before.item
  }
  else{
    data$n.stops.item[i] <- 0
  }
  if(any(stops.elsewhere)){
    data$n.stops.elsewhere[i]<- stops.elsewhere
  }
  else{
    data$n.stops.elsewhere[i]<- 0
  }
  
  
  
  
  #split slows per third.
  
  
  stoppointstibble<-tibble(stoppoints=stop$begin)
  split1<-which.min(abs(time - last(time)/3)) 
  split2<-which.min(abs(time - (last(time)/3*2)))
  
  stops.1st.1.3rd<-nrow(filter(stoppointstibble, stoppoints<split1))
  stops.2nd.1.3rd<-nrow(filter(stoppointstibble, stoppoints>split1 & stoppoints<split2))
  stops.3rd.1.3rd<-nrow(filter(stoppointstibble, stoppoints>split2))

  data$stops.1st.1.3rd[i]<-stops.1st.1.3rd
  data$stops.2nd.1.3rd[i]<-stops.2nd.1.3rd
  data$stops.3rd.1.3rd[i]<-stops.3rd.1.3rd

  
  
  res <- list()
  
  
  gg.stops <- gg
  
  if(full.images){
    if(n.stops > 0){
      gg.stops <- gg.stops +
        geom_circle(data = stop.pos, 
                    mapping = aes(x0 = x, y0 = -z, 
                                  r = stop.radius), 
                    fill = 'green', colour = 'green')
    }
    
    gg.stops <- gg.stops +
      geom_text(aes(y = -48, x = 2, 
                    label = paste("N stops = ", n.stops, "(",n.stops.before.item, ")" )),
                size = 5,
                colour = 'green')
  }
  
  res.stops <- list(gg.stops = gg.stops,
                    data = data,
                    stop.log = stop,
                    stop.points=stop.points,
                    stop.pos=stop.pos,
                    n.stops = n.stops,
                    n.stops.before.item=n.stops.before.item)
  
  return(res.stops)
}
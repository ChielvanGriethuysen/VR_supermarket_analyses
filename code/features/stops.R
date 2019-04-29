# VR Supermarkt: function to extract the stops.
#
# Last edited 2018-10-18 by Laurent Smeets (l.s.m.smeets@uu.nl)

getStops <- function(data, input.data, gg,
                     stop.time, stop.radius, productsbox,
                     save.data, full.images, i){
  
  FootPosition<-input.data[,2:4]
  time<-input.data[,1]
  
  final.step <- first(which(time > max(time) - stop.time)) - 1
  
  stop <- data.frame(begin.stop = rep(FALSE, length(time)), end.stop = numeric(length(time)))

  #calculate distance between two points for every point
  stop.radius.squared<-stop.radius^2
  
  x.change <- diff(FootPosition$x, 1)
  y.change <- diff(FootPosition$z, 1) 
  distance.between.points<-sqrt(x.change^2 + y.change^2)
  
  k <- dis <- final.step.in.stop <- 0
  for(s in 1 : final.step){
    if(s %% 1000 == 0){
      print(paste0('Calculating STOP of time point ', s, ' of ', final.step, 
                   ' of file ', i))
    }
    
    if(final.step.in.stop > s){ #Don't check points that are already part of previous stop
      next
    }
    
    dis <- 0
    k <- s
    #skip points that can't be on the border of the stop.radius, skips distance calculations for uninteresting points
    helpDis<-0
    while (helpDis<stop.radius&& k < length(time)) {
      helpDis<-helpDis+distance.between.points[k]
      k<-k+1
    }
    dis <- (FootPosition$x[s] - FootPosition$x[k]) ^ 2 +
      (FootPosition$z[s] - FootPosition$z[k]) ^ 2
    
    #look at the points that are byond stop.radius walking distance
    while(dis < stop.radius.squared && k < length(time)){
      k <- k + 1
      dis <- (FootPosition$x[s] - FootPosition$x[k]) ^ 2 +
                    (FootPosition$z[s] - FootPosition$z[k]) ^ 2
    }
    # if point on distance stop.radius and time more than stop.time than it is a stop
    if(time[k - 1] - time[s] > stop.time){
      stop[s : (k - 1), 1] <- TRUE
      stop[s, 2] <- final.step.in.stop <- k - 1
    }
  }
  
  #####
  stop.begin.stop<-stop$begin.stop
  stop.end.stop<-stop$end.stop
  
  
  frame1<-tibble(stop.end.stop=stop.end.stop,obs=1:length(stop.end.stop), time.beginning=time) %>%
    filter(stop.end.stop!=0)
  
  frame2<-tibble(time.end=time, observation=1:length(time))%>%
    filter(observation%in%frame1$stop.end.stop)%>%
    bind_cols(frame1) %>%
    select(-observation) %>%
    mutate(stopduration=time.end-time.beginning)
  
  data$total.stoping.time[i]<-sum(frame2$stopduration)
  
  #####
  
  
  stop.points <- stop[, 1]
  
  stop.start.points <- which(stop[, 2] != 0)
  n.stops <- length(stop.start.points)
  stop.pos <- FootPosition[stop.start.points, ]
  
  Checkstopbeforeitem <- function(position, productsbox){
    position[1] > productsbox$xmin & position[1] < productsbox$xmax &
      -position[2] > productsbox$zmin & -position[2] < productsbox$zmax
  }
  
  
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
  
  
  stoppointstibble<-tibble(stoppoints=stop.start.points)
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
                    stop.points = list(stop.points),
                    data = data,
                    n.stops = n.stops,
                    stop.points=stop.points,
                    stop.pos=stop.pos,
                    n.stops.before.item=n.stops.before.item)
  
  return(res.stops)
}
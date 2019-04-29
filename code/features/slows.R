# VR Supermarkt: function to extract the slows.
#
# Last edited 2018-10-18 by Laurent Smeets (l.s.m.smeets@uu.nl)


getSlows <- function(data, input.data, gg,
                     stop.points, slow.time, slow.radius, producttimepoint.time.points,
                     full.images, save.data, i){
  time<- input.data[,1]
  FootPosition<-input.data[,2:4]
  
  
  final.step <- first(which(time > max(time) - slow.time)) - 1
  slow <- data.frame(begin.slow = rep(FALSE, length(time)), end.slow = numeric(length(time)))
  
  k <- dis <- final.step.in.slow <- 0
  #calculate distance between two points for every point
  slow.radius.squared<-slow.radius^2 
  x.change <- diff(FootPosition$x, 1) 
  y.change <- diff(FootPosition$z, 1)  
  distance.between.points<-sqrt(x.change^2 + y.change^2)
  
  for(b in 1 : final.step){
    if(b %% 1000 == 0){
      print(paste0('Calculating SLOW of time point ', b, ' of ', final.step, 
                   ' of file ', i))  }  
    
    if(final.step.in.slow > b || b %in% stop.points){ #Don't check points that are already part of previous slow or a stop
      next
    }
    
    dis <- 0
    k <- b
    #skip points that can't be on the border of the stop.radius, skips distance calculations for uninteresting points
    helpDis<-0
    while (helpDis<sqrt(slow.radius)&& !stop.points[k]&& k < length(time)) {
      helpDis<-helpDis+distance.between.points[k]
      k<-k+1
    }
    dis <- (FootPosition$x[b] - FootPosition$x[k]) ^ 2 +
      (FootPosition$z[b] - FootPosition$z[k]) ^ 2
    #look at the points that are byond stop.radius walking distance
    while(dis < slow.radius.squared && !stop.points[k] && k < length(time)){
      k <- k + 1
      dis <- (FootPosition$x[b] - FootPosition$x[k]) ^ 2 +
                    (FootPosition$z[b] - FootPosition$z[k]) ^ 2
    }
    # if point on distance stop.radius and time more than stop.time than it is a stop
    if(time[k - 1] - time[b] > slow.time) {
      slow[b, 1] <- TRUE
      slow[b, 2] <- final.step.in.slow <- k
    }
    
  }
  
  slow.points<-which(slow[, 1])
  slow.points2<-as.data.frame(slow.points)

  n.slows <- length(slow.points)
  n.slows.before.item<-sum(slow.points  %in% producttimepoint.time.points)
  slows.elsewhere <- n.slows-n.slows.before.item
  
  
  
  slow.begin.slow<-slow$begin.slow
  slow.end.slow<-slow$end.slow
  frame<-tibble(slow.end.slow=slow.end.slow,obs=1:length(slow.end.slow)) %>%
    filter(slow.end.slow!=0)
  if(nrow(frame)==0){
    n.slows.before.item2 <- 0
    slows.elsewhere2 <- 0
  }else{
    listslowpoint<-list()
    for(g in 1:nrow(frame)){
      listslowpoint[[g]]<-as.numeric(frame[g,2]):as.numeric(frame[g,1])
    }
    sumslowpoints<-c()  
    for(w in 1:length(listslowpoint)){
      sumslowpoints[w]<-sum(listslowpoint[[w]] %in%producttimepoint.time.points)
    }
    n.slows.before.item2<-sum(sumslowpoints!=0)
    slows.elsewhere2 <- n.slows-n.slows.before.item2
  }
  
  
  ## the .2 version check whether a slow is during any point near the product
  ## the other version only checks if the slow starts before a product
  
  data$n.slows[i] <- n.slows
  data$n.slows.item[i] <- n.slows.before.item
  data$n.slows.elsewhere[i]<- slows.elsewhere
  data$n.slows.item2[i] <- n.slows.before.item2
  data$n.slows.elsewhere2[i]<- slows.elsewhere2
  
  
  
  #split slows per third.
  
  slowpointstibble<-tibble(slowpoints=slow.points)
  split1<-which.min(abs(time - last(time)/3)) 
  split2<-which.min(abs(time - (last(time)/3*2)))
  
  slows.1st.1.3rd<-nrow(filter(slowpointstibble, slowpoints<split1))
  slows.2nd.1.3rd<-nrow(filter(slowpointstibble, slowpoints>split1 & slowpoints<split2))
  slows.3rd.1.3rd<-nrow(filter(slowpointstibble, slowpoints>split2))
  
  slowpointstibblebeforeitem<-tibble(slowpoints=producttimepoint.time.points[which(producttimepoint.time.points %in%slow.points)])
  slows.1st.1.3rd.items<-nrow(filter(slowpointstibblebeforeitem, slowpoints<split1))
  slows.2nd.1.3rd.items<-nrow(filter(slowpointstibblebeforeitem, slowpoints>split1 & slowpoints<split2))
  slows.3rd.1.3rd.items<-nrow(filter(slowpointstibblebeforeitem, slowpoints>split2))
  
  data$slows.1st.1.3rd[i]<-slows.1st.1.3rd
  data$slows.2nd.1.3rd[i]<-slows.2nd.1.3rd
  data$slows.3rd.1.3rd[i]<-slows.3rd.1.3rd
  data$slows.1st.1.3rd.items[i]<-slows.1st.1.3rd.items
  data$slows.2nd.1.3rd.items[i]<-slows.2nd.1.3rd.items
  data$slows.3rd.1.3rd.items[i]<-slows.3rd.1.3rd.items

  
  ## calculate total slowing time
  
  
  frame1<-tibble(slow.end.slow=slow.end.slow,obs=1:length(slow.end.slow), time.beginning=time) %>%
    filter(slow.end.slow!=0)
  
  frame2<-tibble(time.end=time, observation=1:length(time))%>%
    filter(observation%in%frame1$slow.end.slow)%>%
    bind_cols(frame1) %>%
    select(-observation) %>%
    mutate(slowduration=time.end-time.beginning)
  
  data$total.slowing.time[i]<-sum(frame2$slowduration)
  
  gg.slows <- gg
  
  if(full.images){
    if(any(slow[, 1])){
      for(s in which(slow[, 1])){
        slows.df <- FootPosition[c(s : slow[s, 2]), ]
        gg.slows$layers <- append(gg.slows$layers,
                                  geom_encircle(data = slows.df,
                                                mapping = aes(x = x, y = -z) , s_shape = .5, 
                                                expand = .015, fill = 'white'),
                                  after = 5)
      }
      gg.slows <- gg.slows + geom_text(aes(y = -48, x = 3, 
                                           label = paste("N slows = ", n.slows, "(", n.slows.before.item2, ")")),
                                       colour = 'white', size = 5)
    } else {
      gg.slows <- gg.slows + geom_text(aes(y = -48, x = 3, 
                                           label = paste("N slows = ", 0)),
                                       colour = 'white', size = 5)
    }
    
    
    gg.slows <- gg.slows +
      theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),legend.position="none",
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())
  }
  
  res <- list(gg.slows = gg.slows)
  
  if(save.data){
    res <- c(res,
             data = list(data),
             n.slows = n.slows,
             n.slows.before.item=n.slows.before.item,
             n.slows.before.item2=n.slows.before.item2,
             slow.points=slow.points,
             slow.points2=slow.points2,
             slow=slow,
             final.step=final.step,
             frame2=frame2)
  }
  
  return(res)
}

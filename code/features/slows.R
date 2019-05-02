# VR Supermarkt: function to extract the slows.
#
# Last edited 2018-10-18 by Laurent Smeets (l.s.m.smeets@uu.nl)


getSlows <- function(data, input.data, gg,
                     stop.points, slow.time, slow.radius, producttimepoint.time.points,
                     full.images, save.data, i){
  time<- input.data[,1]
  FootPosition<-input.data[,2:4]
  
  slow<- speedfeature(input.data,slow.time,slow.radius,stop.points)
  slow<- add.times(slow,input.data)
  
  slow.points<-slow$begin
  slow.points2<-as.data.frame(slow.points)

  n.slows <- nrow(slow)
  n.slows.before.item<-sum(slow.points  %in% producttimepoint.time.points)
  slows.elsewhere <- n.slows-n.slows.before.item
  
  
  if(nrow(slow)==0){
    n.slows.before.item2 <- 0
    slows.elsewhere2 <- 0
  }else{
    listslowpoint<-list()
    for(g in 1:nrow(slow)){
      listslowpoint[[g]]<-as.numeric(slow$begin[g]):as.numeric(slow$end[g])
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
  
  data$total.slowing.time[i]<-sum(slow$time.spend)
  
  gg.slows <- gg
  
  if(full.images){
    if(n.slows>0){
      for(s in 1:nrow(slow)){
        slows.df <- FootPosition[c(slow$begin[s] : slow$end[s]), ]
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
             final.step=final.step)#,
             #frame2=frame2)
  }
  
  return(res)
}

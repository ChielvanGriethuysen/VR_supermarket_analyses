#plotting functions, for making nice plots

basic.path.plot<- function(input.data, JSONfile, save=FALSE){
  gg<-ggplot() + 
    #ylim(-53, -7) + xlim(0, 29) +
    scale_x_continuous(limits = c(0, 30), expand=c(0,0)) +
    scale_y_continuous(limits = c(-50, -2.5), expand=c(0,0)) +
    annotation_custom(rasterGrob(image, 
                                 width = unit(1, "npc"), 
                                 height = unit(1, "npc")), 
                      -Inf, Inf, -Inf, Inf) +
    #    coord_fixed() + 
    coord_flip() +
    theme(legend.position = "none",
          plot.margin = margin(0, 0, 0, 0, "cm")) +
    geom_label(aes(y = -5, x = 28, 
                  label = paste("Person", 
                                substr(JSONfile, 1, 5))), 
              size = 5) + 
    geom_hline(yintercept = -45.5)+ 
    geom_path(data = input.data, 
                        mapping = aes(x = x, y = -z, color = 1:length(x)),
                        arrow = arrow(length = unit(4, "points")))
  
  if(save){
    if( ! file.exists(paste0('output/png/basic'))){
      dir.create(paste0('output/png/basic'))
    }
    ggsave(paste0('output/png/basic/',
                  JSONfile, '_RAW.png'), gg, 
           width = 37.5, height = 21, units = 'cm')
  }
  return(gg)
}
full.plot<- function(gg.basic,input.data, logs, JSONfile,products, productbox,aisles, save=FALSE){
  
  cols<-c("main"= "#00BFC4","shopping"= "#F8766D", "TRUE"="lightgreen", "FALSE"="red")
  
  if(data$total.time[i]>180){
    productbox<-productbox
  } else{
    productbox<-filter(productbox, announced != TRUE)
  }
  
  if(data$total.time[i]>180){
    products<-products
  } else{
    products<-filter(products, announced != TRUE)
  }
  discretised.path<-datapoint.add.label(input.data,logs$speed.log)
  info.box<-data.frame(x1=0.1,x2= 2.25,y1=-49.9,y2=-20)
  gg<- gg.basic+
    geom_rect(data= info.box, mapping = aes(xmin=x1, xmax=x2,
                            ymin= y1, ymax=y2),fill="gray80",alpha= 0.7)+
  
    #add aiisles
    geom_rect(data = aisles,
              mapping = aes(xmin = xmin, xmax = xmax,
                            ymin = zmin, ymax = zmax,
                            fill=factor(type)), 
              alpha = .3)+
    geom_label(data = aisles,
               mapping = aes(x = xmin, y = zmin, label = aisles$aisle.names),
               label.size = .32)+  
    #add productbox
    geom_rect(data = productbox,
                                             mapping = aes(xmin = xmin, xmax = xmax,
                                                           ymin = zmin, ymax = zmax
                                             ),
                                             fill= "yellow",
                                             alpha = .2)+
    #add product dot
    geom_point(data = products, 
               mapping = aes(x = x, y = z, 
                             fill= factor(products$productnumber %in% logs$products.hit.log$name.product)),
               color= "red", size= 8, shape=21)+
    scale_fill_manual(values= cols)+
    geom_text(data = products, 
              mapping = aes(x = x, y = z, 
                            label = products$productnumber),
              color= "black")+
    # geom_point(data = products, 
    #            mapping = aes(x = x+.7, y = z+.3),
    #            colour= "white", size=4)+ 
    #add speed
    geom_point(data = discretised.path[discretised.path$label=="stop with no movement",], 
                       mapping = aes(x = x, y = -z),
                       fill = 'tomato', colour = 'tomato', size= 3.5)+
    geom_text(aes(y = -43, x = 0.5, 
                  label = paste("N Stops no movement = ", nrow(logs$speed.log %>% filter(label== "stop with no movement")), "(X)" )),
              size = 5,
              colour = 'tomato')+ 
    geom_point(data = discretised.path[discretised.path$label=="stop with some movement",], 
                     mapping = aes(x = x, y = -z),
                     fill = 'darkorchid', colour = 'darkorchid', size= 3.5)+
    geom_text(aes(y = -43, x = 1.5, 
                  label = paste("N Stops some movement = ", nrow(logs$speed.log %>% filter(label== "stop with some movement")), "(X)")),
              colour = 'darkorchid', size = 5)
  
    #add crossings
    if(nrow(logs$crossings.log)){
    gg<-gg+geom_point(data = logs$crossings.log,
               aes(x = x.start, y = -z.start), size= 3.5,
               col = 'blue')
    }
  
    gg<-gg+geom_text(aes(y = -15, x = 0.5, 
                  label = paste("N crossings =", nrow(logs$crossings.log), "(", logs$crossings.log %>% filter(aisles.type== "shopping") %>%nrow(), ")" )),
              colour = 'blue',size=5)
  if(save){
    if( ! file.exists(paste0('output/png/full'))){
      dir.create(paste0('output/png/full'))
    }
    ggsave(paste0('output/png/full/', JSONfile, '.png'), 
           gg, width = 37.5, height = 21, units = 'cm')
  }
  return(gg)
}

speed.map.combine<- function(speed, map, JSONfile,save=FALSE){
  
  gg<-ggarrange(map,speed, ncol = 1,nrow = 2,heights = c(2,1))
  
  if(save){
    if( ! file.exists(paste0('output/png/speed.map'))){
      dir.create(paste0('output/png/speed.map'))
    }
    ggsave(paste0('output/png/speed.map/', JSONfile, '.png'), 
           gg, width = 37.5, height = 21, units = 'cm')
  }
}

speed.plot<-function(data,log_speed, log_place, start1=0, stop1=nrow(data)){
  
  cols<-c("stop with no movement"= "tomato",
          "stop with some movement"= "darkorchid", 
          "walk"= "darkgoldenrod1",
          "main"="#00BFC4",
          "walk through"="chocolate1",
          "same side in out"= "coral1"
          )
  
  log_speed<- log_speed %>% filter(start>=start1,stop<=stop1, label!="walk")
  log_place<- log_place %>% filter(start>=start1,stop<=stop1)
  
  
  log_speed$start.time<- log_speed$start.time-data$time[1]
  log_speed$stop.time<- log_speed$stop.time-data$time[1]
  
  log_place$start.time<- log_place$start.time-data$time[1]
  log_place$stop.time<- log_place$stop.time-data$time[1]
  data$time<- data$time-data$time[1]
  
  
  plot<-ggplot()+
    geom_point(data[start1:stop1,], mapping =  aes(time, speed),size=0.75, alpha=0.1)+
    ylim(-.1,0.6)
  
  if(nrow(log_speed)>0){
    plot<-plot+
      geom_rect(log_speed,mapping =  aes(xmin=start.time,xmax=stop.time,ymin=0,ymax=0.6, fill= label),alpha=0.35)+
      scale_fill_manual(values= cols)
  }
  if(nrow(log_place)>0){
    plot<-plot+
      geom_rect(log_place,mapping =  aes(xmin=start.time,xmax=stop.time,ymin=-0.1,ymax=0, fill= label),alpha = .3)+
      geom_label(data = log_place,
                 mapping = aes(x = (start.time+stop.time)/2, y = -0.05, label = log_place$aisles.name),
                 label.size = .32)
  }
  plot
}








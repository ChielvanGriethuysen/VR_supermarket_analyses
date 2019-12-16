# plotting functions, for making nice plots
#
# Last edited 2019-07-03 by Chiel van Griethuijsen (m.a.vangriethuijsen@students.uu.nl)
empty.map.plot<- function(){
  ggplot() + 
    #ylim(-53, -7) + xlim(0, 29) +
    scale_x_continuous(limits = c(0, 30), expand=c(0,0)) +
    scale_y_continuous(limits = c(2.5,50), expand=c(0,0)) +
    annotation_custom(rasterGrob(image, 
                                 width = unit(1, "npc"), 
                                 height = unit(1, "npc")), 
                      -Inf, Inf, -Inf, Inf) +
    #    coord_fixed() + 
    coord_flip() +
    ylab("z")+
    theme(legend.position = "none",
          plot.margin = margin(0, 0, 0, 0, "cm"))+
    geom_hline(yintercept = 45.5)
}

basic.path.plot<- function(input.data, id, params,save=FALSE){
  gg<-ggplot() + 
    #ylim(-53, -7) + xlim(0, 29) +
    scale_x_continuous(limits = c(0, 30), expand=c(0,0)) +
    scale_y_continuous(limits = c(2.5,50), expand=c(0,0)) +
    annotation_custom(rasterGrob(image, 
                                 width = unit(1, "npc"), 
                                 height = unit(1, "npc")), 
                      -Inf, Inf, -Inf, Inf) +
    #    coord_fixed() + 
    coord_flip() +
    ylab("z")+
    theme(legend.position = "none",
          plot.margin = margin(0, 0, 0, 0, "cm")) +
    geom_label(aes(y = 5, x = 28, 
                  label = paste("Person", 
                                 id)), 
              size = 5) + 
    geom_hline(yintercept = 45.5)+ 
    geom_path(data = input.data, 
                        mapping = aes(x = x, y = z, color = 1:length(x)),
                        arrow = arrow(length = unit(4, "points")))
  
  if(save){
    if( ! file.exists(paste0('output/',params$output.dir,'/png/basic'))){
      dir.create(paste0('output/',params$output.dir,'/png/basic'),recursive = TRUE)
    }
    ggsave(paste0('output/',params$output.dir,'/png/basic/',
                  id, '_RAW.png'), gg, 
           width = 37.5, height = 21, units = 'cm')
  }
  return(gg)
}
full.plot<- function(gg.basic,input.data, logs, id,params ,products, productbox,aisles, save=FALSE){
  
  cols<-c("main"= "#00BFC4","shopping"= "#F8766D", "TRUE"="lightgreen", "FALSE"="red")
  
    # discretised.path<-datapoint.add.label(input.data,logs$speed.log)
  info.box<-data.frame(x1=0.1,x2= 2.25,y1=49.9,y2=20)
  gg<- gg.basic+
    # geom_rect(data= info.box, mapping = aes(xmin=x1, xmax=x2,
    #                         ymin= y1, ymax=y2),fill="gray80",alpha= 0.7)+
  
    #add aisles
    geom_rect(data = aisles,
              mapping = aes(xmin = xmin, xmax = xmax,
                            ymin = zmin, ymax = zmax,
                            fill=factor(type)), 
              alpha = .3)+
    geom_label(data = aisles,
               mapping = aes(x = xmin, y = zmin, label = aisles$names),
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
                             fill= factor(products$productnumber %in% logs$products.hit.log$productnumber)),
               color= "red", size= 8, shape=21)+
    scale_fill_manual(values= cols)+
    geom_text(data = products, 
              mapping = aes(x = x, y = z, 
                            label = products$productnumber),
              color= "black")+
    #add product order
    geom_point(data = products %>% slice(logs$products.hit.log$prod_id),
               mapping = aes(x = x+.7, y = z+.3),
               colour= "white", size=4)
    if(nrow(logs$products.hit.log)>0){
    gg<- gg+geom_text(data= products %>% slice(logs$products.hit.log$prod_id),
              mapping = aes(x=x+.7,y=z+.3,
                            label=  1:nrow(logs$products.hit.log)),
              colour= "black")
    }
    #add stops to pick product
    if(nrow(logs$stops.log)>0){
    gg<-gg+geom_point(data = log.subset(input.data,logs$stops.log) ,
                       mapping = aes(x = x, y = z),
                       fill = 'tomato', colour = 'tomato', size= 4)+
    geom_text(data = logs$stops.log%>% filter(n_hit>0),
                mapping = aes(x = x.start, y = z.start),
                label="H", colour = 'black', size= 3)
    }
    gg<- gg+geom_text(aes(y = 45.5, x = 27.5,
                  label = paste("N Stops = ", length(logs$stops.log$id %>% unique()), 
                                "(",logs$stops.log %>% filter(aisles.type=="shopping") %>% nrow(),")")),
              size = 5,
              colour = 'tomato')
    # geom_point(data = discretised.path[discretised.path$label=="stop with some movement",], 
    #                  mapping = aes(x = x, y = -z),
    #                  fill = 'darkorchid', colour = 'darkorchid', size= 3.5)+
    # geom_text(aes(y = -43, x = 1.5, 
    #               label = paste("N Stops some movement = ", nrow(logs$speed.log %>% filter(label== "stop with some movement")), "(X)")),
    #           colour = 'darkorchid', size = 5)
  
    #add crossings
    if(nrow(logs$crossings.log)){
    gg<-gg+geom_point(data = logs$crossings.log,
               aes(x = x.start, y = z.start), size= 3.5,
               col = 'blue') +
      geom_text(data=logs$crossings.log, mapping = aes(x=x.start-.5,y=z.start,label= paste0(round(angle,2),"°")))
    }
  
    gg<-gg+geom_text(aes(y = 45.5, x = 29, 
                  label = paste("N crossings =", length(logs$crossings.log$id %>% unique()), 
                                "(", logs$crossings.log %>% filter(aisles.type== "shopping") %>%nrow(), ")" )),
              colour = 'blue',size=5)
  if(save){
    if( ! file.exists(paste0('output/',params$output.dir,'/png/full'))){
      dir.create(paste0('output/',params$output.dir,'/png/full'))
    }
    ggsave(paste0('output/',params$output.dir,'/png/full/', id, '.png'), 
           gg, width = 37.5, height = 21, units = 'cm')
  }
  return(gg)
}

speed.map.combine<- function(speed, map, id,params,save=FALSE){
  
  gg<-ggarrange(map,speed, ncol = 1,nrow = 2,heights = c(2,1))
  
  if(save){
    if( ! file.exists(paste0('output/',params$output.dir,'/png/speed.map'))){
      dir.create(paste0('output/',params$output.dir,'/png/speed.map'))
    }
    ggsave(paste0('output/',params$output.dir,'/png/speed.map/', id, '.png'), 
           gg, width = 37.5, height = 21, units = 'cm')
  }
}

speed.plot<-function(data,log_speed, log_place, start1=0, stop1=nrow(data),id, save= FALSE){
  
  cols<-c("stop with no movement"= "tomato",
          "stop with some movement"= "darkorchid", 
          "walk"= "darkgoldenrod1",
          "main"="#00BFC4",
          "walk through"="chocolate1",
          "same side in out"= "coral1"
          )
  
  log_speed<- log_speed %>% filter(start>=start1,stop<=stop1)
  log_place<- log_place %>% filter(start>=start1,stop<=stop1)
  
  
  # log_speed$start.time<- log_speed$start.time-data$time[1]
  # log_speed$stop.time<- log_speed$stop.time-data$time[1]
  
  # log_place$start.time<- log_place$start.time-data$time[1]
  # log_place$stop.time<- log_place$stop.time-data$time[1]
  data$time<- data$time-data$time[1]
  
  
  ## test  filter code ##
  
  
  
  ##  end  ##
  
  plot<-ggplot()+
    geom_point(data[start1:stop1,], mapping =  aes(time, speed),size=0.75, alpha=0.1)+
    ylim(-.1,0.7)
  
  if(nrow(log_speed)>0){
    plot<-plot+
      geom_rect(log_speed,mapping =  aes(xmin=start.time,xmax=stop.time,ymin=0,ymax=0.6),alpha=0.35)+
      scale_fill_manual(values= cols)
  }
  if(nrow(log_place)>0){
    plot<-plot+
      geom_rect(log_place,mapping =  aes(xmin=start.time,xmax=stop.time,ymin=-0.1,ymax=0, fill= label),alpha = .3)+
      geom_label(data = log_place,
                 mapping = aes(x = (start.time+stop.time)/2, y = -0.05, label = log_place$aisles.name),
                 label.size = .32)
  }
  if(save){
    if( ! file.exists(paste0('output/',params$output.dir,'/png/speed'))){
      dir.create(paste0('output/',params$output.dir,'/png/speed'))
    }
    ggsave(paste0('output/',params$output.dir,'/png/speed/', id, '.png'), 
           plot, width = (last(data$time)-data$time[1])/10, height = 7, units = 'cm',limitsize = FALSE)
  }
  
  plot
}

feature.plot<- function(data, log_speed, map, id){
  
  if( ! file.exists(paste0('output/',params$output.dir,'/png/speed/',map))){
    dir.create(paste0('output/',params$output.dir,'/png/speed/',map),recursive = TRUE)
  }
  if(nrow(log_speed)>0){
    for (i in 1:nrow(log_speed)) {
      start<- log_speed$start[i]
      stop<- log_speed$stop[i]
      plot1<-ggplot()+
        geom_point(data[start:stop,], mapping =  aes(-z, x, colour= time))
      
      plot2<-ggplot()+
        geom_point(data[(start-1000):(stop+1000),], mapping =  aes(time, speed),size=0.75, alpha=1)+
        geom_rect(mapping = aes(xmin=data$time[start],xmax=data$time[stop],ymax=-0.05,ymin=-0.1), fill= "red", alpha=0.5)+
        ylim(-.1,0.7)
      plot<- ggarrange(plot1,plot2, ncol = 1,nrow = 2,heights = c(2,1))
      
      ggsave(paste0('output/',params$output.dir,'/png/speed/',map,'/', id,'_',i, '.png'), 
             plot,  width = 37.5, height = 21, units = 'cm',limitsize = FALSE)
    }
  }
}



filter.feature.plot<-function(data, log_aisles, id){
  
  log_aisles_main<- log_aisles %>% filter(label == "main")
  log_aisles_WT<- log_aisles %>% filter(label == "walk through")
  log_aisles_target<-log_aisles %>% filter(target == "TRUE")
  log_aisles_nontarget<- log_aisles %>% filter(target != "TRUE")
  
  feature.plot(data, log_aisles_main,"main", id)
  feature.plot(data, log_aisles_WT,"walk through", id)
  feature.plot(data, log_aisles_target,"Target", id)
  feature.plot(data, log_aisles_nontarget,"Non target", id)
  

  

  
}

plot.best.r = function(data,id){
  plotx<-ggplot()+geom_point(data, mapping = aes(time,x))+ ggtitle(paste0("X vs Time",id))
  plotz<-ggplot()+geom_point(data, mapping = aes(time,z))+ ggtitle(paste0("Z vs Time",id))
  
  if( ! file.exists(paste0('output/',params$output.dir,'/png/best.r'))){
    dir.create(paste0('output/',params$output.dir,'/png/best.r'),recursive = TRUE)}
  ggsave(paste0('output/',params$output.dir,'/png/best.r/',
                id, '_x.png'), plotx, 
         width = 37.5, height = 21, units = 'cm')
  ggsave(paste0('output/',params$output.dir,'/png/best.r/',
                id, '_z.png'), plotz, 
         width = 37.5, height = 21, units = 'cm')
}

looking.plot<-function(log,input.data, input.look, id, params,gg,title){
  view.points<-log.subset(input.look, log)
  if(any(!is.na(view.points$x)))
  gg<- gg+ geom_point(data=view.points, mapping= aes(x=x, y=z), colour="orange", alpha= 0.2)
  
  if( ! file.exists(paste0('output/',params$output.dir,'/png/Look'))){
    dir.create(paste0('output/',params$output.dir,'/png/Look'),recursive = TRUE)
  }
  ggsave(paste0('output/',params$output.dir,'/png/', 'Look', '/',
                id, title,'_RAW.png'), gg, 
         width = 37.5, height = 21, units = 'cm')
}



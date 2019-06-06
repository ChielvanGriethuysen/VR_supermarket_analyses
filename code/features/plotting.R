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
    geom_text(aes(y = -12, x = 2, 
                  label = paste("Person", 
                                substr(JSONfile, 1, 5),
                                "\n Walking route")), 
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
  
  gg<- gg.basic+
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
    geom_point(data = discretised.path[discretised.path$label=="stop greater than 6 sec speed smaller than 0.075",], 
                       mapping = aes(x = x, y = -z),
                       fill = 'green', colour = 'green', size= 3.5)+
    geom_text(aes(y = -48, x = 2, 
                  label = paste("N stops = ", nrow(logs$speed.log %>% filter(label== "stop greater than 6 sec speed smaller than 0.075")), "(X)" )),
              size = 5,
              colour = 'green')+ 
    geom_point(data = discretised.path[discretised.path$label=="stop greater than 6 sec speed greater than 0.075",], 
                     mapping = aes(x = x, y = -z),
                     fill = 'white', colour = 'white', size= 3.5)+
    geom_text(aes(y = -48, x = 3, 
                  label = paste("N slows = ", nrow(logs$speed.log %>% filter(label== "stop greater than 6 sec speed greater than 0.075")), "(X)")),
              colour = 'white', size = 5)
    #add crossings
    if(nrow(logs$crossings.log)){
    gg<-gg+geom_point(data = logs$crossings.log,
               aes(x = x.start, y = -z.start), size= 3.5,
               col = 'blue')
    }
  
    gg<-gg+geom_text(aes(y = -48, x = 4, 
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








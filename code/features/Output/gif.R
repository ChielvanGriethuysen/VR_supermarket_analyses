# plot
makeGifOfPathAndLooking <- function(input.data, input.look, JSONfile,params, img){
  walk.time<- round(last(input.data$time)-first(input.data$time))
  input.data$time<-input.data$time-first(input.data$time)
  input.look$time<-input.look$time-first(input.look$time)
  
  gg <- ggplot() + 
    #ylim(-53, -7) + xlim(0, 29) +
    scale_x_continuous(limits = c(0, 30), expand=c(0,0)) +
    scale_y_continuous(limits = c(2.5, 50), expand=c(0,0)) +
    annotation_custom(rasterGrob(image, 
                                 width = unit(1, "npc"), 
                                 height = unit(1, "npc")), 
                      -Inf, Inf, -Inf, Inf) +
    #    coord_fixed() + 
    coord_flip() +
    theme(legend.position = "none",
          plot.margin = margin(0, 0, 0, 0, "cm")) +
    geom_label(aes(y = 5, x = 28, 
                   label = paste("Person", 
                                 substr(JSONfile, 1, 5))), 
               size = 5) + 
    geom_hline(yintercept = 45.5)+ 
    geom_point(data = input.data, 
               mapping = aes(x = x, y = z), size=5, colour= "blue")+
    geom_point(data=input.look, 
               mapping= aes(x=x, y=z), size=2, colour="orange", alpha= 0.4)+
    labs(title = 'Time: {frame_time}') +
    transition_time(time)+
    exit_fade(alpha = 1)+
    ease_aes('linear')
  
  ggani<-animate(gg,duration = 15, fps = 10, height= 800, width= 1200)
  
  if( ! file.exists(paste0('output/',params$output.dir,'/gif/path&view'))){
    dir.create(paste0('output/',params$output.dir,'/gif/path&view'), recursive = TRUE)
  }
  JSONfile <- substr(JSONfile, 1, 21)
  anim_save(filename = paste0(JSONfile,'.gif'),animation = ggani ,path='output/gif/path&view' )
}
#not working yet
makeGifOfPath <- function(input.data, JSONfile,params, img){
  time<- round(last(input.data$time)-first(input.data$time))
  
  gg <- ggplot() + 
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
    geom_point(data = input.data, 
               mapping = aes(x = x, y = -z), size=5, colour= "blue")+
    transition_reveal(time)+
    exit_fade(alpha = 1)+
    ease_aes('linear')
  
  ggani<-animate(gg,duration = 60, fps = 10, height= 800, width= 1200)
  
  if( ! file.exists(paste0('output/',params$output.dir,'/gif/path/'))){
    dir.create(paste0( 'output/',params$output.dir,'/gif/path/'),recursive = TRUE)
  }
  JSONfile <- substr(JSONfile, 1, 21)
  anim_save(filename = paste0(JSONfile,'.gif'),animation = ggani,path=paste0('output/gif/path/') )
}
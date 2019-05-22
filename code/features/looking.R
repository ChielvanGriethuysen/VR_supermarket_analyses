getLookings<- function(aisles.log, input.look, aisles){
  walk.throughs<-filter(aisles.log, label== "walk through")
  walk.throughs$view.left<- walk.throughs$view.right<- numeric(nrow(walk.throughs))
  

  for( i in 1: nrow(walk.throughs)){
    # walked from right to left
    if(walk.throughs$z.start[i]>walk.throughs$z.stop[i]){
      viewpoints<- input.look[walk.throughs$start[i]:walk.throughs$stop[i],]
      cur.aisles<- aisles[walk.throughs$id[i],]
      filterd<-viewpoints %>% filter(z> -cur.aisles$zmax , z< -cur.aisles$zmin)
      
      n.left<- filterd%>% filter(x< mean(c(cur.aisles$xmax,cur.aisles$xmin))) %>% nrow()
      n.right<- filterd%>% filter(x> mean(c(cur.aisles$xmax,cur.aisles$xmin))) %>% nrow()
      walk.throughs$view.left[i]<- n.left
      walk.throughs$view.right[i]<- n.right


    }
    #walked from left to right
    else{
      viewpoints<- input.look[walk.throughs$start[i]:walk.throughs$stop[i],]
      cur.aisles<- aisles[walk.throughs$id[i],]
      filterd<-viewpoints %>% filter(z> -cur.aisles$zmax , z< -cur.aisles$zmin)
      
      n.right<- filterd%>% filter(x< mean(c(cur.aisles$xmax,cur.aisles$xmin))) %>% nrow()
      n.left<- filterd%>% filter(x> mean(c(cur.aisles$xmax,cur.aisles$xmin))) %>% nrow()
      walk.throughs$view.left[i]<- n.left
      walk.throughs$view.right[i]<- n.right
    }
  }
  walk.throughs$View.fraction<- walk.throughs$view.left/walk.throughs$view.right
  return(list(log= walk.throughs))
}




looking.plot.stop<-function(speed.log,input.data, input.look, JSONfile,gg){
  
  
  stops<-log.subset(input.look, speed.log %>% filter(label == "stop"))
  no.stops<-log.subset(input.look, speed.log %>% filter(label == "stop"), rev = TRUE)
  
  #gg<- gg+ geom_point(data=no.stops, mapping= aes(x=x, y=-z), colour="gold", alpha= 0.05)  
  gg<- gg+ geom_point(data=stops, mapping= aes(x=x, y=-z), colour="orange", alpha= 0.2)


  
  ggsave(paste0('output/png/', 'Look', '/',
                JSONfile, 'stop','_RAW.png'), gg, 
         width = 37.5, height = 21, units = 'cm')
  
}

looking.plot.aisles<-function(aisles.log,input.data, input.look, JSONfile,gg){
  
  
  aisles<-log.subset(input.look, aisles.log %>% filter(label == "walk through"| label == "same side in out"))
  

  gg<- gg+ geom_point(data=aisles, mapping= aes(x=x, y=-z), colour="orange", alpha= 0.2)
  
  
  
  ggsave(paste0('output/png/', 'Look', '/',
                JSONfile, 'aisles','_RAW.png'), gg, 
         width = 37.5, height = 21, units = 'cm')
  
}
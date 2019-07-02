getLookings<- function(aisles.log, input.look, aisles){
  walk.throughs<-filter(aisles.log, label== "walk through")
  walk.throughs$view.left<- walk.throughs$view.right<-walk.throughs$look.switch<- numeric(nrow(walk.throughs))
  
  if( nrow(walk.throughs)>0){
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
      #calculate switches
      divide<- filterd$x<mean(c(cur.aisles$xmax,cur.aisles$xmin))
      walk.throughs$look.switch[i]<- sum(divide!=lead(divide,default =  FALSE))
      
    }
  }
  walk.throughs$View.fraction<- walk.throughs$view.left/walk.throughs$view.right
  return(list(log= walk.throughs))
}




looking.plot.stop<-function(speed.log,input.data, input.look, JSONfile, params,gg){
  
  
  stops<-log.subset(input.look, speed.log %>% filter(label == "stop"))
  no.stops<-log.subset(input.look, speed.log %>% filter(label == "stop"), rev = TRUE)
  
  #gg<- gg+ geom_point(data=no.stops, mapping= aes(x=x, y=-z), colour="gold", alpha= 0.05)  
  gg<- gg+ geom_point(data=stops, mapping= aes(x=x, y=-z), colour="orange", alpha= 0.2)

  if( ! file.exists(paste0('output/',params$output.dir,'/png/Look'))){
    dir.create(paste0('output/',params$output.dir,'/png/Look'),recursive = TRUE)
  }
  
  ggsave(paste0('output/',params$output.dir,'/png/', 'Look', '/',
                JSONfile, 'stop','_RAW.png'), gg, 
         width = 37.5, height = 21, units = 'cm')
  
}

looking.plot.aisles<-function(aisles.log,input.data, input.look, JSONfile,params,gg){
  
  
  aisles<-log.subset(input.look, aisles.log %>% filter(label == "walk through"| label == "same side in out"))
  

  gg<- gg+ geom_point(data=aisles, mapping= aes(x=x, y=-z), colour="orange", alpha= 0.2)
  
  if( ! file.exists(paste0('output/',params$output.dir,'/png/Look'))){
    dir.create(paste0('output/',params$output.dir,'/png/Look'),recursive = TRUE)
  }
  
  ggsave(paste0('output/',params$output.dir,'/png/', 'Look', '/',
                JSONfile, 'aisles','_RAW.png'), gg, 
         width = 37.5, height = 21, units = 'cm')
  
}
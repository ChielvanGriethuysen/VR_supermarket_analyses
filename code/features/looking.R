# Extracts the places where somone has looked and how many times there was a change in side
#
# Last edited 2019-07-03 by Chiel van Griethuijsen (m.a.vangriethuijsen@students.uu.nl)

getLookings<- function(aisles.log, input.look, aisles){
  walk.throughs<-filter(aisles.log, label== "walk through")
  walk.throughs$view.left<- walk.throughs$view.right<-walk.throughs$look.switch<- numeric(nrow(walk.throughs))
  
  if( nrow(walk.throughs)>0){
    for( i in 1: nrow(walk.throughs)){
      # walked from right to left
      if(walk.throughs$z.start[i]>walk.throughs$z.stop[i]){
        viewpoints<- input.look[walk.throughs$start[i]:walk.throughs$stop[i],]
        cur.aisles<- aisles[walk.throughs$box.id[i],]
        filterd<-viewpoints %>% filter(z< cur.aisles$zmax , z> cur.aisles$zmin)
        
        n.left<- filterd%>% filter(x< mean(c(cur.aisles$xmax,cur.aisles$xmin))) %>% nrow()
        n.right<- filterd%>% filter(x> mean(c(cur.aisles$xmax,cur.aisles$xmin))) %>% nrow()
        walk.throughs$view.left[i]<- n.left
        walk.throughs$view.right[i]<- n.right
  
  
      }
      #walked from left to right
      else{
        viewpoints<- input.look[walk.throughs$start[i]:walk.throughs$stop[i],]
        cur.aisles<- aisles[walk.throughs$box.id[i],]
        filterd<-viewpoints %>% filter(z< cur.aisles$zmax , z> cur.aisles$zmin)
        
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






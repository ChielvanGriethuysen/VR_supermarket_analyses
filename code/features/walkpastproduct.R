# VR Supermarkt: function to extract the whether someone walked past a product
#
# Last edited 2018-10-18 by Laurent Smeets (l.s.m.smeets@uu.nl)




WalkpastProduct<-function(data,
                          input.data,
                          gg,
                          products,
                          products2,
                          hit.log,
                          full.images, 
                          save.data, 
                          i){
    
  
  if(full.images){
    gg.products <- gg
    
    cols<-c("main"= "#00BFC4","shopping"= "#F8766D", "TRUE"="lightgreen", "FALSE"="red")
    
    if(data$total.time[i]>180){
      productbox2<-products
    } else{
      productbox2<-filter(products, announced != TRUE)
    }
    
    if(data$total.time[i]>180){
      productslocation2<-products2
    } else{
      productslocation2<-filter(products2, announced != TRUE)
    }
    
    
    suppressWarnings(
      gg.products<- gg.products+  geom_rect(data = productbox2,
                                            mapping = aes(xmin = xmin, xmax = xmax,
                                                          ymin = zmin, ymax = zmax
                                                          ),
                                            fill= "yellow",
                                            alpha = .2)+
        geom_point(data = productslocation2, 
                   mapping = aes(x = x, y = z, 
                                 fill= factor(productslocation2$productnumber %in% hit.log$name.product)),
                   color= "red", size= 8, shape=21)+
        scale_fill_manual(values= cols)+
        geom_text(data = productslocation2, 
                  mapping = aes(x = x, y = z, 
                                label = productslocation2$productnumber),
                  color= "black")+
        geom_point(data = productslocation2, 
                   mapping = aes(x = x+.7, y = z+.3),
                   colour= "white", size=4)#+
        # geom_text(data = plyr::join(x=mutate(productslocation2, productnumber2= substr(productnumber,2,2)), 
        #                             y=mutate(rownames_to_column(as_tibble(as.character(unlist(select(data[i,],Hit_1:Hit_8)))), var= "hit"), productnumber2=value),
        #                             by= "productnumber2"), 
        #           mapping = aes(x = x, y = z, 
        #                         label = plyr::join(x=mutate(productslocation2, productnumber2= substr(productnumber,2,2)), 
        #                                            y=mutate(rownames_to_column(as_tibble(as.character(unlist(select(data[i,],Hit_1:Hit_8)))), var= "hit"), productnumber2=value),
        #                                            by= "productnumber2")$hit),
        #           color= "black", nudge_x= .7, nudge_y = .3, size=3)
      
    )
  } else {
    gg.products <- gg
  }
  
  # get data about visiting product boxes
  box.data<-calc.box.feature(input.data,productbox2)
  box.data<-productbox.label.add(box.data,input.data,productbox2)
   
  if(save.data){
  
  box.summarised<-box.data %>% group_by(name) %>%
    summarise(time=sum(time.spend),n=n()) %>%
    complete(name,fill = list(time=0,n=0))
    
  data[i,]<-mutate(data[i,],
                   n.box.P1= box.summarised$n[1],
                   n.box.P2= box.summarised$n[2],
                   n.box.P3= box.summarised$n[3],
                   n.box.P4= box.summarised$n[4],
                   n.box.P5= box.summarised$n[5],
                   n.box.P6= box.summarised$n[6],
                   n.box.P7= box.summarised$n[7],
                   n.box.P8= box.summarised$n[8])
  
  data[i,]<-mutate(data[i,],time.box.P1= box.summarised$time[1],
                   time.box.P2= box.summarised$time[2],
                   time.box.P3= box.summarised$time[3],
                   time.box.P4= box.summarised$time[4],
                   time.box.P5= box.summarised$time[5],
                   time.box.P6= box.summarised$time[6],
                   time.box.P7= box.summarised$time[7],
                   time.box.P8= box.summarised$time[8])

  
  
  #todo, make this code more readable
  walked.past.not.picked.up1<-box.data$id %in% hit.log$prod_id
  
  walked.past.not.picked.up<-box.data[walked.past.not.picked.up1==F,]
  }
  res.products  <- list(gg.products = gg.products,
                          data = data,
                        log= box.data,
                        log.walked.past= walked.past.not.picked.up)
  return(res.products)  
}  





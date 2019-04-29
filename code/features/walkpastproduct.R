# VR Supermarkt: function to extract the whether someone walked past a product
#
# Last edited 2018-10-18 by Laurent Smeets (l.s.m.smeets@uu.nl)




WalkpastProduct<-function(data,
                          input.data,
                          gg,
                          products,
                          products2,
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
                                                          ymin = zmin, ymax = zmax,
                                                          colour = colour, x = x, y = y),
                                            fill= "yellow",
                                            alpha = .2)+
        geom_point(data = productslocation2, 
                   mapping = aes(x = x, y = z, 
                                 fill= factor(substr(productslocation2$productnumber, 2,2)  %in% unlist(select(data[i,],Hit_1:Hit_8)), levels=c(T,F))),
                   color= "red", size= 8, shape=21)+
        scale_fill_manual(values= cols)+
        geom_text(data = productslocation2, 
                  mapping = aes(x = x, y = z, 
                                label = productslocation2$productnumber),
                  color= "black")+
        geom_point(data = productslocation2, 
                   mapping = aes(x = x+.7, y = z+.3),
                   colour= "white", size=4)+
        geom_text(data = plyr::join(x=mutate(productslocation2, productnumber2= substr(productnumber,2,2)), 
                                    y=mutate(rownames_to_column(as_tibble(as.character(unlist(select(data[i,],Hit_1:Hit_8)))), var= "hit"), productnumber2=value),
                                    by= "productnumber2"), 
                  mapping = aes(x = x, y = z, 
                                label = plyr::join(x=mutate(productslocation2, productnumber2= substr(productnumber,2,2)), 
                                                   y=mutate(rownames_to_column(as_tibble(as.character(unlist(select(data[i,],Hit_1:Hit_8)))), var= "hit"), productnumber2=value),
                                                   by= "productnumber2")$hit),
                  color= "black", nudge_x= .7, nudge_y = .3, size=3)
      
    )
  } else {
    gg.products <- gg
  }
  Checkwalkinproductbox <- function(position, productbox){
    position[1] > productbox$xmin & position[1] < productbox$xmax &
      -position[2] > productbox$zmin & -position[2] < productbox$zmax
  }
  
  
  a <- apply(input.data[, 2:4], 1, Checkwalkinproductbox, productbox = productbox2)
  t <- which(a, arr.ind = TRUE)
  producttimepoint.time.points <- t[, 2]
   
  if(save.data){
    
    if(length(producttimepoint.time.points)==0){
      time.in.productbox= array(0)
      times.through.productbox <- data.frame(a=0, b=0, d=0, e=0)
      colnames(times.through.productbox)[1:4] <- c("productnumber", "product", "Entry.time.point", "time.in.productbox")
      times.through.productbox$productnumber <- factor(times.through.productbox$productnumber, levels = productbox2$productnumber)
      
    }else{
      time.in.productbox <- input.data$time[t[c(diff(t[,1]) != 0, T), 2]] - input.data$time[t[c(T, diff(t[,1]) != 0), 2]] 
      times.through.productbox  <- matrix(t[c(TRUE, diff(t[,1]) != 0), ], ncol=2)
      times.through.productbox <- data.frame(productbox2$productnumber[ as.data.frame(times.through.productbox)[, 1]], times.through.productbox, time.in.productbox)
      colnames(times.through.productbox)[1:3] <- c("productnumber", "product", "Entry.time.point")
      times.through.productbox$productnumber <- factor(times.through.productbox$productnumber, levels = productbox2$productnumber)
    }
  }
  
  data[i,]<-mutate(data[i,],n.box.P1= table(times.through.productbox$productnumber)[1],
                   n.box.P2= table(times.through.productbox$productnumber)[2],
                   n.box.P3= table(times.through.productbox$productnumber)[3],
                   n.box.P4= table(times.through.productbox$productnumber)[4],
                   n.box.P5= table(times.through.productbox$productnumber)[5],
                   n.box.P6= table(times.through.productbox$productnumber)[6],
                   n.box.P7= table(times.through.productbox$productnumber)[7],
                   n.box.P8= table(times.through.productbox$productnumber)[8])
  
  
  product.times <- tapply(times.through.productbox$time.in.productbox, 
                          list(Category = times.through.productbox$productnumber), 
                          FUN = sum)
  
  product.times[is.na(product.times)] <- 0
  
  walked.past.not.picked.up1<-times.through.productbox$product %in% plyr::join(x=mutate(productslocation2, productnumber2= substr(productnumber,2,2)), 
                                                                               y=mutate(rownames_to_column(as_tibble(as.character(unlist(select(data[i,],Hit_1:Hit_8)))), var= "hit"), productnumber2=value),
                                                                               by= "productnumber2")$value
  
  walked.past.not.picked.up<-times.through.productbox$product[walked.past.not.picked.up1==F]
  
  n.walked.past.not.picked.up<-length(walked.past.not.picked.up)
  
  data[i,]<-mutate(data[i,],time.box.P1= product.times[1],
                   time.box.P2= product.times[2],
                   time.box.P3= product.times[3],
                   time.box.P4= product.times[4],
                   time.box.P5= product.times[5],
                   time.box.P6= product.times[6],
                   time.box.P7= product.times[7],
                   time.box.P8= product.times[8])
  
  data$n.walked.past.not.picked.up[i]<-n.walked.past.not.picked.up
  data$n.walked.past.not.picked.up.unique[i]<-length(unique(walked.past.not.picked.up))
  
  
  res.products  <- list(gg.products = gg.products,
                        data = data, 
                        time.in.productbox=time.in.productbox,
                        producttimepoint.time.points=producttimepoint.time.points,
                        times.through.productbox=times.through.productbox,
                        walked.past.not.picked.up=walked.past.not.picked.up,
                        n.walked.past.not.picked.up=n.walked.past.not.picked.up)
  
  return(res.products)  
}  





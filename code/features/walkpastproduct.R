# VR Supermarkt: function to extract the whether someone walked past a product
#
# Last edited 2018-10-18 by Laurent Smeets (l.s.m.smeets@uu.nl)




WalkpastProduct<-function(data,
                          input.data,
                          products,
                          products2,
                          hit.log,
                          full.images, 
                          save.data, 
                          i){
  
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
  
  
  # get data about visiting product boxes
  box.data<-calc.box.feature(input.data,productbox2)
  box.data<-productbox.label.add(box.data,input.data,productbox2)
  
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


  walked.past.not.picked.up1<-box.data$id %in% hit.log$prod_id
  
  walked.past.not.picked.up<-box.data[walked.past.not.picked.up1==F,]
  
  res.products  <- list(data = data,
                        log= box.data,
                        log.walked.past= walked.past.not.picked.up)
  return(res.products)  
}  





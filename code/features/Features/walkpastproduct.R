# VR Supermarkt: function to extract the whether someone walked past a product
#
# Last edited 2019-07-03 by Chiel van Griethuijsen (m.a.vangriethuijsen@students.uu.nl)




WalkpastProduct<-function(input.data,
                          productbox,
                          products,
                          hit.log){
  
  
  # get data about visiting product boxes
  box.data<-calc.box.feature(input.data,productbox)
  box.data<-add.basic.features(box.data,input.data)
  box.data<-add.quality.features(box.data,input.data)
  box.data<-add.view.quality.features(box.data,input.data)
  box.data<-productbox.label.add(box.data,input.data,productbox)
  
  # box.summarised<-box.data %>% group_by(name) %>%
  #   summarise(time=sum(time.spend),n=n()) %>%
  #   complete(name,fill = list(time=0,n=0))

  #check if somone gets near a product but dous not pick it up
  walked.past.not.picked.up<-box.data[!(box.data$box.id %in% hit.log$prod_id),]
  
  res.products  <- list(log= box.data,
                        log.walked.past= walked.past.not.picked.up)
  return(res.products)  
}  





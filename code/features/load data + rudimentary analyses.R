# VR Supermarkt: create dataframe to load all statistics in and run some basic analyses.
#
# Last edited 2019-07-03 by Chiel van Griethuijsen (m.a.vangriethuijsen@students.uu.nl)


runFirstAnalyses <- function(JSONfile,
                             Excel,
                             image,
                             params,
                             i){
  
  # Read the JSON files, remove excess data, 
  # save first basic results (total time, total distance and n data points),
  # make first gg plot and save it
  #
  # JSONfile   = name of the JSON file required
  # image      = the image for the background
  # raw.images = TRUE/FALSE should the raw image be saved
  # save.data  = TRUE/FALSE should the data be saved to a CSV file
  # data       = data frame to save the data in the end to a CSV file
  # input.dir  = directory of the input JSON files
  # output.dir = directory of the output image files
  # i          = number of data file, required to save to in data
  
  # Read data
  suppressWarnings(
    dat <- fromJSON(readLines(paste0('input/', params$input.dir, '/', JSONfile)),
                    simplifyDataFrame = TRUE)
  )
  input.log<- str_remove(JSONfile,".json") %>% str_c("Log.xml")
  
  if(file.exists(paste0('input/', params$input.dir, '/', input.log))){
    suppressWarnings(
      product.log <- xmlToDataFrame(paste0('input/', params$input.dir, '/', input.log), stringsAsFactors = FALSE)
    )
  }else{
    product.log<- data.frame(SesionLog= as.character())
  }
  
  # put data in one dataframe(position and eye data)
  input.data<-data.frame(time=dat$tracking_data$m_PupilTime,dat$tracking_data$m_FootPosition)
  input.look.left<-data.frame(time=dat$tracking_data$m_PupilTime,
                         dat$tracking_data$m_PositionLeftObject, 
                         dist= dat$tracking_data$m_SqrtDistanceLeftEye, 
                         confidence= dat$tracking_data$m_PupilConfidenceLeft,
                         lost=dat$tracking_data$m_PupilLoss )
  input.look.right<-data.frame(time=dat$tracking_data$m_PupilTime,
                              dat$tracking_data$m_PositionRightObject, 
                              dist= dat$tracking_data$m_SqrtDistanceRightEye, 
                              confidence= dat$tracking_data$m_PupilConfidenceRight,
                              lost=dat$tracking_data$m_PupilLoss )
  
  
  #make uncertain data nul
  input.look.left<- input.look.left %>% mutate(x=ifelse(confidence>0.75 &(lost=="None" | lost== "Right"),x,0),
                                     y=ifelse(confidence>0.75 &(lost=="None" | lost== "Right"),y,0),
                                     z=ifelse(confidence>0.75 &(lost=="None" | lost== "Right"),z,0))
  input.look.right<- input.look.right %>% mutate(x=ifelse(confidence>0.75 &(lost=="None" | lost== "Left"),x,0),
                                               y=ifelse(confidence>0.75 &(lost=="None" | lost== "Left"),y,0),
                                               z=ifelse(confidence>0.75 &(lost=="None" | lost== "Left"),z,0))
  
  # Remove duplicate data (speeds up all analyses)
  dup <- which(diff(input.data$time) == 0)
  if(length(dup) > 0){
    input.data<-input.data[-dup,]
  }
  
  #only use the data in the actuale supermarket
  first <- first(which(input.data$z < 45.5 & input.data$z >10))
  if(first > 1){
    input.data<-input.data[-1:-first,]
    input.look.left<-input.look.left[-1:-first,]
    input.look.right<-input.look.right[-1:-first,]
  }
  
  # Remove all datapoints after end of the task
  last <- last(which(input.data$z < 45.5))
  if(last < nrow(input.data)){
    input.data<-input.data[-last:-nrow(input.data),]
    input.look.left<-input.look.left[-last:-nrow(input.look.left),]
    input.look.right<-input.look.right[-last:-nrow(input.look.right),]
  }
  row.names(input.data) <- 1:nrow(input.data)
  row.names(input.look.left) <- 1:nrow(input.look.left)
  row.names(input.look.right) <- 1:nrow(input.look.right)
  
  #add data when possible
  input.look.left<- missing.view.data.fill(input.look.left)
  input.look.right<- missing.view.data.fill(input.look.right)
  
  #add speed and distance to dataframe and calculate total distance
  x.change <- diff(input.data$x, 1)
  y.change <- diff(input.data$z, 1)
  distance.between.points<-sqrt(x.change^2 + y.change^2)
  speed<-c(0,distance.between.points/diff(input.data$t,1))
  input.data<- data.frame(input.data, speed,c(mean(distance.between.points),distance.between.points))
  names(input.data)[6]<- "dist"
  
  # create product box based on suppermarket
  if(str_detect(dat$dataHeader$m_SupermarketName,"Nemo B")){
    productbox<- calc.productbox(params$products$nemo_b)
    products<- params$products$nemo_b
  }else if(str_detect(dat$dataHeader$m_SupermarketName,"Nemo A")){
    productbox<- calc.productbox(params$products$nemo_a)
    products<-params$products$nemo_a
  }else if(str_detect(dat$dataHeader$m_SupermarketName,"UMC")){
    productbox<- calc.productbox(params$products$UMC3pro)
    products<-params$products$UMC3pro
  }
  
  if(last(input.data$time)- input.data$time[1]<180){
    productbox<-filter(productbox, announced != TRUE)
    products<- filter(products, announced != TRUE)
  }
  products$prod_id<- 1:nrow(products)
  
  # put the hit log in a dataframe, filter on products from the product list
  start.stop<- which(apply(product.log, 1, function(x) any(grepl("Sessie|Ending", x))))
  product.all<- product.hit.log( data.frame(SesionLog=product.log[(start.stop[1]+1):(start.stop[2]-1),]))
  product.all<- cbind(product.all,
                       calc.spot.event.in.box(data.frame(x.start=product.all$x,z.start= product.all$z),params$features$aisles))
  product.all<-add.product.hit.position(product.all,input.data)
  product.all<- merge(product.all,products[,c(7,8,10)],by.x  = "product",by.y= "productname",all.x = TRUE)
  product.all<- product.all %>% arrange(time)
  product.all<- product.all %>% distinct(product, .keep_all = TRUE)
  
  product.hits<- filter.product.hits(products,product.all)$hit.target
  # product.hits<- check.product.hit(product.all, products)
  # product.hits<- add.product.hit.position(product.hits,input.data)
  # product.hits<- calc.short.dist(params$features$aisles, product.hits, input.data)
  
  res <- list(input.data= input.data,
              input.look.left= input.look.left,
              input.look.right= input.look.right,
              product.hits= product.hits,
              product.all= product.all,
              productbox = productbox,
              products= products)
  
  return(res)
}
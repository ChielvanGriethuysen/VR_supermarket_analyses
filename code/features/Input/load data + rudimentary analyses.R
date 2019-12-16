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
  input.log<- str_remove(JSONfile,".json") %>% str_c("_Log.xml")
  
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
                         lost= rep("None", length(dat$tracking_data$m_PupilTime)) )
  input.look.right<-data.frame(time=dat$tracking_data$m_PupilTime,
                              dat$tracking_data$m_PositionRightObject, 
                              dist= dat$tracking_data$m_SqrtDistanceRightEye, 
                              confidence= dat$tracking_data$m_PupilConfidenceRight,
                              lost=rep("None", length(dat$tracking_data$m_PupilTime)) )
  
  if(!is.null(dat$tracking_data$m_PupilLoss)){
    input.look.left$lost<- dat$tracking_data$m_PupilLoss
    input.look.right$lost<-dat$tracking_data$m_PupilLoss
  }
  
  #make uncertain data nul
  input.look.left<- input.look.left %>% mutate(x=ifelse(confidence>0.75 &(lost=="None" | lost== "Right"),x,NA),
                                     y=ifelse(confidence>0.75 &(lost=="None" | lost== "Right"),y,NA),
                                     z=ifelse(confidence>0.75 &(lost=="None" | lost== "Right"),z,NA))
  input.look.right<- input.look.right %>% mutate(x=ifelse(confidence>0.75 &(lost=="None" | lost== "Left"),x,NA),
                                               y=ifelse(confidence>0.75 &(lost=="None" | lost== "Left"),y,NA),
                                               z=ifelse(confidence>0.75 &(lost=="None" | lost== "Left"),z,NA))
  
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
  
  #chose wich eye is the beste one for the analysis
  input.look<- if(sum(is.na(input.look.left$x))< sum(is.na(input.look.right$x))) input.look.left else input.look.right
  
  #add looking directions
  input.look$angle<- calculate.direction(input.data, input.look)
  
  #add looking distance
  input.look$dist<- dist.2sets.2d(input.data,input.look)
  
  #add lodation
  input.look<- aisles.scan.location(input.look,params$features$aisles%>% filter(type=="shopping"),0.2)

  #remove confidence and lost variable
  input.look<- input.look %>% select(-c(lost, confidence))
  
  #add speed, distance and direction to position data
  input.data<- speed.dist.add(input.data)
  input.data$walk.direction<-calculate.direction(input.data, find.first.point.on.dist(input.data,1))
  
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
  
  #remove product if not announced
  if(last(input.data$time)- input.data$time[1]<180){
    productbox<-filter(productbox, announced != TRUE)
    products<- filter(products, announced != TRUE)
  }
  products$prod_id<- 1:nrow(products)
  
  # put the hit log in a dataframe, filter on products from the product list
  start.stop<- which(apply(product.log, 1, function(x) any(grepl("Sessie|Ending", x))))
  product.all<- product.hit.log( data.frame(SesionLog=product.log[(start.stop[1]+1):(start.stop[2]-1),]))
  product.all$id<- if(nrow(product.all)>0) 1:nrow(product.all) else numeric()
  product.all<- merge(product.all,
                       calc.spot.event.in.box(data.frame(x.start=product.all$x,z.start= product.all$z),params$features$aisles) %>%
                        transmute(aisles.type= params$features$aisles$type[row],
                                  aisles.name= params$features$aisles$names[row],
                                  id= col), by = "id")
  
  product.all<-add.product.hit.position(product.all,input.data)
  product.all<- merge(product.all,products[,c(7,8,10)],by.x  = "product",by.y= "productname",all.x = TRUE)
  product.all<- product.all %>% arrange(time)
  product.all<- product.all %>% distinct(product, .keep_all = TRUE)
  
  product.hits<- filter.product.hits(products,product.all)$hit.target
  
  res <- list(input.data= input.data,
              input.look= input.look,
              product.hits= product.hits,
              product.all= product.all,
              productbox = productbox,
              products= products)
  
  return(res)
}

#analyse productlog, put products in a dataframe
product.hit.log<- function(logs){
  logs<-logs  %>% filter(apply(logs,2, str_detect, pattern= "HIT"))%>% 
    separate(SesionLog, c("time","Product"), sep = "- HIT product #")%>% 
    separate(Product, c("product", "rest"), sep = c("\\|\\| X:"),extra = "merge")%>% 
    separate(rest, c("x", "rest"), sep = c("\\|\\| Y:"),extra = "merge")%>% 
    separate(rest, c("y", "z"), sep = c("\\|\\| Z:"),extra = "merge")
  
  hour<- logs$time %>% str_sub(2,3)%>% as.numeric()
  minutes<- logs$time %>% str_sub(5,6)%>% as.numeric()
  seconds<- logs$time %>% str_sub(8,9)%>% as.numeric()
  rest<- logs$time %>% str_sub(11,13)%>% as.numeric()
  
  time<- hour*60*60+minutes*60+seconds+rest/1000
  logs$time<- time
  
  logs[,3:5]<- sapply (logs[,3:5], as.numeric)
  return(logs)
  
}

# fill gaps when gap is small enough 
missing.view.data.fill<- function(data){
  n<-start<- stop<- i<-0
  while (i <= nrow(data)) {
    i<-i+1
    if(i<= nrow(data) &&is.na(data$x[i])){
      start<- i
      while (i<= nrow(data) &&is.na(data$x[i])) {
        i<-i+1
      }
      stop<- i-1
      #fill in gap when time between points is les than 0.1 sec
      if(data$time[stop]-data$time[start]<0.1)
      {
        for (j in start:stop) {
          if(start==1){
            data$x[j]<- data$x[stop+1]
            data$y[j]<- data$y[stop+1]
            data$z[j]<- data$z[stop+1]
          } else if(stop == nrow(data)){
            data$x[j]<- data$x[start-1]
            data$y[j]<- data$y[start-1]
            data$z[j]<- data$z[start-1]
            
          }else{
            data$x[j]<- mean(c(data$x[start-1],data$x[stop+1]))
            data$y[j]<- mean(c(data$y[start-1],data$y[stop+1]))
            data$z[j]<- mean(c(data$z[start-1],data$z[stop+1]))
          }
        }
      }
      if(i >= nrow(data)){
        break
      }
    }
  }
  return(data)
}
#calculate where at eache aisles somone viewed, ad location to view points
aisles.scan.location<- function(view, aisles, grid.size){
  #divide aisles in the two sides
  aisles.up<- aisles %>%mutate(xmin= xmax-0.5, side="up")
  aisles.down<- aisles %>%mutate(xmax= xmin+0.5, side="down")
  aisles<- rbind(aisles.up,aisles.down)
  
  #find location of view points
  view$id<- 1:nrow(view)
  view.in.box<- box.check.list(view, aisles) %>% transmute(id= col,aisle.side= aisles$side[row], aisle.name= aisles$names[row])
  view<-merge(view, view.in.box, by= "id", all = TRUE)
  
  #divide aisles based on type because the have different length
  aisles.A<- aisles%>% filter(grepl("A",names))
  aisles.B<- aisles%>% filter(grepl("B",names))
  #make grid for A and B aisles
  aisles.A.widths<- seq(aisles.A$zmin[1], aisles.A$zmax[1],grid.size) 
  aisles.B.widths<- seq(aisles.B$zmin[1], aisles.B$zmax[1],grid.size) 
  aisles.hight<- seq(0,2.2,grid.size)
  #use grid as buckets to discretize coordinates 
  view.else<- view %>% filter(!grepl("A|B",aisle.name)) %>% mutate(hight= NA,
                                                                   width= NA)
  if(grepl("A", view$aisle.name) %>% any()){
  view.A<- view %>% filter(grepl("A",aisle.name)) %>% mutate(hight= cut(y,aisles.hight, labels = aisles.hight[-length(aisles.hight)]),
                                                             width= cut(z,aisles.A.widths, labels=aisles.A.widths[-length(aisles.A.widths)])%>%
                                                               as.character()%>% as.numeric())
  }else{
    view.A<-NULL
  }
  if(grepl("B", view$aisle.name) %>% any()){
  view.B<- view %>% filter(grepl("B",aisle.name)) %>% mutate(hight= cut(y,aisles.hight, labels = aisles.hight[-length(aisles.hight)]),
                                                             width= cut(z,aisles.B.widths, labels=aisles.B.widths[-length(aisles.B.widths)])%>%
                                                               as.character()%>% as.numeric())
  }else{
    view.B<- NULL
  }
  view<- rbind(view.A,view.B, view.else) %>% arrange(id)
  return(view)
}


#calculate box around products, used to determine if person is doing somting close to the product
calc.productbox<- function(products){
  data.frame(xmin = rep(NA, nrow(products)),
             xmax = rep(NA, nrow(products)),
             zmin = rep(NA, nrow(products)),
             zmax = rep(NA, nrow(products)),
             up.down.side= products$up.down.side,
             announced = products$announced,
             productnumber =  products$productnumber
  ) %>%
    mutate(xmin = ifelse(up.down.side == "up", products$x-products$height,
                         ifelse(up.down.side =="down", products$x,
                                products$x-.5*products$height))) %>% 
    mutate(xmax = ifelse(up.down.side == "up", products$x,
                         ifelse(up.down.side == "down",products$x+products$height,
                                products$x+.5*products$height)))%>%
    mutate(zmin = ifelse(up.down.side == "sideleft", products$z-products$width, 
                         ifelse(up.down.side == "sideright",  products$z,
                                products$z-.5*products$width))) %>%
    mutate(zmax = ifelse(up.down.side == "sideleft", products$z, 
                         ifelse(up.down.side == "sideright",  products$z+products$width,
                                products$z+.5*products$width)))
}
# VR Supermarkt: create dataframe to load all statistics in and run some basic analyses.
#
# Last edited 2019-07-03 by Chiel van Griethuijsen (m.a.vangriethuijsen@students.uu.nl)


# createDataFrame <- function(data.files){
#   names1<-c()
#   for(P in 1:params$features$number.of.items){
#     names1[P]<-paste0("n.box.P", P)
#   }
#   Y1<-as.data.frame(matrix(0 , ncol= length(names1), nrow=length(numeric(length(data.files)))))
#   colnames(Y1)<-names1
#   
#   names2<-c()
#   for(PP in 1:params$features$number.of.items){
#     names2[PP]<-paste0("time.box.P", PP)
#   }
#   Y2<-as.data.frame(matrix(0 , ncol= length(names2), nrow=length(numeric(length(data.files)))))
#   colnames(Y2)<-names2
#   
#   data <- data.frame(name = character(length(data.files)),          # name of input file
#                      ID= character(length(data.files)),             # Identification variable
#                      total.time = numeric(length(data.files)),      # total time spent on task
#                      total.distance = numeric(length(data.files)),  # needs to be fixed.
#                      
#                      n.aisle.1A = numeric(length(data.files)),      # number of times someone went into aisle 1A
#                      n.aisle.2A = numeric(length(data.files)),      # number of times someone went into aisle 2A
#                      n.aisle.3A = numeric(length(data.files)),      # number of times someone went into aisle 3A
#                      n.aisle.4A = numeric(length(data.files)),      # number of times someone went into aisle 4A
#                      n.aisle.5A = numeric(length(data.files)),      # number of times someone went into aisle 5A
#                      n.aisle.1B = numeric(length(data.files)),      # number of times someone went into aisle 1B
#                      n.aisle.2B = numeric(length(data.files)),      # number of times someone went into aisle 2B
#                      n.aisle.3B = numeric(length(data.files)),      # number of times someone went into aisle 3B
#                      n.aisle.4B = numeric(length(data.files)),      # number of times someone went into aisle 4B
#                      n.aisle.5B = numeric(length(data.files)),      # number of times someone went into aisle 5B
#                      n.aisle.6B = numeric(length(data.files)),      # number of times someone went into aisle 6B
#                      n.aisle.M1 = numeric(length(data.files)),      # number of times someone went into aisle M1
#                      n.aisle.M2 = numeric(length(data.files)),      # number of times someone went into aisle M2
#                      n.aisle.M3 = numeric(length(data.files)),      # number of times someone went into aisle M3
#                      time.aisle.1A = numeric(length(data.files)),   # total time someone spent into aisle 1A  
#                      time.aisle.2A = numeric(length(data.files)),   # total time someone spent into aisle 2A  
#                      time.aisle.3A = numeric(length(data.files)),   # total time someone spent into aisle 3A
#                      time.aisle.4A = numeric(length(data.files)),   # total time someone spent into aisle 4A  
#                      time.aisle.5A = numeric(length(data.files)),   # total time someone spent into aisle 5A  
#                      time.aisle.1B = numeric(length(data.files)),   # total time someone spent into aisle 1B  
#                      time.aisle.2B = numeric(length(data.files)),   # total time someone spent into aisle 2B  
#                      time.aisle.3B = numeric(length(data.files)),   # total time someone spent into aisle 3B  
#                      time.aisle.4B = numeric(length(data.files)),   # total time someone spent into aisle 4B  
#                      time.aisle.5B = numeric(length(data.files)),   # total time someone spent into aisle 5B  
#                      time.aisle.6B = numeric(length(data.files)),   # total time someone spent into aisle 6B  
#                      time.aisle.M1 = numeric(length(data.files)),   # total time someone spent into aisle M1  
#                      time.aisle.M2 = numeric(length(data.files)),   # total time someone spent into aisle M2  
#                      time.aisle.M3 = numeric(length(data.files)),   # total time someone spent into aisle M3  
#                      
#                      n.crossings = numeric(length(data.files)),     # total number of crossings someone made (crossing his/her own path)
#                      n.crossings.outside.aisles = numeric(length(data.files)), # how many crossings did make outside of the horizontal shopping aisles
#                      
#                      n.stops= numeric(length(data.files)),             # number of stops someone made
#                      n.stops.item = numeric(length(data.files)),       # number of stops someone made in front of an item
#                      n.stops.elsewhere= numeric(length(data.files)),   # number of stops someone made elsewhere (not in front of items) 
#                      n.slows = numeric(length(data.files)),            # number of slows someone made
#                      n.slows.item = numeric(length(data.files)),       # number of slows someone made in front of an item if slows starts in front of item
#                      n.slows.elsewhere = numeric(length(data.files)),  # number of slows someone made elsewhere (not in front of items) 
#                      n.slows.item2 = numeric(length(data.files)),      # number of slows someone made in front of an item if a slow is anywhere in the hitbox of the item
#                      n.slows.elsewhere2 = numeric(length(data.files)), # number of slows someone made elsewhere (not in front of items, when a slow is anywhere in the hitbox of the item) 
#                      total.stoping.time= numeric(length(data.files)),  # total stopping time
#                      total.slowing.time = numeric(length(data.files)), # total time someone spent slowing
#                      
#                      cross.1st.1.3rd= numeric(length(data.files)),     # number of crossings in the first 1/3rd of the time someone made
#                      cross.2nd.1.3rd= numeric(length(data.files)),     # number of crossings in the second 1/3rd of the time someone made
#                      cross.3rd.1.3rd= numeric(length(data.files)),     # number of crossings in the third 1/3rd of the time someone made
#                      slows.1st.1.3rd = numeric(length(data.files)),    # number of slows in the first 1/3rd of the time someone made
#                      slows.2nd.1.3rd = numeric(length(data.files)),    # number of slows in the second 1/3rd of the time someone made
#                      slows.3rd.1.3rd = numeric(length(data.files)),    # number of slows in the third 1/3rd of the time someone made
#                      slows.1st.1.3rd.items = numeric(length(data.files)), # number of slows in the first 1/3rd of the time someone made in front of an item
#                      slows.2nd.1.3rd.items = numeric(length(data.files)), # number of slows in the second 1/3rd of the time someone made in front of an item
#                      slows.3rd.1.3rd.items = numeric(length(data.files)), # number of slows in the third 1/3rd of the time someone made in front of an item
#                      stops.1st.1.3rd = numeric(length(data.files)),    # number of stops in the first 1/3rd of the time someone made
#                      stops.2nd.1.3rd = numeric(length(data.files)),    # number of stops in the second 1/3rd of the time someone made
#                      stops.3rd.1.3rd = numeric(length(data.files)),    # number of stops in the third 1/3rd of the time someone made
#                      
#                      n.datapoints = numeric(length(data.files)),                       # total number of datapoints in the file (data quality metric)
#                      datapoints.per.second = numeric(length(data.files)),              # number datapoints per second in the file (data quality metric)
#                      max.difference.between.points = numeric(length(data.files)),      # maximum difference in distance between two consequtive points in the file (data quality metric) 
#                      qt95.difference.between.points = numeric(length(data.files)),     # .95 quantile difference in distance between consequtive points in the file (data quality metric) 
#                      qt99.difference.between.points = numeric(length(data.files)),     # .99 quantile difference in distance between consequtive points in the file (data quality metric) 
#                      max.difference.between.timepoints = numeric(length(data.files)),  # maximum difference in time between two consequtive points in the file (data quality metric) 
#                      qt95.difference.between.timepoints = numeric(length(data.files)), # .95 quantile difference in time between consequtive points in the file (data quality metric)
#                      qt99.difference.between.timepoints = numeric(length(data.files)), # .99 quantile difference in time between consequtive points in the file (data quality metric)
#                      average.speed = numeric(length(data.files)),      # average speed of user
#                      
#                      Y1,                                               # number of times someone walked into the hitbox of the different products
#                      Y2,                                               # total time spent in the hitbox of the different products
#                      n.walked.past.not.picked.up= numeric(length(data.files)),         # number of times walked past an item without picking it up
#                      n.walked.past.not.picked.up.unique = numeric(length(data.files)), # number of times walked past an unique item without picking it up
#                      
#                      n.walked.through.aisles= numeric(length(data.files)), #nr of times walked through a aisles
#                      n.walked.in.out.aisles= numeric(length(data.files)), #nr of times walked in an aisles and the same side out
#                      
#                      Hit_Totaal = numeric(length(data.files)),        # total items someone picked up
#                      Avatars = numeric(length(data.files)),           # this item was merged from an other dataset and not created in this code
#                      VR_aborted = numeric(length(data.files)),        # this item was merged from an other dataset and not created in this code
#                      Tijd = numeric(length(data.files)),              # this item was merged from an other dataset and not created in this code
#                      Interference = numeric(length(data.files)),      # this item was merged from an other dataset and not created in this code
#                      Hit_1 = numeric(length(data.files)),             # this item was merged from an other dataset and not created in this code
#                      Hit_2 = numeric(length(data.files)),             # this item was merged from an other dataset and not created in this code
#                      Hit_3  = numeric(length(data.files)),            # this item was merged from an other dataset and not created in this code
#                      Hit_4 = numeric(length(data.files)),             # this item was merged from an other dataset and not created in this code
#                      Hit_5 = numeric(length(data.files)),             # this item was merged from an other dataset and not created in this code
#                      Hit_6 = numeric(length(data.files)),             # this item was merged from an other dataset and not created in this code
#                      Hit_7  = numeric(length(data.files)),            # this item was merged from an other dataset and not created in this code
#                      Hit_8 = numeric(length(data.files)),             # this item was merged from an other dataset and not created in this code
#                      Tijd_H1 = numeric(length(data.files)),           # this item was merged from an other dataset and not created in this code
#                      Tijd_H2 = numeric(length(data.files)),           # this item was merged from an other dataset and not created in this code
#                      Tijd_H3 = numeric(length(data.files)),           # this item was merged from an other dataset and not created in this code
#                      Tijd_H4 = numeric(length(data.files)),           # this item was merged from an other dataset and not created in this code
#                      Tijd_H5 = numeric(length(data.files)),           # this item was merged from an other dataset and not created in this code
#                      Tijd_H6 = numeric(length(data.files)),           # this item was merged from an other dataset and not created in this code
#                      Tijd_H7 = numeric(length(data.files)),           # this item was merged from an other dataset and not created in this code
#                      Tijd_H8 = numeric(length(data.files)),           # this item was merged from an other dataset and not created in this code
#                      FA_Totaal = numeric(length(data.files)),         # this item was merged from an other dataset and not created in this code
#                      FA_Time = numeric(length(data.files)),           # this item was merged from an other dataset and not created in this code
#                      Kassa = numeric(length(data.files)),             # this item was merged from an other dataset and not created in this code
#                      stringsAsFactors = FALSE
#   )
#   return(data)
# }

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
  
  # put data in one dataframe
  input.data<-data.frame(dat$tracking_data$m_PupilTime,dat$tracking_data$m_FootPosition)
  input.look<-data.frame(dat$tracking_data$m_PupilTime,dat$tracking_data$m_PositionLeftObject)
  
  names(input.data)[1]<- "time"
  names(input.look)[1]<- "time"
  
  # Remove duplicate data (speeds up all analyses)
  dup <- which(diff(input.data$time) == 0)
  if(length(dup) > 0){
    input.data<-input.data[-dup,]
  }
  
  #only use the data in the actuale supermarket
  first <- first(which(input.data$z < 45.5 & input.data$z >10))
  if(first > 1){
    input.data<-input.data[-1:-first,]
    input.look<-input.look[-1:-first,]
  }
  
  # Remove all datapoints after end of the task
  last <- last(which(input.data$z < 45.5))
  if(last < nrow(input.data)){
    input.data<-input.data[-last:-nrow(input.data),]
    input.look<-input.look[-last:-nrow(input.look),]
  }
  row.names(input.data) <- 1:nrow(input.data)
  row.names(input.look) <- 1:nrow(input.look)
  
  #add speed and distance to dataframe and calculate total distance
  x.change <- diff(input.data$x, 1)
  y.change <- diff(input.data$z, 1)
  distance.between.points<-sqrt(x.change^2 + y.change^2)
  speed<-c(0,distance.between.points/diff(input.data$t,1))
  input.data<- data.frame(input.data, speed,c(mean(distance.between.points),distance.between.points))
  names(input.data)[6]<- "dist"
  
  #input.data<-data.frame(input.data,input.look)
  
  
  # Save results of basic analyses
  # data<-data.basic(data,input.data, i)
  # if(params$external.excel){
  #   #add VRlog data
  #   data<-data.VRlog(data,Excel,i)
  # }
  
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
  # put the hit log in a dataframe, filter on products from the product list
  start.stop<- which(apply(product.log, 1, function(x) any(grepl("Sessie|Ending", x))))
  product.all<- product.hit.log( data.frame(SesionLog=product.log[(start.stop[1]+1):(start.stop[2]-1),]))
  
  product.all<- cbind(product.all,
                       calc.spot.event.in.box(data.frame(x.start=product.all$x,z.start= product.all$z),params$features$aisles))
  product.all<-add.product.hit.position(product.all,input.data)
  products$prod_id<- 1:nrow(products)
  product.all<- merge(product.all,products[,c(7,8,10)],by.x  = "product",by.y= "productname",all.x = TRUE)
  product.all<- product.all %>% arrange(time)
  product.all<- product.all %>% distinct(product, .keep_all = TRUE)
  
  product.hits<- filter.product.hits(products,product.all)$hit.target
  # product.hits<- check.product.hit(product.all, products)
  # product.hits<- add.product.hit.position(product.hits,input.data)
  # product.hits<- calc.short.dist(params$features$aisles, product.hits, input.data)
  
  res <- list(input.data= input.data,
              input.look= input.look,
              product.hits= product.hits,
              product.all= product.all,
              data = data,
              productbox = productbox,
              products= products)
  
  return(res)
}
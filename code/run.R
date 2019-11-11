# VR Supermarkt: Extract features
#
# Last edited 2019-07-03 by Chiel van Griethuijsen (m.a.vangriethuijsen@students.uu.nl)

# preamble ----------------------------------------------------------------
packages <- c("jsonlite", "tidyverse", "png", "ggforce",
              "ggalt", "Rcpp", "grid", "gganimate","XML", 
              "DescTools", "xlsx","ggpubr","LearnGeom")

lapply(packages, require, character.only = TRUE)

source("code/config.R") # load the configuration file
sapply(list.files("code/features", full.names = TRUE, '.R', recursive= TRUE), source) # load FE funcs
sourceCpp('code/features/Support/cppDoLinesIntersect.cpp') # load the C function that calculates crossings

# Load image
image <- readPNG(paste0('input/', params$img.name))

# get data files
data.files <- list.files(
  path = file.path("input", params$input.dir),
  pattern = 'json',
  full.names = FALSE
)


# Create data frame to save results
data <- createDataFrame(data.files)


# loop over all the participants 
for(i in 1 : length(data.files)){
  print(paste('Calculating logs for file', i, "of",length(data.files) , "started at",Sys.time()))
  JSONfile <- data.files[i]
  id<- str_split(JSONfile,"_")[[1]][1]
  
  #load data, make datasets to use for analysis
  res <- runFirstAnalyses(JSONfile = JSONfile, 
                          Excel=Excel, 
                          image = image, 
                          params= params,
                          i = i)

  #calculate start and stop points and add features
  #for entering a aislesbox
  res.aisles <- getAisleTimes(input.data= res$input.data,
                              products= res$products,
                              aisles = params$features$aisles)
  #for entering a productbox  
  res.products <- WalkpastProduct(input.data = res$input.data,
                                  productbox =  res$productbox,
                                  products = res$products,
                                  hit.log = res$product.hits)
  
  #for a change in speed
  res.speed<- speeddiscretisation(aisles.log=res.aisles$log,
                                  hits.log= res$product.hits,
                                  input.data = res$input.data,
                                  products= res$products,
                                  params= params)
  #calculate crossings
  res.cross <- getCrossings(input.data = res$input.data,
                            params= params,
                            products= res$products)
  
  #adds features to aisles data, move to aisles function
  # res.look<- getLookings( aisles.log= res.speed$aisles.log,
  #                         input.look= res$input.look.left,
  #                         aisles = params$features$aisles)

  log.list<- list(aisles.log= res.speed$aisles.log, 
                  speed.log= res.speed$speed.log, 
                  crossings.log= res.cross$log, 
                  products.log= res.products$log, 
                  walked.past.log= res.products$log.walked.past, 
                  products.hit.log= res$product.hits,
                  product.all= res$product.all)
  
  #make and save features 
  data<-  logs.to.features(data,i,log.list,res$input.data,res$products, params)
  
  #filter.feature.plot(res$input.data,res.aisles$log)
  #feature.plot(res$input.data, res.speed$speed.log, "all")
  
  #Do intermeadiate save of data
  if(params$save.feature){
    if( ! file.exists(paste0('output/',params$output.dir,'/csv_temp'))){
      dir.create(paste0('output/',params$output.dir,'/csv_temp'),recursive = TRUE)
    }
    write.csv2(data, file = paste0("output/",params$output.dir,"/csv_temp/data_until_file_", i, ".csv"), row.names = FALSE)
  }

  #write the log with the timestemsp of events
  if(params$save.log){

  #save the log to excel
  export.logs(id= id,params=params, log.list= log.list)
  #combine all partisipants log files to one df
  combined.logs<-all.logs(log.list, combined.logs, i, id)
  }
  #print pathplots
  if(params$save.images){
    basic<- basic.path.plot(res$input.data, id,params,save = TRUE)
    
    full<-full.plot(basic,res$input.data,log.list,id,params,res$products,res$productbox,params$features$aisles, save = TRUE)
    # plot places where somone looks
    looking.plot(res.speed$speed.log %>% filter(label== "stop with no movement"), 
                 res$input.data, res$input.look.left, id,params, full,"stop")
    
    looking.plot(res.aisles$log %>% filter(label == "walk through"| label == "same side in out"), 
                 res$input.data, res$input.look.left, id,params, full, "aisles")
    
    speed<-speed.plot(res$input.data,res.speed$speed.log, res.aisles$log,id=id, save = TRUE)
    speed.map.combine(speed,full,id,params,save=TRUE)
  }
  #plot.best.r(res$input.data, id)
}
#export all log files in one file
if(params$save.log){
  export.logs("allfilescombined_view_inproved",params,combined.logs)
}



# VR Supermarkt: Extract features
#
# Last edited 2019-07-03 by Chiel van Griethuijsen (m.a.vangriethuijsen@students.uu.nl)

# preamble ----------------------------------------------------------------
packages <- c("jsonlite", "tidyverse", "png", "ggforce",
              "ggalt", "Rcpp", "grid", "gganimate","XML", 
              "DescTools", "xlsx","ggpubr","LearnGeom","future.apply")

lapply(packages, require, character.only = TRUE)

source("code/config.R",encoding = "UTF-8") # load the configuration file
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
input.data.list.all<-list()
id.list<-c()

# loop over all the participants 
for(i in 1 : length(data.files)){
  print(paste('Calculating logs for file', i, "of",length(data.files) , "started at",Sys.time()))
  JSONfile <- data.files[i]
  id<- str_split(JSONfile,"_")[[1]][1]
  
  #load data, make datasets to use for analysis
  if(is.null(input.data.list.all[[id]])){
    res <- runFirstAnalyses(JSONfile = JSONfile, 
                            params= params)
    #put input data in one list to make it accasible later for dynemic testing
    input.data.list.all[id]<- list(res)
    id.list[i]<- id
  }else{
    res<- input.data.list.all[[id]]
  }

  #for a change in speed
  res.stops<- speeddiscretisation(hits.log= res$product.hits,
                                  input.data = res$input.data,
                                  input.look= res$input.look,
                                  products= res$products,
                                  params= params)
  res$input.data["stop"]<- datapoint.add.label(res$input.data,res.stops$log)$label
  #calculate start and stop points and add features
  #for entering a aislesbox
  res.aisles <- getAisleTimes(input.data= res$input.data,
                              input.look = res$input.look,
                              products= res$products,
                              params = params,
                              stops= res.stops$log)
  #for entering a productbox but the product is not picked  
  res.walkpast<- WalkpastProduct(input.data = res$input.data,
                                 input.look = res$input.look,
                                  productbox =  res$productbox,
                                  products = res$products,
                                  hit.log = res$product.hits,
                                  params = params)
  
  #if product is picked look at what is done during the time before
  res.products<- picked.products(input.data = res$input.data,
                                 view= res$input.look,
                                 products= res$product.hits,
                                 params= params)
  
  
  #walking
  res.walks<- walks(input.data=res$input.data, 
                    input.look=res$input.look, 
                    params=params, 
                    stops=res.stops$log,
                    products= res$products)
  
  #calculate crossings
  res.cross <- getCrossings(input.data = res$input.data,
                            params= params,
                            products= res$products)
  
  #adds features to aisles data, move to aisles function
  # res.look<- getLookings( aisles.log= res.speed$aisles.log,
  #                         input.look= res$input.look.left,
  #                         aisles = params$features$aisles)

  log.list<- list(aisles.log= res.aisles$log, 
                  stops.log= res.stops$log,
                  walks.log= res.walks$log,
                  crossings.log= res.cross$log, 
                  products.log= res.products$log, 
                  walked.past.log= res.walkpast$log.walked.past, 
                  products.hit.log= res$product.hits,
                  product.all= res$product.all)
  
  #make and save features 
  data<-  logs.to.features(data,i,log.list,res$input.data,res$products, params)
  
  #filter.feature.plot(res$input.data,res.aisles$log)
  feature.plot(res$input.data, res.stops$log, "all.stops",id)
  
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
    looking.plot(res.stops$log, 
                 res$input.data, res$input.look, id,params, full,"stop")
    
    looking.plot(res.aisles$log %>% filter(label == "walk through"| label == "same side in out"), 
                 res$input.data, res$input.look, id,params, full, "aisles")
    
    speed<-speed.plot(res$input.data,res.stops$log, res.aisles$log,id=id, save = TRUE)
    speed.map.combine(speed,full,id,params,save=TRUE)
  }
  #plot.best.r(res$input.data, id)
}
#export all log files in one file
if(params$save.log){
  export.logs("allfilescombined_view_inproved",params,combined.logs)
}



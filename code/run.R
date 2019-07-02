# VR Supermarkt: Extract features
#
# Last edited 2018-10-18 by Laurent Smeets (l.s.m.smeets@uu.nl)

# preamble ----------------------------------------------------------------
packages <- c("jsonlite", "tidyverse", "png", "ggforce",
              "ggalt", "Rcpp", "grid", "gganimate","XML", "DescTools", "xlsx","ggpubr")

lapply(packages, require, character.only = TRUE)



source("code/config.R") # load the configuration file
sapply(list.files("code/features", full.names = TRUE, '.R'), source) # load FE funcs
sourceCpp('code/features/cppDoLinesIntersect.cpp') # load the C function that calculates crossings


# Load image
image <- readPNG(paste0('input/', params$img.name))

# get data files
data.files <- list.files(
  path = file.path("input", params$input.dir),
  pattern = 'json',
  full.names = FALSE
)
data.logs <- list.files(
  path = file.path("input", params$input.dir),
  pattern = 'Log.xml',
  full.names = FALSE
)


# get the excel sheet
data.files2 <- list.files(
  path = file.path("input", params$input.dir),
  pattern = 'xlsx',
  full.names = FALSE
)

# load the excelsheet with the hits of the respondents
Excel<-readxl::read_excel(path= file.path("input", params$input.dir, data.files2),
                                          sheet=params$sheet.excel,
                                          n_max=params$n.row.excel)%>%
  mutate(ID = as.character(ID))



# Create data frame to save results
data <- createDataFrame(data.files)


# loop over all the participants 
for(i in 1 : length(data.files)){
  JSONfile <- data.files[i]
  input.log<- data.logs[i]
  
  res <- runFirstAnalyses(JSONfile = JSONfile, 
                          input.log = input.log,
                          Excel=Excel, 
                          image = image, 
                          params= params,
                          i = i)
  

  res.aisles <- getAisleTimes(input.data= res$input.data,
                              aisles = params$features$aisles,
                              full.images = params$full.images,
                              save.data = params$save.data,
                              i = i)
    
  res.products <- WalkpastProduct(input.data = res$input.data,
                                  products =  res$productbox,
                                  products2 = res$products,
                                  hit.log = res$product.hits,
                                  full.images = params$full.images,
                                  save.data = params$save.data,
                                  i = i)
  
  res.speed<- speeddiscretisation(aisles.log=res.aisles$log,
                                  hits.log= res$product.hits,
                                  input.data = res$input.data,
                                  stop.params = params$features$stops,
                                  walk.params = params$features$walk,
                                  aisles= params$features$aisles,
                                  i=i)
  
  res.cross <- getCrossings(input.data = res$input.data,
                            params= params,
                            i = i)

  res.look<- getLookings( aisles.log= res.speed$aisles.log,
                          input.look= res$input.look,
                          aisles = params$features$aisles)


  log.list<- list(aisles.log= res.speed$aisles.log, 
                  speed.log= res.speed$speed.log, 
                  crossings.log= res.cross$log, 
                  products.log= res.products$log, 
                  walked.past.log= res.products$log.walked.past, 
                  products.hit.log= res$product.hits,
                  look.log= res.look$log)
  
  #make and save features 
  data<-  logs.to.features(data,i,log.list,res$input.data,params)
  if(params$save.feature){
    if( ! file.exists(paste0('output/',params$output.dir,'/csv_temp'))){
      dir.create(paste0('output/',params$output.dir,'/csv_temp'),recursive = TRUE)
    }
    write.csv2(data, file = paste0("output/",params$output.dir,"/csv_temp/data_until_file_", i, ".csv"), row.names = FALSE)
  }

    
  #add the npo and persenal data to the export
  # if(params$external.excel){
  # datamerged<-add.npo.and.persenal.data(data,params,data.files2)
  # write.csv2(datamerged, file = paste0("output/csv_temp/data_until_file_", i, ".csv"), row.names = FALSE)
  # }

  #write the log with the timestemsp of events
  if(params$save.log){

  #save the log to excel
  export.logs(JSONfile= JSONfile,params=params, log.list= log.list)
  #combine all partisipants log files to one df
  combined.logs<-all.logs(log.list, combined.logs, i, str_split(JSONfile,"_")[[1]][1])
  }
  #print pathplots
  if(params$save.images){
    basic<- basic.path.plot(res$input.data, JSONfile,params,save = TRUE)
    
    full<-full.plot(basic,res$input.data,log.list,JSONfile,params,res$products,res$productbox,params$features$aisles, save = TRUE)
    # plot places where somone looks
    looking.plot.stop(res.speed$speed.log, res$input.data, res$input.look, JSONfile,params, full)
    looking.plot.aisles(res.aisles$log, res$input.data, res$input.look, JSONfile,params, full)
    
    speed<-speed.plot(res$input.data,res.speed$speed.log, res.aisles$log)
    speed.map.combine(speed,full,JSONfile,params,save=TRUE)
  }
  
}
#export all log files in one file
if(params$save.log){
  export.logs("allfilescombined",params,combined.logs)
}
# save the data to an excel sheet
# if(params$save.to.excel){
#   write.csv2(datamerged, file = "output/csv/features.csv", row.names = FALSE)
# }


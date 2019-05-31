# VR Supermarkt: Extract features
#
# Last edited 2018-10-18 by Laurent Smeets (l.s.m.smeets@uu.nl)

# preamble ----------------------------------------------------------------
packages <- c("jsonlite", "tidyverse", "png", "ggforce",
              "ggalt", "Rcpp", "grid", "gganimate","XML", "DescTools", "xlsx")

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
                          data = data,
                          i = i)
  

  if(params$save.data | params$full.images){
    res.aisles <- getAisleTimes(data = res$data,
                                input.data= res$input.data,
                                aisles = params$features$aisles,
                                full.images = params$full.images,
                                save.data = params$save.data,
                                i = i)
    
    res.products <- WalkpastProduct(data = res.aisles$data,
                                    input.data = res$input.data,
                                    products =  res$productbox,
                                    products2 = res$products,
                                    hit.log = res$product.hits,
                                    full.images = params$full.images,
                                    save.data = params$save.data,
                                    i = i)
    
    res.speed<- speeddiscretisation(data = res.products$data,
                                    aisles.log=res.aisles$log,
                                    hits.log= res$product.hits,
                                    input.data = res$input.data,
                                    stop.params = params$features$stops,
                                    walk.params = params$features$walk,
                                    aisles= params$features$aisles,
                                    i=i)
    
    res.cross <- getCrossings(data = res.speed$data,
                              input.data = res$input.data,
                              shopping.aisle.time.points = res.aisles$shopping.aisle.time.points,
                              aisles = params$features$aisles,
                              cross.lag1 = params$features$cross$cross.lag1,
                              cross.lag2 = params$features$cross$cross.lag2,
                              full.images = params$full.images,
                              i = i)

    res.look<- getLookings( aisles.log= res.aisles$log,
                            input.look= res$input.look,
                            aisles = params$features$aisles)

    data<-  res.cross$data
    
    


      
    #add the npo and persenal data to the export
    # if(params$external.excel){
    # datamerged<-add.npo.and.persenal.data(data,params,data.files2)
    # write.csv2(datamerged, file = paste0("output/csv_temp/data_until_file_", i, ".csv"), row.names = FALSE)
    # }
    
    #write the log with the timestemsp of events
    if(params$save.log){
    log.list<- list(aisles.log= res.speed$aisles.log, 
                    speed.log= res.speed$speed.log, 
                    crossings.log= res.cross$log, 
                    products.log= res.products$log, 
                    walked.past.log= res.products$log.walked.past, 
                    products.hit.log= res$product.hits)
    #save the log to excel
    export.logs(Jsonfile= Jsonfile, log.list= log.list)
    #combine all partisipants log files to one df
    combined.logs<-all.logs(log.list, combined.logs, i, str_split(JSONfile,"_")[[1]][1])
    }
    #print pathplots
    if(params$save.images){
    basic<- basic.path.plot(res$input.data, JSONfile,save = TRUE)
    
    full<-full.plot(basic,res$input.data,log.list,JSONfile,res$products,res$productbox,params$features$aisles, save = TRUE)
    # plot places where somone looks
    looking.plot.stop(res.speed$speed.log, res$input.data, res$input.look, JSONfile, full)
    looking.plot.aisles(res.aisles$log, res$input.data, res$input.look, JSONfile, full)
    
    #plot the speed, with categories of stop/slow/walk
    ggsave(paste0('output/png/', params$output.dir, '/', JSONfile,'speed', '.png'), 
           speed.plot(res$input.data,res.speed$speed.log, res.aisles$log), width = 40, height = 7, units = 'cm')
    }
  }
}
#export all log files in one file
if(params$save.log){
  export.logs("allfilescombined",combined.logs)
}
# save the data to an excel sheet
# if(params$save.to.excel){
#   write.csv2(datamerged, file = "output/csv/features.csv", row.names = FALSE)
# }


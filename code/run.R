# VR Supermarkt: Extract features
#
# Last edited 2018-10-18 by Laurent Smeets (l.s.m.smeets@uu.nl)

# preamble ----------------------------------------------------------------
packages <- c("jsonlite", "tidyverse", "png", "ggforce",
              "ggalt", "Rcpp", "grid", "gganimate")

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
  
  res <- runFirstAnalyses(JSONfile = JSONfile, Excel=Excel, 
                          image = image, 
                          params= params,
                          data = data,
                          i = i)
  

  if(params$save.data | params$full.images){
    res.aisles <- getAisleTimes(data = res$data,
                                input.data= res$input.data,
                                gg = res$gg,
                                aisles = params$features$aisles,
                                full.images = params$full.images,
                                save.data = params$save.data,
                                i = i)
    
    res.products <- WalkpastProduct(data = res.aisles$data,
                                    input.data = res$input.data,
                                    gg = res.aisles$gg,
                                    products =  res$productbox,
                                    products2 = res$products, 
                                    full.images = params$full.images,
                                    save.data = params$save.data,
                                    i = i)
    
    res.speed<- speeddiscretisation(data = res.products$data,
                                    aisles.log<- res.aisles$log,
                                    input.data = res$input.data,
                                    gg=res.products$gg.products,
                                    stop.params = params$features$stops,
                                    walk.params = params$features$walk,
                                    aisles= params$features$aisles,
                                    i=i)
    
    res.cross <- getCrossings(data = res.speed$data,
                              input.data = res$input.data,
                              gg = res.speed$gg,
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
    
    looking.plot.stop(res.speed$log, res$input.data, res$input.look, JSONfile, res.cross$gg)
    looking.plot.aisles(res.aisles$log, res$input.data, res$input.look, JSONfile, res.cross$gg)
 
      if(params$full.images){
      if( ! file.exists(paste0('output/png/', params$output.dir))){
        dir.create(paste0('output/png/', params$output.dir))
      }
      # Save variables required for ggplot in global environment 
      # (ggplot cannot handle variables created in functions)
      aisles <- params$features$aisles
      aisles2 <- params$features$aisles2
      n.crossings <- res.cross$n.crossings
      stop.radius <- params$features$stops$stop.radius
      productsbox <- params$features$products1
      productslocation <- params$features$products2
      n.stops<-res.speed$n.stops
      n.slows<-res.speed$n.slows
      ggsave(paste0('output/png/', params$output.dir, '/', JSONfile, '.png'), 
             res.cross$gg.cross, width = 37.5, height = 21, units = 'cm')
      
      
      #add the npo and persenal data to the export
      if(params$external.excel){
      datamerged<-add.npo.and.persenal.data(data,params,data.files2)
      write.csv2(datamerged, file = paste0("output/csv_temp/data_until_file_", i, ".csv"), row.names = FALSE)
      }
      
      #plot the speed, with categories of stop/slow/walk
      ggsave(paste0('output/png/', params$output.dir, '/', JSONfile,'speed', '.png'), 
      speed.plot(res$input.data,res.speed$log, res.aisles$log), width = 40, height = 7, units = 'cm')
      
      #write the log with the timestemsp of events
      write.csv2(res.speed$log, file = paste0("output/logs/",strsplit(JSONfile,"_")[[1]][1],"_log", ".csv"), row.names = FALSE)
    }
  }
}

# save the data to an excel sheet
if(params$save.to.excel){
  write.csv2(datamerged, file = "output/csv/features.csv", row.names = FALSE)
}


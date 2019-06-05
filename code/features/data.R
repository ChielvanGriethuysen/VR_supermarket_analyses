createDataFrame <- function(data.files){
  
  data <- data.frame(name = character(length(data.files)),          # name of input file
                     ID= character(length(data.files)),             # Identification variable
                     total.time = numeric(length(data.files)),      # total time spent on task
                     total.distance = numeric(length(data.files)),  # needs to be fixed.
                     average.speed = numeric(length(data.files)),   # average speed of user
                     
                     n.crossings = numeric(length(data.files)),     # total number of crossings someone made (crossing his/her own path)
                     n.crossings.dist.15= numeric(length(data.files)),
                     n.crossings.dist.20= numeric(length(data.files)),
                     n.crossings.dist.25=numeric(length(data.files)),
                    
                     n.crossings.outside.aisles = numeric(length(data.files)), # how many crossings did make outside of the horizontal shopping aisles
                     n.crossings.dist.15.outside.aisles= numeric(length(data.files)),
                     n.crossings.dist.20.outside.aisles= numeric(length(data.files)),
                     n.crossings.dist.25.outside.aisles=numeric(length(data.files)),
                     
                     # n.stops= numeric(length(data.files)),             # number of stops someone made
                     # n.stops.min.2.sec= numeric(length(data.files)), 
                     # n.stops.min.4.sec= numeric(length(data.files)), 
                     # n.stops.min.6.sec= numeric(length(data.files)), 
                     # n.stops.min.8.sec= numeric(length(data.files)), 
                     
                     # n.stops.min.2.max8.sec.speed.0_5= numeric(length(data.files)), 
                     # n.stops.min.3.max8.sec.speed.0_5= numeric(length(data.files)), 
                     # n.stops.min.4.max8.sec.speed.0_5= numeric(length(data.files)), 
                     #n.stops.min.8.sec.speed.0_5= numeric(length(data.files)), 
                     
                     n.stops.min.2.max6.sec.speed.g75= numeric(length(data.files)), 
                     n.stops.min.2.max6.sec.speed.l75= numeric(length(data.files)),
                     # n.stops.min.3.max6.sec.speed.0_5= numeric(length(data.files)), 
                     # n.stops.min.4.max6.sec.speed.0_5= numeric(length(data.files)), 
                     #n.stops.min.8.sec.speed.0_10= numeric(length(data.files)), 
                     
                     # n.stops.min.2.sec.speed.10_15= numeric(length(data.files)), 
                     # n.stops.min.4.sec.speed.10_15= numeric(length(data.files)), 
                     # n.stops.min.6.sec.speed.10_15= numeric(length(data.files)), 
                     # n.stops.min.8.sec.speed.10_15= numeric(length(data.files)), 
                      
                     # n.stops.min.6.sec.speed.g15= numeric(length(data.files)), 
                     # n.stops.min.6.sec.speed.g10= numeric(length(data.files)), 
                     n.stops.min.6.sec.speed.g75= numeric(length(data.files)), 
                     n.stops.min.6.sec.speed.l75= numeric(length(data.files)), 
                     
                     # n.stops.min.8.sec.speed.g15= numeric(length(data.files)), 
                     # n.stops.min.8.sec.speed.g10= numeric(length(data.files)), 
                     # n.stops.min.8.sec.speed.g5= numeric(length(data.files)), 
                     
                     total.stoping.time= numeric(length(data.files)),  # total stopping time
                     
                     n.walked.through.aisles= numeric(length(data.files)), #nr of times walked through a aisles
                     n.walked.in.out.aisles= numeric(length(data.files)), #nr of times walked in an aisles and the same side out
                     
                     Hit_Totaal = numeric(length(data.files)),        # total items someone picked up
                     stringsAsFactors = FALSE
  )
  return(data)
}

logs.to.features<- function(data,i, log.list,input.data, params){
  
  data$name[i] = JSONfile
  data$ID[i]= substr(JSONfile,1,6)
  data$total.time[i] = last(input.data$time) - input.data$time[1]
  data$total.distance[i] = sum(input.data$dist)
  data$average.speed[i] = data$total.distance[i] / data$total.time[i]
  
  data$n.crossings[i] = nrow(log.list$crossings.log)
  data$n.crossings.dist.15[i]= nrow(log.list$crossings.log %>% filter(absolute.dist>15))
  data$n.crossings.dist.20[i]= nrow(log.list$crossings.log %>% filter(absolute.dist>20))
  data$n.crossings.dist.25[i]=nrow(log.list$crossings.log %>% filter(absolute.dist>25))
  
  data$n.crossings.outside.aisles[i] = nrow(log.list$crossings.log %>% filter(aisles.type== "main"))
  data$n.crossings.dist.15.outside.aisles[i]= nrow(log.list$crossings.log %>% filter(aisles.type== "main"& absolute.dist>15))
  data$n.crossings.dist.20.outside.aisles[i]= nrow(log.list$crossings.log %>% filter(aisles.type== "main"& absolute.dist>20))
  data$n.crossings.dist.25.outside.aisles[i]=nrow(log.list$crossings.log %>% filter(aisles.type== "main"& absolute.dist>25))
  
  # data$n.stops.min.2.max8.sec.speed.0_5[i]= nrow(log.list$speed.log%>% filter(label=="stop"& time.spend>2&time.spend<8&absolute.speed<0.075))
  # data$n.stops.min.3.max8.sec.speed.0_5[i]= nrow(log.list$speed.log%>% filter(label=="stop"& time.spend>3&time.spend<8&absolute.speed<0.075))
  # data$n.stops.min.4.max8.sec.speed.0_5[i]= nrow(log.list$speed.log%>% filter(label=="stop"& time.spend>4&time.spend<8&absolute.speed<0.075))
  
  data$n.stops.min.2.max6.sec.speed.g75[i]= nrow(log.list$speed.log%>% filter(label=="stop"& time.spend>2&time.spend<6&absolute.speed<0.075))
  # data$n.stops.min.3.max6.sec.speed.0_5[i]= nrow(log.list$speed.log%>% filter(label=="stop"& time.spend>3&time.spend<6&absolute.speed<0.075))
  # data$n.stops.min.4.max6.sec.speed.0_5[i]= nrow(log.list$speed.log%>% filter(label=="stop"& time.spend>4&time.spend<6&absolute.speed<0.075))
  data$n.stops.min.2.max6.sec.speed.l75[i]= nrow(log.list$speed.log%>% filter(label=="stop"& time.spend>2&time.spend<6&absolute.speed<0.075))
  
  # data$n.stops.min.6.sec.speed.g15[i]= nrow(log.list$speed.log%>% filter(label=="stop"& time.spend>6& absolute.speed>0.15))
  # data$n.stops.min.6.sec.speed.g10[i]= nrow(log.list$speed.log%>% filter(label=="stop"& time.spend>6& absolute.speed>0.10))
  data$n.stops.min.6.sec.speed.g75[i]= nrow(log.list$speed.log%>% filter(label=="stop"& time.spend>6& absolute.speed>0.075))
  
  data$n.stops.min.6.sec.speed.l75[i]= nrow(log.list$speed.log%>% filter(label=="stop"& time.spend>6& absolute.speed<0.075))
  
  
  # data$n.stops.min.8.sec.speed.g15[i]= nrow(log.list$speed.log%>% filter(label=="stop"& time.spend>8& absolute.speed>0.15))
  # data$n.stops.min.8.sec.speed.g10[i]= nrow(log.list$speed.log%>% filter(label=="stop"& time.spend>8& absolute.speed>0.10))
  # data$n.stops.min.8.sec.speed.g5[i]= nrow(log.list$speed.log%>% filter(label=="stop"& time.spend>8& absolute.speed>0.075))
  
  data$total.stoping.time[i]= sum(log.list$speed.log%>% filter(label=="stop")%>% select(time.spend))
  
  data$n.walked.through.aisles[i]= log.list$aisles.log%>% filter(label== "walk through") %>% nrow()
  data$n.walked.in.out.aisles[i]= log.list$aisles.log%>% filter(label== "same side in out") %>% nrow()
  
  data$Hit_Totaal[i] = length(unique(log.list$products.hit.log$prod_id))
  return(data)
}


data.basic<-function(data, input.data,i){
  data$name[i] <- JSONfile
  data$ID[i]<-substr(JSONfile,1,6)
  data$total.time[i] <- last(input.data$time) - input.data$time[1]
  data$total.distance[i] <- sum(input.data$dist)
  data$n.datapoints[i] <- length(input.data$time)
  data$datapoints.per.second[i] <- length(input.data$time) / (last(input.data$time) - input.data$time[1])
  data$average.speed[i]<- data$total.distance[i] / data$total.time[i]
  
  data$max.difference.between.points[i] <-  max(abs(diff(input.data$dist)))
  data$qt95.difference.between.points[i] <-quantile(abs(diff(input.data$dist)), probs = 0.95)
  data$qt99.difference.between.points[i]  <-quantile(abs(diff(input.data$dist)), probs = 0.99)
  data$max.difference.between.timepoints[i] <- max(diff(input.data$time))
  data$qt95.difference.between.timepoints[i] <- quantile(diff(input.data$time),probs = 0.95)
  data$qt99.difference.between.timepoints[i]<- quantile(diff(input.data$time),probs = 0.99)
  
  return(data)
}

data.VRlog<-function(data, Excel, i){
  data$Hit_Totaal[i]<-filter(Excel, ID== data$ID[i])$Hit_Totaal
  data$Avatars[i]<- filter(Excel, ID== data$ID[i])$Avatars
  data$VR_aborted[i]<-filter(Excel, ID== data$ID[i])$VR_aborted
  data$Tijd[i]<-filter(Excel, ID== data$ID[i])$Tijd
  data$Interference[i]<-filter(Excel, ID== data$ID[i])$Interference
  data$Hit_1[i]<-   filter(Excel, ID== data$ID[i])$Hit_1
  data$Hit_2[i]<-   filter(Excel, ID== data$ID[i])$Hit_2
  data$Hit_3[i]<-   filter(Excel, ID== data$ID[i])$Hit_3
  data$Hit_4[i]<-   filter(Excel, ID== data$ID[i])$Hit_4
  data$Hit_5[i]<-   filter(Excel, ID== data$ID[i])$Hit_5
  data$Hit_6[i]<-   filter(Excel, ID== data$ID[i])$Hit_6
  data$Hit_7[i]<-   filter(Excel, ID== data$ID[i])$Hit_7
  data$Hit_8[i]<-   filter(Excel, ID== data$ID[i])$Hit_8
  data$Tijd_H1[i]<- filter(Excel, ID== data$ID[i])$Tijd_H1
  data$Tijd_H2[i]<- filter(Excel, ID== data$ID[i])$Tijd_H2
  data$Tijd_H3[i]<- filter(Excel, ID== data$ID[i])$Tijd_H3
  data$Tijd_H4[i]<- filter(Excel, ID== data$ID[i])$Tijd_H4
  data$Tijd_H5[i]<- filter(Excel, ID== data$ID[i])$Tijd_H5
  data$Tijd_H6[i]<- filter(Excel, ID== data$ID[i])$Tijd_H6
  data$Tijd_H7[i]<- filter(Excel, ID== data$ID[i])$Tijd_H7
  data$Tijd_H8[i]<- filter(Excel, ID== data$ID[i])$Tijd_H8
  data$FA_Totaal[i]<-filter(Excel, ID== data$ID[i])$FA_Totaal
  data$FA_Time[i]<- filter(Excel, ID== data$ID[i])$FA_Time
  data$Kassa[i]<-   filter(Excel, ID== data$ID[i])$Kassa
  
  
  
  return(data)
  
}
add.npo.and.persenal.data<-function(data,params, data.file ){
  
  
  # merge data from other excel sheets with other test results
  Excel.personal <- readxl::read_excel(path = file.path("input", params$input.dir, data.file),
                                       sheet = params$sheet.excel2,
                                       range = paste0(params$range.personal, params$n.row.excel)) %>%
    mutate(ID = as.character(ID))
  
  Excel.NPO<-readxl::read_excel(path  = file.path("input", params$input.dir, data.file),
                                sheet = params$sheet.excel3,
                                range = paste0(params$range.NPO, params$n.row.excel))%>%
    mutate(ID = as.character(ID))
  
  # for some reason distance doesnt really work yet so it is calculated here
  datamerged <-
    left_join(data, select(Excel.personal, -VR_aborted, -Avatars), by = "ID" ) %>%
    left_join(select(Excel.NPO, -education, -age), by = "ID" ) %>%
    mutate(distance = total.time*average.speed)
  return(datamerged)
}

export.logs<- function(JSONfile, log.list){
  file <- paste0("output/logs/",strsplit(JSONfile,"_")[[1]][1],"_log",".xlsx")
  
  write.xlsx2(log.list$aisles.log,      file = file, sheetName = "aisles")
  write.xlsx2(log.list$speed.log,       file = file, sheetName = "speed", append = TRUE)
  write.xlsx2(log.list$crossings.log,   file = file, sheetName = "crossings", append = TRUE)
  write.xlsx2(log.list$products.log,    file = file, sheetName = "products", append = TRUE)
  write.xlsx2(log.list$walked.past.log, file = file, sheetName = "walked.past", append = TRUE)
  write.xlsx2(log.list$products.hit.log,file = file, sheetName = "hit", append = TRUE)
  write.xlsx2(log.list$look.log,file = file, sheetName = "look", append = TRUE)
  
}
all.logs<- function(log, combined.logs,i,file){
  if(i==1){
    log$aisles.log<- cbind(rep(file,nrow(log$aisles.log)),log$aisles.log)
    log$speed.log<- cbind(rep(file,nrow(log$speed.log)),log$speed.log)
    log$crossings.log<- cbind(rep(file,nrow(log$crossings.log)),log$crossings.log)
    log$products.log<- cbind(rep(file,nrow(log$products.log)),log$products.log)
    log$walked.past.log<- cbind(rep(file,nrow(log$walked.past.log)),log$walked.past.log)
    log$products.hit.log<- cbind(rep(file,nrow(log$products.hit.log)),log$products.hit.log)
    log$look.log<- cbind(rep(file,nrow(log$look.log)),log$look.log)
    return(log)
  }else{
    combined.logs$aisles.log<- rbind(combined.logs$aisles.log,cbind(rep(file,nrow(log$aisles.log)),log$aisles.log))
    combined.logs$speed.log<- rbind(combined.logs$speed.log,cbind(rep(file,nrow(log$speed.log)),log$speed.log))
    combined.logs$crossings.log<- rbind(combined.logs$crossings.log,cbind(rep(file,nrow(log$crossings.log)),log$crossings.log))
    combined.logs$products.log<- rbind(combined.logs$products.log,cbind(rep(file,nrow(log$products.log)),log$products.log))
    combined.logs$walked.past.log<- rbind(combined.logs$walked.past.log,cbind(rep(file,nrow(log$walked.past.log)),log$walked.past.log))
    combined.logs$products.hit.log<- rbind(combined.logs$products.hit.log,cbind(rep(file,nrow(log$products.hit.log)),log$products.hit.log))
    combined.logs$look.log<- rbind(combined.logs$look.log,cbind(rep(file,nrow(log$look.log)),log$look.log))
    return(combined.logs)
  }
}


  
  
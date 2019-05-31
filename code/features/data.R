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

export.logs<- function(Jsonfile, log.list){
  file <- paste0("output/logs/",strsplit(JSONfile,"_")[[1]][1],"_log",".xlsx")
  
  write.xlsx2(log.list$aisles.log,      file = file, sheetName = "aisles")
  write.xlsx2(log.list$speed.log,       file = file, sheetName = "speed", append = TRUE)
  write.xlsx2(log.list$crossings.log,   file = file, sheetName = "crossings", append = TRUE)
  write.xlsx2(log.list$products.log,    file = file, sheetName = "products", append = TRUE)
  write.xlsx2(log.list$walked.past.log, file = file, sheetName = "walked.past", append = TRUE)
  write.xlsx2(log.list$products.hit.log,file = file, sheetName = "hit", append = TRUE)
  
}
all.logs<- function(log, combined.logs,i,file){
  if(i==1){
    log$aisles.log<- cbind(rep(file,nrow(log$aisles.log)),log$aisles.log)
    log$speed.log<- cbind(rep(file,nrow(log$speed.log)),log$speed.log)
    log$crossings.log<- cbind(rep(file,nrow(log$crossings.log)),log$crossings.log)
    log$products.log<- cbind(rep(file,nrow(log$products.log)),log$products.log)
    log$walked.past.log<- cbind(rep(file,nrow(log$walked.past.log)),log$walked.past.log)
    log$products.hit.log<- cbind(rep(file,nrow(log$products.hit.log)),log$products.hit.log)
    return(log)
  }else{
    combined.logs$aisles.log<- rbind(combined.logs$aisles.log,cbind(rep(file,nrow(log$aisles.log)),log$aisles.log))
    combined.logs$speed.log<- rbind(combined.logs$speed.log,cbind(rep(file,nrow(log$speed.log)),log$speed.log))
    combined.logs$crossings.log<- rbind(combined.logs$crossings.log,cbind(rep(file,nrow(log$crossings.log)),log$crossings.log))
    combined.logs$products.log<- rbind(combined.logs$products.log,cbind(rep(file,nrow(log$products.log)),log$products.log))
    combined.logs$walked.past.log<- rbind(combined.logs$walked.past.log,cbind(rep(file,nrow(log$walked.past.log)),log$walked.past.log))
    combined.logs$products.hit.log<- rbind(combined.logs$products.hit.log,cbind(rep(file,nrow(log$products.hit.log)),log$products.hit.log))
    return(combined.logs)
  }
}


  
  
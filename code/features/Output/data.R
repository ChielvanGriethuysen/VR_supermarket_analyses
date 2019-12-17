#
#
# Last edited 2019-07-03 by Chiel van Griethuijsen (m.a.vangriethuijsen@students.uu.nl)

createDataFrame <- function(data.files){
  
  data <- data.frame(name = character(length(data.files)),          # name of input file
                     ID= character(length(data.files)),             # Identification variable
                     total.time = numeric(length(data.files)),      # total time spent on task
                     total.distance = numeric(length(data.files)),  # needs to be fixed.
                     average.speed = numeric(length(data.files)),   # average speed of user
                     
                     r.x = numeric(length(data.files)),
                     r.z = numeric(length(data.files)),
                     r.best= numeric(length(data.files)),
                     
                     time.target=numeric(length(data.files)),
                     time.none.target=numeric(length(data.files)),
                     target.fraction= numeric(length(data.files)),
                     
                     n.crossings = numeric(length(data.files)),     # total number of crossings someone made (crossing his/her own path)
                    
                     n.crossings.outside.aisles = numeric(length(data.files)), # how many crossings did make outside of the horizontal shopping aisles
                     n.crossings.inside.aisles = numeric(length(data.files)),
                     n.crossings.target= numeric(length(data.files)),
                     n.crossings.non.target= numeric(length(data.files)),
                     
                     n.target.enter = numeric(length(data.files)),
                     n.non.target.enter = numeric(length(data.files)),
                     n.enter.fraction = numeric(length(data.files)),
                     
                     n.target.enter.unique = numeric(length(data.files)),
                     n.non.target.enter.unique = numeric(length(data.files)),
                     n.enter.fraction.unique = numeric(length(data.files)),
                     
                     n.revisit = numeric(length(data.files)),
                     n.revisit.target = numeric(length(data.files)),
                     n.revisit.non.target = numeric(length(data.files)),
                     n.revisit.fraction = numeric(length(data.files)),
                     
                     distance.between.target.products.mean = numeric(length(data.files)),
                     distance.between.target.products.sd  = numeric(length(data.files)),
                     
                     distance.between.related.products.mean = numeric(length(data.files)),
                     distance.between.related.products.sd  = numeric(length(data.files)),
                     
                     distance.between.aisles.mean = numeric(length(data.files)),
                     distance.between.aisles.sd = numeric(length(data.files)),
                     
                     
                    
                     
                     total.stoping.time= numeric(length(data.files)),  # total stopping time
                     
                     n.walked.through.aisles= numeric(length(data.files)), #nr of times walked through a aisles
                     n.walked.in.out.aisles= numeric(length(data.files)), #nr of times walked in an aisles and the same side out
                     
                     Hit_Totaal.target = numeric(length(data.files)),        # total items someone picked up
                     Hit_Totaal.related = numeric(length(data.files)), 
                     Hit_Totaal.all = numeric(length(data.files)), 
                     
                     stringsAsFactors = FALSE
  )
  return(data)
}

logs.to.features<- function(data,i, log.list,input.data,products ,params){
  
  data$name[i] = JSONfile
  data$ID[i]= substr(JSONfile,1,6)
  data$total.time[i] = last(input.data$time) - input.data$time[1]
  data$total.distance[i] = sum(input.data$dist)
  data$average.speed[i] = data$total.distance[i] / data$total.time[i]
  
  data$r.x[i] = cor(input.data$time, input.data$x)
  data$r.z[i] = cor(input.data$time, input.data$z)
  data$r.best[i]= max(data$r.x[i],data$r.z[i])
  
  data$time.target[i]=(log.list$aisles.log %>% filter(target==TRUE))$time.spent %>%sum()
  data$time.none.target[i]=(log.list$aisles.log %>% filter(target==FALSE))$time.spent %>%sum()
  data$target.fraction[i]= data$time.target[i]/data$time.none.target[i]
  
  data$n.crossings[i] = nrow(log.list$crossings.log)
  
  data$n.crossings.outside.aisles[i] = nrow(log.list$crossings.log %>% filter(aisles.type== "main"))
  
  data$n.crossings.inside.aisles[i] = nrow(log.list$crossings.log %>% filter(aisles.type== "shopping"))
  data$n.crossings.target[i]= nrow(log.list$crossings.log %>% filter(aisles.type== "shopping", target== TRUE))
  data$n.crossings.non.target[i]= nrow(log.list$crossings.log %>% filter(aisles.type== "shopping", target== FALSE))
  
  aisles<- log.list$aisles.log %>% group_by(aisles.name) %>% summarise(target= first(target),
                                                                       visits=n(),
                                                                       main= first(label)=="main")
  
  data$n.target.enter[i] = sum((aisles %>% filter(target== TRUE, main==FALSE))$visits)
  data$n.non.target.enter[i] = sum((aisles %>% filter(target== FALSE, main==FALSE))$visits)
  data$n.enter.fraction[i] = data$n.target.enter[i]/data$n.non.target.enter[i]
  
  data$n.target.enter.unique[i] = nrow(aisles %>% filter(target== TRUE, main==FALSE))
  data$n.non.target.enter.unique[i] = nrow(aisles %>% filter(target== FALSE, main==FALSE))
  data$n.enter.fraction.unique[i] = data$n.target.enter.unique[i]/data$n.non.target.enter.unique[i]
  
  data$n.revisit[i] = nrow(aisles %>% filter(main==FALSE, visits>1))
  data$n.revisit.target[i] = nrow(aisles %>% filter(main==FALSE, visits>1,target== TRUE))
  data$n.revisit.non.target[i] = nrow(aisles %>% filter(main==FALSE, visits>1,target== FALSE))
  data$n.revisit.fraction[i] = data$n.revisit.target[i]/data$n.revisit.non.target[i]
  
  DBAE<- distance.between.aisles.enters(input.data,log.list$aisles.log)
  
  data$distance.between.aisles.mean[i] = mean(DBAE)
  data$distance.between.aisles.sd[i] = sd(DBAE)
  
  # product.hits<- cbind(log.list$product.all,
  #                      calc.spot.event.in.box(data.frame(x.start=log.list$product.all$x,z.start= log.list$product.all$z),params$features$aisles))
  # product.hits<-add.product.hit.position(product.hits,input.data)
  
  
  product.hits<- filter.product.hits(products,log.list$product.all)

  product.hits.target<- calc.short.dist(params$features$aisles, product.hits$hit.target, input.data)
  
  
  data$distance.between.target.products.mean[i]= product.hits.target[-nrow(product.hits.target),]$dist.frac %>% mean()
  data$distance.between.target.products.sd[i]= product.hits.target[-nrow(product.hits.target),]$dist.frac %>% sd()
  
  product.hits.all<- rbind(product.hits$hit.target,product.hits$hit.related)%>% arrange(time)
  product.hits.all<- calc.short.dist(params$features$aisles, product.hits.all, input.data)
  
  
  data$distance.between.related.products.mean[i]= product.hits.all[-nrow(product.hits.all),]$dist.frac %>% mean()
  data$distance.between.related.products.sd[i]= product.hits.all[-nrow(product.hits.all),]$dist.frac %>% sd()
  
  
  
  
  data$total.stoping.time[i]= log.list$stops.log%>% group_by(id) %>% summarise(time.spent= first(time.spent)) %>% sum()
  
  data$n.walked.through.aisles[i]= log.list$aisles.log%>% filter(aisles.type== "shopping", label== "walk through") %>% nrow()
  data$n.walked.in.out.aisles[i]= log.list$aisles.log%>% filter(aisles.type== "shopping",label== "same side in out") %>% nrow()
  
  data$Hit_Totaal.target[i] = length(unique(product.hits$hit.target$prod_id))
  data$Hit_Totaal.related[i]=length(unique(product.hits$hit.related$prod_id))
  data$Hit_Totaal.all[i]=length(unique(log.list$product.all$prod_id))
  return(data)
}


data.basic<-function(data, input.data,i){
  data$name[i] <- JSONfile
  data$ID[i]<-strsplit(JSONfile,"_")[[1]][1]
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

export.logs<- function(id,params, log.list){
  if( ! file.exists(paste0('output/',params$output.dir,'/logs'))){
    dir.create(paste0('output/',params$output.dir,'/logs'))
  }
  file <- paste0("output/",params$output.dir,"/logs/",id,"_log",".xlsx")
  #options(java.parameters = "-Xmx1024m")
  #gc() for combined file to overcome memory error
  write.xlsx2(log.list$aisles.log,      file = file, sheetName = "aisles")
  gc()
  write.xlsx2(log.list$stops.log,       file = file, sheetName = "stops", append = TRUE)
  gc()
  write.xlsx2(log.list$walks.log,       file = file, sheetName = "walks", append = TRUE)
  gc()
  write.xlsx2(log.list$crossings.log,   file = file, sheetName = "crossings", append = TRUE)
  gc()
  write.xlsx2(log.list$products.log,    file = file, sheetName = "products", append = TRUE)
  gc()
  write.xlsx2(log.list$walked.past.log, file = file, sheetName = "walked.past", append = TRUE)
  gc()
  write.xlsx2(log.list$products.hit.log,    file = file, sheetName = "target.hit", append = TRUE)
  gc()
  write.xlsx2(log.list$product.all,    file = file, sheetName = "all.hit", append = TRUE)
  
}
all.logs<- function(log, combined.logs,i,file){
  status<-participant.category(file)
  
  if(i==1){
    
    log$aisles.log<- cbind(status=rep(status,nrow(log$aisles.log)),p.id=rep(file,nrow(log$aisles.log)),log$aisles.log)
    log$stops.log<- cbind(status=rep(status,nrow(log$stops.log)),p.id=rep(file,nrow(log$stops.log)),log$stops.log)
    log$walks.log<- cbind(status=rep(status,nrow(log$walks.log)),p.id=rep(file,nrow(log$walks.log)),log$walks.log)
    log$crossings.log<- cbind(status=rep(status,nrow(log$crossings.log)),p.id=rep(file,nrow(log$crossings.log)),log$crossings.log)
    log$products.log<- cbind(status=rep(status,nrow(log$products.log)),p.id=rep(file,nrow(log$products.log)),log$products.log)
    log$walked.past.log<- cbind(status=rep(status,nrow(log$walked.past.log)),p.id=rep(file,nrow(log$walked.past.log)),log$walked.past.log)
    log$products.hit.log<- cbind(status=rep(status,nrow(log$products.hit.log)),p.id=rep(file,nrow(log$products.hit.log)),log$products.hit.log)
    log$product.all<- cbind(status=rep(status,nrow(log$product.all)),p.id=rep(file,nrow(log$product.all)),log$product.all)
    return(log)
  }else{
    combined.logs$aisles.log<- rbind(combined.logs$aisles.log,             cbind(status=rep(status,nrow(log$aisles.log)),p.id=rep(file,nrow(log$aisles.log)),log$aisles.log))
    combined.logs$stops.log<- rbind(combined.logs$stops.log,               cbind(status=rep(status,nrow(log$stops.log)),p.id=rep(file,nrow(log$stops.log)),log$stops.log))
    combined.logs$walks.log<- rbind(combined.logs$walks.log,               cbind(status=rep(status,nrow(log$walks.log)),p.id=rep(file,nrow(log$walks.log)),log$walks.log))
    combined.logs$crossings.log<- rbind(combined.logs$crossings.log,       cbind(status=rep(status,nrow(log$crossings.log)),p.id=rep(file,nrow(log$crossings.log)),log$crossings.log))
    combined.logs$products.log<- rbind(combined.logs$products.log,         cbind(status=rep(status,nrow(log$products.log)),p.id=rep(file,nrow(log$products.log)),log$products.log))
    combined.logs$walked.past.log<- rbind(combined.logs$walked.past.log,   cbind(status=rep(status,nrow(log$walked.past.log)),p.id=rep(file,nrow(log$walked.past.log)),log$walked.past.log))
    combined.logs$products.hit.log<- rbind(combined.logs$products.hit.log, cbind(status=rep(status,nrow(log$products.hit.log)),p.id=rep(file,nrow(log$products.hit.log)),log$products.hit.log))
    combined.logs$product.all<- rbind(combined.logs$product.all,           cbind(status=rep(status,nrow(log$product.all)),p.id=rep(file,nrow(log$product.all)),log$product.all))
    return(combined.logs)
  }
}
participant.category<-function(file){
  healthy.code<- c("GEZ","DS","H1","HC","C1","01")
  ill.code<- c("C2","PP","H2","PSY","02","06","IC")
  
  if(any(startsWith(file,ill.code))){
    return("ill")
  }else if(any(startsWith(file, healthy.code))){
    return("healthy")
  }else{
    return("none")
  }
}

distance.between.aisles.enters<- function(input.data, aisles.log){
  aisles<-aisles.log %>% filter(label!= "main")
  
  pieces<-data.frame(start=aisles$stop[-nrow(aisles)], stop=aisles$start[-1])
  if(nrow(pieces)>0)
  for(i in 1:nrow(pieces)){
    pieces$dist[i]<-sum(input.data$dist[pieces$start[i]:pieces$stop[i]])
  }
  return(pieces$dist)
}

  
  
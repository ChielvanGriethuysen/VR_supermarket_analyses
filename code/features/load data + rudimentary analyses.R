# VR Supermarkt: create dataframe to load all statistics in and run some basic analyses.
#
# Last edited 2018-10-18 by Laurent Smeets (l.s.m.smeets@uu.nl)


createDataFrame <- function(data.files){
  names1<-c()
  for(P in 1:params$features$number.of.items){
    names1[P]<-paste0("n.box.P", P)
  }
  Y1<-as.data.frame(matrix(0 , ncol= length(names1), nrow=length(numeric(length(data.files)))))
  colnames(Y1)<-names1
  
  names2<-c()
  for(PP in 1:params$features$number.of.items){
    names2[PP]<-paste0("time.box.P", PP)
  }
  Y2<-as.data.frame(matrix(0 , ncol= length(names2), nrow=length(numeric(length(data.files)))))
  colnames(Y2)<-names2
  
  data <- data.frame(name = character(length(data.files)),          # name of input file
                     ID= character(length(data.files)),             # Identification variable
                     total.time = numeric(length(data.files)),      # total time spent on task
                     total.distance = numeric(length(data.files)),  # needs to be fixed.
                     n.aisle.1A = numeric(length(data.files)),      # number of times someone went into aisle 1A
                     n.aisle.2A = numeric(length(data.files)),      # number of times someone went into aisle 2A
                     n.aisle.3A = numeric(length(data.files)),      # number of times someone went into aisle 3A
                     n.aisle.4A = numeric(length(data.files)),      # number of times someone went into aisle 4A
                     n.aisle.5A = numeric(length(data.files)),      # number of times someone went into aisle 5A
                     n.aisle.1B = numeric(length(data.files)),      # number of times someone went into aisle 1B
                     n.aisle.2B = numeric(length(data.files)),      # number of times someone went into aisle 2B
                     n.aisle.3B = numeric(length(data.files)),      # number of times someone went into aisle 3B
                     n.aisle.4B = numeric(length(data.files)),      # number of times someone went into aisle 4B
                     n.aisle.5B = numeric(length(data.files)),      # number of times someone went into aisle 5B
                     n.aisle.6B = numeric(length(data.files)),      # number of times someone went into aisle 6B
                     n.aisle.M1 = numeric(length(data.files)),      # number of times someone went into aisle M1
                     n.aisle.M2 = numeric(length(data.files)),      # number of times someone went into aisle M2
                     n.aisle.M3 = numeric(length(data.files)),      # number of times someone went into aisle M3
                     time.aisle.1A = numeric(length(data.files)),   # total time someone spent into aisle 1A  
                     time.aisle.2A = numeric(length(data.files)),   # total time someone spent into aisle 2A  
                     time.aisle.3A = numeric(length(data.files)),   # total time someone spent into aisle 3A
                     time.aisle.4A = numeric(length(data.files)),   # total time someone spent into aisle 4A  
                     time.aisle.5A = numeric(length(data.files)),   # total time someone spent into aisle 5A  
                     time.aisle.1B = numeric(length(data.files)),   # total time someone spent into aisle 1B  
                     time.aisle.2B = numeric(length(data.files)),   # total time someone spent into aisle 2B  
                     time.aisle.3B = numeric(length(data.files)),   # total time someone spent into aisle 3B  
                     time.aisle.4B = numeric(length(data.files)),   # total time someone spent into aisle 4B  
                     time.aisle.5B = numeric(length(data.files)),   # total time someone spent into aisle 5B  
                     time.aisle.6B = numeric(length(data.files)),   # total time someone spent into aisle 6B  
                     time.aisle.M1 = numeric(length(data.files)),   # total time someone spent into aisle M1  
                     time.aisle.M2 = numeric(length(data.files)),   # total time someone spent into aisle M2  
                     time.aisle.M3 = numeric(length(data.files)),   # total time someone spent into aisle M3  
                     n.crossings = numeric(length(data.files)),     # total number of crossings someone made (crossing his/her own path)
                     n.crossings.outside.aisles = numeric(length(data.files)), # how many crossings did make outside of the horizontal shopping aisles
                     cross.1st.1.3rd= numeric(length(data.files)),     # number of crossings in the first 1/3rd of the time someone made
                     cross.2nd.1.3rd= numeric(length(data.files)),     # number of crossings in the second 1/3rd of the time someone made
                     cross.3rd.1.3rd= numeric(length(data.files)),     # number of crossings in the third 1/3rd of the time someone made
                     n.stops= numeric(length(data.files)),             # number of stops someone made
                     n.stops.item = numeric(length(data.files)),       # number of stops someone made in front of an item
                     n.stops.elsewhere= numeric(length(data.files)),   # number of stops someone made elsewhere (not in front of items) 
                     n.slows = numeric(length(data.files)),            # number of slows someone made
                     n.slows.item = numeric(length(data.files)),       # number of slows someone made in front of an item if slows starts in front of item
                     n.slows.elsewhere = numeric(length(data.files)),  # number of slows someone made elsewhere (not in front of items) 
                     n.slows.item2 = numeric(length(data.files)),      # number of slows someone made in front of an item if a slow is anywhere in the hitbox of the item
                     n.slows.elsewhere2 = numeric(length(data.files)), # number of slows someone made elsewhere (not in front of items, when a slow is anywhere in the hitbox of the item) 
                     total.stoping.time= numeric(length(data.files)),  # total stopping time
                     total.slowing.time = numeric(length(data.files)), # total time someone spent slowing
                     slows.1st.1.3rd = numeric(length(data.files)),    # number of slows in the first 1/3rd of the time someone made
                     slows.2nd.1.3rd = numeric(length(data.files)),    # number of slows in the second 1/3rd of the time someone made
                     slows.3rd.1.3rd = numeric(length(data.files)),    # number of slows in the third 1/3rd of the time someone made
                     slows.1st.1.3rd.items = numeric(length(data.files)), # number of slows in the first 1/3rd of the time someone made in front of an item
                     slows.2nd.1.3rd.items = numeric(length(data.files)), # number of slows in the second 1/3rd of the time someone made in front of an item
                     slows.3rd.1.3rd.items = numeric(length(data.files)), # number of slows in the third 1/3rd of the time someone made in front of an item
                     stops.1st.1.3rd = numeric(length(data.files)),    # number of stops in the first 1/3rd of the time someone made
                     stops.2nd.1.3rd = numeric(length(data.files)),    # number of stops in the second 1/3rd of the time someone made
                     stops.3rd.1.3rd = numeric(length(data.files)),    # number of stops in the third 1/3rd of the time someone made
                     n.datapoints = numeric(length(data.files)),                       # total number of datapoints in the file (data quality metric)
                     datapoints.per.second = numeric(length(data.files)),              # number datapoints per second in the file (data quality metric)
                     max.difference.between.points = numeric(length(data.files)),      # maximum difference in distance between two consequtive points in the file (data quality metric) 
                     qt95.difference.between.points = numeric(length(data.files)),     # .95 quantile difference in distance between consequtive points in the file (data quality metric) 
                     qt99.difference.between.points = numeric(length(data.files)),     # .99 quantile difference in distance between consequtive points in the file (data quality metric) 
                     max.difference.between.timepoints = numeric(length(data.files)),  # maximum difference in time between two consequtive points in the file (data quality metric) 
                     qt95.difference.between.timepoints = numeric(length(data.files)), # .95 quantile difference in time between consequtive points in the file (data quality metric)
                     qt99.difference.between.timepoints = numeric(length(data.files)), # .99 quantile difference in time between consequtive points in the file (data quality metric)
                     average.speed = numeric(length(data.files)),      # average speed of user
                     Y1,                                               # number of times someone walked into the hitbox of the different products
                     Y2,                                               # total time spent in the hitbox of the different products
                     Hit_Totaal = numeric(length(data.files)),         # total items someone picked up
                     n.walked.past.not.picked.up= numeric(length(data.files)),         # number of times walked past an item without picking it up
                     n.walked.past.not.picked.up.unique = numeric(length(data.files)), # number of times walked past an unique item without picking it up
                     
                     n.walked.through.aisles= numeric(length(data.files)), #nr of times walked through a aisles
                     n.walked.in.out.aisles= numeric(length(data.files)), #nr of times walked in an aisles and the same side out
                     
                     Avatars = numeric(length(data.files)),           # this item was merged from an other dataset and not created in this code
                     VR_aborted = numeric(length(data.files)),        # this item was merged from an other dataset and not created in this code
                     Tijd = numeric(length(data.files)),              # this item was merged from an other dataset and not created in this code
                     Interference = numeric(length(data.files)),      # this item was merged from an other dataset and not created in this code
                     Hit_1 = numeric(length(data.files)),             # this item was merged from an other dataset and not created in this code
                     Hit_2 = numeric(length(data.files)),             # this item was merged from an other dataset and not created in this code
                     Hit_3  = numeric(length(data.files)),            # this item was merged from an other dataset and not created in this code
                     Hit_4 = numeric(length(data.files)),             # this item was merged from an other dataset and not created in this code
                     Hit_5 = numeric(length(data.files)),             # this item was merged from an other dataset and not created in this code
                     Hit_6 = numeric(length(data.files)),             # this item was merged from an other dataset and not created in this code
                     Hit_7  = numeric(length(data.files)),            # this item was merged from an other dataset and not created in this code
                     Hit_8 = numeric(length(data.files)),             # this item was merged from an other dataset and not created in this code
                     Tijd_H1 = numeric(length(data.files)),           # this item was merged from an other dataset and not created in this code
                     Tijd_H2 = numeric(length(data.files)),           # this item was merged from an other dataset and not created in this code
                     Tijd_H3 = numeric(length(data.files)),           # this item was merged from an other dataset and not created in this code
                     Tijd_H4 = numeric(length(data.files)),           # this item was merged from an other dataset and not created in this code
                     Tijd_H5 = numeric(length(data.files)),           # this item was merged from an other dataset and not created in this code
                     Tijd_H6 = numeric(length(data.files)),           # this item was merged from an other dataset and not created in this code
                     Tijd_H7 = numeric(length(data.files)),           # this item was merged from an other dataset and not created in this code
                     Tijd_H8 = numeric(length(data.files)),           # this item was merged from an other dataset and not created in this code
                     FA_Totaal = numeric(length(data.files)),         # this item was merged from an other dataset and not created in this code
                     FA_Time = numeric(length(data.files)),           # this item was merged from an other dataset and not created in this code
                     Kassa = numeric(length(data.files)),             # this item was merged from an other dataset and not created in this code
                     stringsAsFactors = FALSE
  )
  return(data)
}

runFirstAnalyses <- function(JSONfile, 
                             image,
                             raw.images, 
                             save.data, 
                             data,
                             input.dir,
                             output.dir,
                             products,
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
    dat <- fromJSON(readLines(paste0('input/', input.dir, '/', JSONfile)),
                    simplifyDataFrame = TRUE)
  )
  
  # Remove duplicate data (speeds up all analyses)
  time <- dat[[1]]$m_PupilTime
  dup <- which(diff(time) == 0)
  FootPosition <- dat[[1]]$m_FootPosition
  if(length(dup) > 0){
    time <- time[-dup]
    FootPosition <- FootPosition[-dup, ]
    row.names(FootPosition) <- 1:nrow(FootPosition)
  }
  
  # Remove all datapoints before start of the task
  first <- first(which(FootPosition$z < 45.5 & FootPosition$z >10))
  if(first > 1){
    FootPosition <- FootPosition[-1 : -first, ]
    time <- time[-1 : -first]
  }
  
  # Remove all datapoints after end of the task
  last <- last(which(FootPosition$z < 45.5))
  if(last < nrow(FootPosition)){
    FootPosition <- FootPosition[-last : -nrow(FootPosition), ]
    time <- time[-last : -length(time)]
  }
  
  row.names(FootPosition) <- 1:nrow(FootPosition)
  
  x.change <- diff(FootPosition$x, 1)
  y.change <- diff(FootPosition$z, 1)
  distance.between.points<-sqrt(x.change^2 + y.change^2)
  total.distance<-sum(sqrt(x.change^2 + y.change^2))
  
  # Save results of basic analyses
  if(save.data){
    data$name[i] <- JSONfile
    data$ID[i]<-substr(JSONfile,1,6)
    data$total.time[i] <- last(time) - time[1]
    data$total.distance[i] <- total.distance
    data$n.datapoints[i] <- length(time)
    data$datapoints.per.second[i] <- length(time) / (last(time) - time[1])
    data$average.speed[i]<- data$total.distance[i] / data$total.time[i]
    data$Hit_Totaal[i]<-filter(Excel, ID== data$ID[i])$Hit_Totaal
    data$Avatars[i]<-filter(Excel, ID== data$ID[i])$Avatars
    data$VR_aborted[i]<-filter(Excel, ID== data$ID[i])$VR_aborted
    data$Tijd[i]<-filter(Excel, ID== data$ID[i])$Tijd
    data$Interference[i]<-filter(Excel, ID== data$ID[i])$Interference
    data$Hit_1[i]<-filter(Excel, ID== data$ID[i])$Hit_1
    data$Hit_2[i]<-filter(Excel, ID== data$ID[i])$Hit_2
    data$Hit_3[i]<-filter(Excel, ID== data$ID[i])$Hit_3
    data$Hit_4[i]<-filter(Excel, ID== data$ID[i])$Hit_4
    data$Hit_5[i]<-filter(Excel, ID== data$ID[i])$Hit_5
    data$Hit_6[i]<-filter(Excel, ID== data$ID[i])$Hit_6
    data$Hit_7[i]<-filter(Excel, ID== data$ID[i])$Hit_7
    data$Hit_8[i]<-filter(Excel, ID== data$ID[i])$Hit_8
    data$Tijd_H1[i]<-filter(Excel, ID== data$ID[i])$Tijd_H1
    data$Tijd_H2[i]<-filter(Excel, ID== data$ID[i])$Tijd_H2
    data$Tijd_H3[i]<-filter(Excel, ID== data$ID[i])$Tijd_H3
    data$Tijd_H4[i]<-filter(Excel, ID== data$ID[i])$Tijd_H4
    data$Tijd_H5[i]<-filter(Excel, ID== data$ID[i])$Tijd_H5
    data$Tijd_H6[i]<-filter(Excel, ID== data$ID[i])$Tijd_H6
    data$Tijd_H7[i]<-filter(Excel, ID== data$ID[i])$Tijd_H7
    data$Tijd_H8[i]<-filter(Excel, ID== data$ID[i])$Tijd_H8
    data$FA_Totaal[i]<-filter(Excel, ID== data$ID[i])$FA_Totaal
    data$FA_Time[i]<-filter(Excel, ID== data$ID[i])$FA_Time
    data$Kassa[i]<-filter(Excel, ID== data$ID[i])$Kassa
    data$max.difference.between.points[i] <-  max(abs(diff(distance.between.points)))
    data$qt95.difference.between.points[i] <-quantile(abs(diff(distance.between.points)), probs = 0.95)
    data$qt99.difference.between.points[i]  <-quantile(abs(diff(distance.between.points)), probs = 0.99)
    data$max.difference.between.timepoints[i] <- max(diff(time))
    data$qt95.difference.between.timepoints[i] <- quantile(diff(time),probs = 0.95)
    data$qt99.difference.between.timepoints[i]<- quantile(diff(time),probs = 0.99)

  }
  subpoints<- skippoints(FootPosition,time,0.2)
  F1<-subpoints[,-4]
  T1<-subpoints[,4]
  
  x.change <- diff(F1$x, 1)
  y.change <- diff(F1$z, 1)
  distance.between.subpoints<-sqrt(x.change^2 + y.change^2)
  speed<- distance.between.subpoints/diff(T1)
  speed<- c(mean(speed),speed)
  
  
  # Make and save plot of raw walking path
  ggpath <- ggplot() + 
    #ylim(-53, -7) + xlim(0, 29) +
    scale_x_continuous(limits = c(0, 30), expand=c(0,0)) +
    scale_y_continuous(limits = c(-50, -2.5), expand=c(0,0)) +
    annotation_custom(rasterGrob(image, 
                                 width = unit(1, "npc"), 
                                 height = unit(1, "npc")), 
                      -Inf, Inf, -Inf, Inf) +
    geom_path(data = FootPosition, 
              mapping = aes(x = x, y = -z, color = 1:length(x)),
              arrow = arrow(length = unit(5, "points"))) +
    #    coord_fixed() + 
    coord_flip() +
    theme(legend.position = "none",
          plot.margin = margin(0, 0, 0, 0, "cm")) +
    geom_text(aes(y = -12, x = 2, 
                  label = paste("Person", 
                                substr(JSONfile[i], 1, 5),
                                "\n Walking route")), 
              size = 5) + 
    geom_hline(yintercept = -45.5)
  
  ggspeed <- ggplot() + 
    #ylim(-53, -7) + xlim(0, 29) +
    scale_x_continuous(limits = c(0, 30), expand=c(0,0)) +
    scale_y_continuous(limits = c(-50, -2.5), expand=c(0,0)) +
    annotation_custom(rasterGrob(image, 
                                 width = unit(1, "npc"), 
                                 height = unit(1, "npc")), 
                      -Inf, Inf, -Inf, Inf) +
    geom_path(data = F1, 
              mapping = aes(x = x, y = -z, color = speed),
              arrow = arrow(length = unit(5, "points"))) +
    scale_colour_gradient(low = "red", high = "yellow") +
    #    coord_fixed() + 
    coord_flip() +
    theme(plot.margin = margin(0, 0, 0, 0, "cm")) +
    geom_text(aes(y = -12, x = 2, 
                  label = paste("Person", 
                                substr(JSONfile[i], 1, 5),
                                "\n Walking route")), 
              size = 5) + 
    geom_hline(yintercept = -45.5)
  
  # Save if required
  if(raw.images){
    # Create path if exists and create if it does not
    if( ! file.exists(paste0('output/png/', output.dir))){
      dir.create(paste0('output/png/', output.dir))
    }
    ggsave(paste0('output/png/', output.dir, '/',
                  JSONfile, '_RAW.png'), ggpath, 
           width = 37.5, height = 21, units = 'cm')
    ggsave(paste0('output/png/', output.dir, '/',
                  JSONfile, '_RAWspeed.png'), ggspeed, 
           width = 37.5, height = 21, units = 'cm')
    
  }
  
  
  # create product box
  productbox<- data.frame(xmin = rep(NA, nrow(products)),
                          xmax = rep(NA, nrow(products)),
                          zmin = rep(NA, nrow(products)),
                          zmax = rep(NA, nrow(products)),
                          up.down.side= products$up.down.side,
                          announced = products$announced,
                          productnumber =  products$productnumber,
                          x = products$x,
                          y = products$y,
                          colour = products$colour) %>%
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
  
  
  res <- list(dat = dat,
              gg = ggpath, 
              FootPosition = FootPosition,
              time = time, 
              data = data,
              x.change=x.change,
              y.change=y.change,
              distance.between.points=distance.between.points,
              productbox = productbox)
  
  return(res)
}
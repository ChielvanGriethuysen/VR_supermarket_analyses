# VR Supermarkt: function to extract crossings
#
# Last edited 2018-10-18 by Laurent Smeets (l.s.m.smeets@uu.nl)




getCrossings = function(data, FootPosition, time, gg, 
                        shopping.aisle.time.points, aisles,
                        cross.lag1, cross.lag2,
                        full.images, i){
  
  print(paste('Calculating crossings of file', i))
  crossings <- anyIntersects(FootPosition$x, -FootPosition$z, time, cross.lag1)
  #A crossings consists of 4 time points (a, a+1, b, b+1), both a and b ares saved.
  #Only a is used to calculate n crossings and plot them.
  #b can be used in the future to calculate the angle.
  crossings <- t(unname(as.data.frame(crossings)))
  colnames(crossings) <- c("First Time point", "Second Time point")
  
  
  cross.points1 <- crossings[, 1]


  
  if(length(cross.points1) > 1){ # If crossings exist
    cross.points1 <- cross.points1[-1]
    cross.time <- time[cross.points1]
    
    # Remove crossings which are cross.lag2 removed from a previous crossing
    l = 1 #Select first crossing
    if(length(cross.time) > 1){ #If there are at least 2 crossing
      d <- diff(cross.time)
      k = 0
      for(ii in 1:length(d)){
        if(ii > k){ # Don't look at crossings that have been skipped
          k = ii
          if(d[ii] < cross.lag2){
            dd = d[ii]
            while(dd < cross.lag2 & k < length(d)){ #Stop when there is more than lag between two crossings
              k = k + 1
              dd = dd + d[k]
            }
            if(dd < cross.lag2) # Stop if no crossings left
              break
          }
          
          l <- c(l, k + 1)
        }
      }
    }
    
    
    cross.points1 <- cross.points1[l]
    n.crossings <- length(cross.points1)
    crossings.pos <- FootPosition[cross.points1, ]
    
    data$n.crossings[i] <- n.crossings
  
    # if there is no crossings just assign 0
    } else {
    data$n.crossings[i] <- 0
    n.crossings<-0
    }
    
    # now do the same for crossing that take place in a shopping aisle.
    cross.points2 <- crossings[, 1]
    cross.points2 <- cross.points2[!cross.points2 %in% shopping.aisle.time.points] 
    
    if(length(cross.points2) > 1){ # If crossings exist
      cross.points2 <- cross.points2[-1]
      cross.time2 <- time[cross.points2]
      
      # Remove crossings which are cross.lag2 removed from a previous crossing
      l = 1 #Select first crossing
      if(length(cross.time2) > 1){ #If there are at least 2 crossing
        d <- diff(cross.time2)
        k = 0
        for(ii in 1:length(d)){
          if(ii > k){ # Don't look at crossings that have been skipped
            k = ii
            if(d[ii] < cross.lag2){
              dd = d[ii]
              while(dd < cross.lag2 & k < length(d)){ #Stop when there is more than lag between two crossings
                k = k + 1
                dd = dd + d[k]
              }
              if(dd < cross.lag2) # Stop if no crossings left
                break
            }
            
            l <- c(l, k + 1)
          }
        }
      }
      
      
      cross.points2 <- cross.points2[l]
      n.crossings.out.aisles <- length(cross.points2)
      crossings.pos.out.aisles <- FootPosition[cross.points2, ]
      data$n.crossings.outside.aisles[i] <- n.crossings.out.aisles
    
      # if there is no crossings just assign 0
      
    } else {
      data$n.crossings.outside.aisles[i] <- 0
      n.crossings.out.aisles<-0
      }
    
    
    #split slows per third.
    
    crosspointstibble<-tibble(crosspoints=cross.points1)
    split1<-which.min(abs(time - last(time)/3)) 
    split2<-which.min(abs(time - (last(time)/3*2)))
    
    cross.1st.1.3rd<-nrow(filter(crosspointstibble, crosspoints<split1))
    cross.2nd.1.3rd<-nrow(filter(crosspointstibble, crosspoints>split1 & crosspoints<split2))
    cross.3rd.1.3rd<-nrow(filter(crosspointstibble, crosspoints>split2))
  
    data$cross.1st.1.3rd[i]<-cross.1st.1.3rd
    data$cross.2nd.1.3rd[i]<-cross.2nd.1.3rd
    data$cross.3rd.1.3rd[i]<-cross.3rd.1.3rd

    # add the crossings to the plot
    if(full.images && n.crossings>0){
      gg.cross <- gg +
        geom_point(data = crossings.pos,
                   aes(x = x, y = -z, size = 2),
                   col = 'blue') +
        geom_text(aes(y = -48, x = 4, 
                      label = paste("N crossings =", n.crossings, "(", n.crossings.out.aisles, ")" )),
                  colour = 'blue')
    }else{
      gg.cross<-gg+ geom_text(aes(y = -48, x = 4, 
                                  label = paste("N crossings =", n.crossings, "(", n.crossings.out.aisles, ")" )),
                              colour = 'blue')
    }
    

     
  res.cross <- list(gg.cross = gg.cross, 
                    data = data, 
                    n.crossings = n.crossings,
                    cross.points.all=cross.points1,
                    cross.points.out.aisles=cross.points2)
  
  return(res.cross)
}

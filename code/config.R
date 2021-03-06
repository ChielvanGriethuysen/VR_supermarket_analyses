# VR Supermarkt: Set configuration parameters for feature extraction
#
# Last edited 2019-07-03 by Chiel van Griethuijsen (m.a.vangriethuijsen@students.uu.nl)


# set the parameters
params <- list(
  
  # name of image of supermarket
  #img.name ='Supermarkt Screenshot 10 Producten (volwassenen, tieners, kinderen).png',
  img.name = 'Supermarkt.png',
  
  # directory with input files
  input.dir = "test",
  
  # directory with output files
  output.dir = "8prod 0.1",
  
  # what output is saved (TRUE/FALSE)
  # It is adviced that full.images and save.data are both either TRUE or FALSE.
  # This ensures the features and the images shown always correspond.
  save.images   = TRUE,     # should the maps, and plots be saved     
  external.excel= FALSE,    # should the external excel be used(NPO and persenal data)
  save.log      = TRUE,     # should the raw log's be saved
  save.feature  = TRUE,     # should a feature file be made
  
  make.gif      = FALSE,    # Gif of a path (DOES NOT WORK RIGHT NOW)
  
  sheet.excel  = 2,        # On which sheet are the data point
  sheet.excel2 = 1,        # On which sheet are the personal data
  sheet.excel3 = 3,        # On which sheet are the NPO data
  n.row.excel  = 74,       # How many rows of data points are there
  
  range.personal = "A1:H",
  range.NPO      = "A1:V",
  
  features = list(

    # Stops measures the amount of stops. A stop is defined as less than 
    # stop.radius movement in at least stop.time from a point
    stops = list(
      stop.minimum.duration= 1, #minimum time a stop schould take
      stop.max.speed= 0.2, #speed used for discretisation
      stop.merge.time.1=0.1, #max time between two parts, used to reduce splits by outliers 
      stop.merge.time.2=1, #max time between two parts, used to ignore short button hits
      stop.merge.dist.1=0.1,
      stop.merge.dist.2=1
    ),
    walk= list(
      walk.minimum.dist= 1 # minimum time a walk schould take
    ),
    # Crossings
    cross = list(
      cross.lag1 = 3, #Time required for a path to cross itself before it counts as a crossing
      cross.lag2 = 3, #Minimum time required between two crossings
      cross.dist1= 4, #distance required for a path to cross itself before it counts as a crossing 
      cross.dist2= 4  #distance between two crossings
    ),
    pick= list(
      pick.time.before= 8
    ),
    view= list(
      min.points.1= 5,
      min.points.2= 25
    ),
    
    number.of.items = 8,  # total number of items in supermarket people can pick up.
    
    # Aisles, both names and coordinates are defined here
    aisles = data.frame(xmin = c(rep(c(4.9, 8.9, 12.9, 17, 21), 2), 25.2, c(3,3,5)),
                        xmax = c(rep(c(8.4, 12.4, 16.4, 20.5, 24.5), 2), 29.3, c(24.5 , 29.3, 29.3)),
                        zmin = c(rep(29.2, 5), rep(12.8, 6),  c(37.6, 25, 10)), 
                        zmax = c(rep(38.4, 5), rep(26, 6), c(41, 30.2, 13.8)),
                        names = c(paste0(rep(1:5, 2), rep(LETTERS[1:2], each = 5)), "6B", "1M", "2M", "3M"), # Names of aisles
                        type= c(rep("shopping", 11), rep("main", 3)))
    
    
    
    # possible value for up.down.side: "up", "down", "sideleft", "sideright"
    
    
    ),

  #These are the exact locations of the product
  #if no alternative reuse the complete name
  products =list( nemo_a= data.frame( x =             c( 16.3,  17.2,   13.1,   24.3,   12.3,  24.3,   8.3,   12.9     ),  
                                      z =             c(18.3,   34.4,   32.9,   17.2,   37.2,  15.6,   21.9,  37.9     ),
                                      height =        c(3.3,    3.3,    3.3,    3.3,    3.3,   3.3,    3.3,   4        ),
                                      width =         c(3,      3,      3,      3,      3,     3,      3,     3        ),
                                      up.down.side =  c("up",   "down", "down", "up",   "up",  "up",   "up",  "sideright"),
                                      announced =     c(FALSE,  FALSE,  FALSE,  FALSE,  FALSE, FALSE,  FALSE, TRUE     ),
                                      productnumber = c("P1",   "P2",  "P3",    "P4",   "P5",  "P6",  "P7",   "P8"     ),
                                      productname =   c("LU Oreo cookies", "Andrélon Wax flexible hold", "Dreft Afwasmiddel original", "Nutella Hazelnootpasta", "AH Sportsokken wit maat 43-46", "Calvé Pindakaas regular", "AH Molen zeezout", "Lay's Naturel"),
                                      alternativename=I(list(c("cookies"),
                                                           c("Wax"),
                                                           c("Dreft Afwasmiddel"),
                                                           c("hazelnootpasta","Nutella"),
                                                           c("sokken"),
                                                           c("Pindakaas", "Calvé"),
                                                           c("Ah Molen"),
                                                           c("Lay's Naturel")) ), stringsAsFactors = FALSE),
                   
                  nemo_b= data.frame(x =            c( 8.3 ,  8.3 ,   12.3,   16.3,   13.2,  17.2,   24.3,  16.8     ),  
                                    z =             c( 18.8,  16.1,   35.0,   35.1,   17.9,  33.0,   19.1,  38.2     ),
                                    height =        c( 3.3,   3.3,    3.3,    3.3,    3.3,   3.3,    3.3,   4        ),
                                    width =         c( 3,     3,      3,      3,      3,     3,      3,     3        ),
                                    up.down.side =  c("up",   "up",   "up" ,  "up",   "down","down", "up",  "sideright"),
                                    announced =     c(FALSE,  FALSE,  FALSE,  FALSE,  FALSE, FALSE,  FALSE, TRUE     ),
                                    productnumber = c("P1",   "P2",  "P3",    "P4",   "P5",  "P6",  "P7",   "P8"     ),
                                    productname =   as.character(c("Inproba Sambal oelek", "AH Mie nestjes", "AH Handzeep", "Omo Wit vloeibaar", "LU Tuc crackers cheese", "Fa Mystic moments deodorant", "Bolletje Meergranen beschuit", "Douwe Egberts Aroma rood grove maling")),
                                    alternativename=I(list(c("Sambal"),
                                                           c("Mie"),
                                                           c("zeep"),
                                                           c("Omo"),
                                                           c("Lu Tuc","Crackers"),
                                                           c("deodorant"),
                                                           c("beschuit"),
                                                           c("AH Perla", "Douwe Egberts","Arome","koffiepads")) ), stringsAsFactors = FALSE),
                  UMC3pro= data.frame(x =            c( 8.3 ,  16.3 ,   24.3,   17.2,   16.3,  12.3),  
                                     z =             c( 21.9,  32.5,   18.3,   34.4,   20.0,  37.3 ),
                                     height =        c(3.3,    3.3,    3.3,    3.3,    3.3,   3.3      ),
                                     width =         c(3,   3,    3,    3,    3,    3      ),
                                     up.down.side =  c("up", "up", "up" , "down",   "up",   "up"),
                                     announced =     c(FALSE, FALSE,  FALSE,  FALSE,  FALSE,  FALSE     ),
                                     productnumber = c("P1",   "P2",  "P3",    "P4",   "P5",  "P6" ),
                                     productname =   as.character(c("AH Molen zeezout", "Zwaluw Lucifers 5-pack", "De Ruijter Vruchtenhagel", "Andrélon Wax flexible hold", "LU Bastogne", "AH Sportsokken wit maat 43-46")),
                                     alternativename=I(list(c("AH Molen", "zout"),
                                                            c("Lucifers"),
                                                            c("Vruchtenhagel"),
                                                            c("wax","gel"),
                                                            c("Bastogne"),
                                                            c("sokken")) ), stringsAsFactors = FALSE)
                   
                   
                   
  )
)



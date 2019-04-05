# VR Supermarkt: Set configuration parameters for feature extraction
#
# Last edited 2018-10-18 by Laurent Smeets (l.s.m.smeets@uu.nl)


# set the parameters
params <- list(
  
  # name of image of supermarket
  #img.name ='Supermarkt Screenshot 10 Producten (volwassenen, tieners, kinderen).png',
  img.name = 'Supermarkt2.png',
  
  # directory with input files
  input.dir = "test",
  
  # directory with output files
  output.dir = "test2",
  
  # what output is saved (TRUE/FALSE)
  # It is adviced that full.images and save.data are both either TRUE or FALSE.
  # This ensures the features and the images shown always correspond.
  raw.images    = TRUE,     # Images with just the path
  full.images   = TRUE,     # Images with path + aisles, stops, crosses, etc.
  save.data     = TRUE,     # should stops, slows etc. be saved.
  save.to.excel = FALSE,    # should stops, slows etc. be saved.to an excel file
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
      stop.time = 2,      # seconds of stopping
      stop.radius = 0.25  # meters of walking
    ),
    
    # Slows measures the times someone has slowed. Is defined the same as a
    # stop, but with different numbers
    slows = list(
      slow.time = 4,   # seconds of stopping
      slow.radius = 1    # meters of walking
    ),
    
    # Crossings
    cross = list(
      cross.lag1 = 3, #Time required for a path to cross itself before it counts as a crossing
      cross.lag2 = 3  #Minimum time required between two crossings
    ),
    
    number.of.items = 8,  # total number of items in supermarket people can pick up.
    
    # Aisles, both names and coordinates are defined here
    aisles = data.frame(xmin = c(rep(c(5, 9.2, 13, 17, 21), 2), 25.2, rep(5,3)),
                        xmax = c(rep(c(8.5, 12.3, 16.3, 20.3, 24.5), 2), 29.3, c(24.5 , 29.3, 29.3)),
                        zmin = c(rep(-37.5, 5), rep(-24.9, 6), c(-42, -30.2, -13.8)),
                        zmax = c(rep(-30.3, 5), rep(-13.9, 6),  c(-37.6, -25, -10)), 
                        aisle.names = c(paste0(rep(1:5, 2), rep(LETTERS[1:2], each = 5)), "6B", "M1", "M2", "M3"), # Names of aisles
                        x = 1, y = 1, colour = 1, # Required for gg plot to work (don't ask why)
                        type= c(rep("shopping", 11), rep("main", 3))),
    
    
    #These are the exact locations of the product
    products = data.frame(x =             c( 16.3,  17.2,   13.1,   24.3,   12.3,  24.3,   8.3,   12.9     ),  
                          z =             c(-18.3, -34.4,  -32.9,  -17.2,  -37.2, -15.6,  -21.9, -37.9     ),
                          height =        c(3.3,    3.3,    3.3,    3.3,    3.3,   3.3,    3.3,  .3        ),
                          width =         c(1.5,   1.5,    1.5,    1.5,    1.5,    1.5,   1.5,    3.3        ),
                          up.down.side =  c("up", "down", "down" , "up",   "up",   "up",   "up", "sideleft"),
                          announced =     c(FALSE, FALSE,  FALSE,  FALSE,  FALSE,  FALSE,  FALSE, TRUE     ),
                          productnumber = c("P1",   "P2",  "P3",    "P4",   "P5",  "P6",  "P7",   "P8"     ),
                          x = 1, y = 1, colour = 1)
    # possible value for up.down.side: "up", "down", "sideleft", "sideright"
    
    
    )
)

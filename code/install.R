# VR Supermarkt: Install required packages
#
# Last edited 2018-06-25 by Sjoerd
install.packages(c("jsonlite", "tidyverse", "png", "ggforce",
                   "ggalt", "Rcpp", "grid", "gganimate","XML", 
                   "DescTools", "xlsx","ggpubr","LearnGeom","future.apply"))

library(devtools)
devtools::install_github("dgrtwo/gganimate")



packages <- c("jsonlite", "tidyverse", "png", "ggforce",
              "ggalt", "Rcpp", "grid", "gganimate")

lapply(packages, require, character.only = TRUE)


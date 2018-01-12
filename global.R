
#install and load required packages -----------------
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("shiny", "RODBC","dplyr","Distance","mrds", "ggplot2", "rgdal",
              "rgeos","dsm","knitr","maptools","gridExtra","sp", "DT", "kableExtra", "ggmap",
              "investr", "raster")
ipak(packages)

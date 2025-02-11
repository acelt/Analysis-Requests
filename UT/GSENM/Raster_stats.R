# RASTER STATS FOR GSENM

# Load packages
library(raster)
library(rgdal)
library(ggplot2)
library(sf)
library(terra)
library(dplyr)


# Set filepaths
dir <- 'C:\\Users\\alaurencetraynor\\Documents\\GSENM'
raster_name <- "annual_herbaceous.tif"
filepath <- paste0(dir,"\\", raster_name)

# Load raster
annual_herb <- rast(x = filepath)

# Create classes
classes <- c(0,1,1,
               1,5,2,
               5,10,3,
               10,25,4,
               25,100,5,
             )

class_mat <- matrix(classes, ncol=3, byrow=TRUE)

# Reclassify
reclass_annual_herb <- classify(annual_herb, class_mat, include.lowest=TRUE)

# summarise by class
size <- cellSize(reclass_annual_herb, unit = "ha")

zonal_stats <- zonal(x = size, z = reclass_annual_herb, fun = "sum")

#convert to acres
zonal_stats$acres <- zonal_stats$area * 2.471

zonal_stats

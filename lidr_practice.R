library(devtools)
library(sf)
library(ggplot2)
library(raster)
library(viridis)

las.dir <- "C:/Users/jaria/Downloads/2015_ME_MA_lidar_Job847219/"
file.names <- list.files(path = las.dir, pattern = "Job847219_42072_*", full.names = TRUE)

las1 <- lidR::readLAS(file.names[1])

las1.norm <- lidR::normalize_height(las1, algorithm = lidR::tin(), na.rm = TRUE, res = 1)
las1.norm <- lidR::filter_poi(las1.norm, Z > 0, Z < 100) #try Z < 200

lidR::plot(las1.norm, pal = viridis::magma(24), bg = "white", axis = FALSE, legend = TRUE)

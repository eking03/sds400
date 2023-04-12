library(devtools)
library(sf)
library(ggplot2)
library(raster)
library(viridis)

las.dir <- "C:/Users/jaria/Downloads/2015_ME_MA_lidar_Job847219/"
file.names <- list.files(path = las.dir, pattern = "Job847219_42072_*", full.names = TRUE)


#las67 <- lidR::readLAScatalog(file.names[c(6, 7)], progress = FALSE, filter = "-keep_first -drop_z_below 0 -drop_z_above 500")
#las <- lidR::clip_rectangle(las67, 345214.82, 2940022.52, 350525.83, 2945560.64)

las7 <- lidR::readLAScatalog(file.names[7], progress = FALSE, filter = "-keep_first -drop_z_below 0 -drop_z_above 500")
lasQuad <- lidR::clip_rectangle(las7, 346242.477, 2943359.757, 347338.812, 2944438.415)
lasQuad <- lidR::filter_poi(lasQuad, Z < 350)

lidR::plot(lasQuad, pal = viridis::magma, bg = "white", axis = FALSE, legend = TRUE)






library(devtools)
library(sf)
library(ggplot2)
library(raster)
library(viridis)
library(suncalc)

las.dir <- "C:/Users/jaria/Downloads/2015_ME_MA_lidar_Job847219/"
file.names <- list.files(path = las.dir, pattern = "Job847219_42072_*", full.names = TRUE)


#las67 <- lidR::readLAScatalog(file.names[c(6, 7)], progress = FALSE, filter = "-keep_first -drop_z_below 0 -drop_z_above 500")
#las <- lidR::clip_rectangle(las67, 345214.82, 2940022.52, 350525.83, 2945560.64)

las7 <- lidR::readLAScatalog(file.names[7], progress = FALSE, filter = "-keep_first -drop_z_below 0 -drop_z_above 350")
lasQuad <- lidR::clip_rectangle(las7, 346242.477, 2943359.757, 347338.812, 2944438.415)

wilson_roof <- lidR::clip_circle(lasQuad, roof$x[1], roof$y[1], 1)
hist(wilson_roof@data$Z)

gardiner_roof <- lidR::clip_circle(lasQuad, roof$x[3], roof$y[3], 1)
hist(gardiner_roof@data$Z)

#lidR::plot(lasQuad, pal = viridis::magma, bg = "white", axis = FALSE, legend = TRUE)

gnd <- lidR::filter_ground(lasQuad)
plot(gnd, size = 3, bg = "white", color = "Classification")




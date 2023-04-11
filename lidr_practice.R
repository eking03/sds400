library(devtools)
library(sf)
library(ggplot2)
library(raster)
library(viridis)

las.dir <- "C:/Users/jaria/Downloads/2015_ME_MA_lidar_Job847219/"
file.names <- list.files(path = las.dir, pattern = "Job847219_42072_*", full.names = TRUE)


las67 <- lidR::readLAScatalog(file.names[c(6, 7)], progress = FALSE, filter = "-keep_first -drop_z_below 0 -drop_z_above 500")
las <- lidR::clip_rectangle(las67, 345214.82, 2940022.52, 350525.83, 2947382.60)

lidR::plot(las, pal = viridis::magma(24), bg = "white", axis = FALSE, legend = TRUE)

#lidR::plot(las, color = "Classification", bg = "white", axis = FALSE, legend = TRUE)
#las.norm <- lidR::normalize_height(las, algorithm = lidR::tin(), na.rm = TRUE, res = 1)





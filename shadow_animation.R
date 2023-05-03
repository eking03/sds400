library(ggplot2)
library(whitebox)
library(lidR)
library(rayshader)
library(rayrender)
library(raster)
library(suncalc)
library(sf)
library(lubridate)
library(rgdal)
library(reshape2)   #is there a way to check for unused libraries?
library(av)
library(Cairo)
library(magick)

setwd(here::here())



# Importing LAS --------------------------------

las.dir <- "C:/Users/jaria/Downloads/2015_ME_MA_lidar_Job847219/"
file.names <- list.files(path = las.dir, pattern = "Job847219_42072_*", full.names = TRUE)

las7 <- lidR::readLAScatalog(file.names[7], progress = FALSE, filter = "-keep_first -drop_z_below 0 -drop_z_above 350")
lasQuad <- lidR::clip_rectangle(las7, 346242.477, 2943359.757, 347338.812, 2944438.415)
#file.copy(from = file.names[7], to = ".")

quad_dtm <- lidR::rasterize_canopy(lasQuad, res = 1, algorithm = dsmtin(), pkg = "raster")
quad_mat <- raster_to_matrix(quad_dtm)



# Sun and Shadow Calculations ------------------

suntimes <- getSunlightTimes(as.Date("2022-12-21"), lat = 42.3205, lon = -72.6440,tz = "EST")
# Note: the latest sunrise occurs on Jan 4th
#       the earliest sunset on Dec 8th

#Start an hour after sunrise and end an hour before sunset
dawn <- suntimes$nauticalDawn
dusk <- suntimes$nauticalDusk

now = dawn
quad_shadows = list()
sunangles = list()
i = 1

Sys.time()   # Start Time: "2023-05-02 22:26:48 EDT"
while(now < dusk) {
  sunangles[[i]] = suncalc::getSunlightPosition(date = now, lat = 42.3205, lon = -72.6440)[4:5]*180/pi
  print(now)
  quad_shadows[[i]] = ray_shade(quad_mat,
                                sunangle = sunangles[[i]]$azimuth+180, 
                                sunaltitude = sunangles[[i]]$altitude,
                                lambert = FALSE, zscale = 2, multicore = TRUE)
  now = now + duration("180s") 
  i = i + 1
}
Sys.time()   # End Time: "2023-05-02 23:23:39 EDT"



# Animating Results  ---------------------------

Cairo(file = "./shadowplot%03d.png", height = 662, width = 672)

for (n in 1:i) {
  plot_map(quad_shadows[[n]])
}

dev.off()

png_names <- sprintf("./shadowplot%03d.png", 1:(i-1))
png_frames <- image_read(png_names)
image_write_gif(png_frames, "./quad_dec21shadows.gif", delay = 1/10)
### "C:\\Users\\jaria\\Documents\\R\\Capstone-410\\sds400\\quad_dec21shadows.gif"


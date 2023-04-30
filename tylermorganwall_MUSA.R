# https://github.com/tylermorganwall/MusaMasterclass

# install.packages(c("ggplot2","raster", "rayrender", "spatstat", "spatstat.utils","suncalc","here", "sp","lubridate","rgdal", "magick", "av","xml2", "dplyr"))

# install.packages("remotes")

# remotes::install_github("giswqs/whiteboxR")
# whitebox::wbt_init()

# remotes::install_github("tylermorganwall/rayshader")

options(rgl.useNULL = FALSE)
library(ggplot2)
library(whitebox)
library(rayshader)
library(rayrender)
library(raster)
library(spatstat)
library(spatstat.utils)
library(suncalc)
library(sp)
library(lubridate)
library(rgdal)
library(reshape2)
library(av)

setwd(here::here())

#
# -------------------------------
#

#291.3 MB
# download.file("https://dl.dropboxusercontent.com/s/2jge03ptvsley62/26849E233974N.zip", destfile = "26849E233974N.zip")

## Alternate Link
# download.file("https://www.tylermw.com/data/26849E233974N.zip", destfile = "26849E233974N.zip")

#unzip("26849E233974N.zip")

whitebox::wbt_lidar_tin_gridding(here::here("26849E233974N.las"),
                                 output = here::here("phillydem.tif"), minz = 0,
                                 resolution = 1, exclude_cls = '3,4,5,7,13,14,15,16,18')

# backups in case whitebox doesn't work for you
#download.file("https://dl.dropboxusercontent.com/s/3auywgq93lokurf/phillydem.tif", destfile = "phillydem.tif")

## Alternate Link
# download.file("https://www.tylermw.com/data/phillydem.tif", destfile = "phillydem.tif")

phillyraster = raster::raster("phillydem.tif")

building_mat = raster_to_matrix(phillyraster)

## Alternate Link
# download.file("https://www.tylermw.com/data/building_mat_small.Rds", destfile = "building_mat_small.Rds")
# building_mat_small = readRDS("building_mat_small.Rds")

building_mat_small = reduce_matrix_size(building_mat, 0.5)
dim(building_mat_small)

#
# -------------------------------
#

getSunlightTimes(as.Date("2019-06-21"), lat = 39.9526, lon = -75.1652,tz = "EST")

#
# -------------------------------
#

#Start an hour after sunrise and end an hour before sunset
philly_time_start = ymd_hms("2019-06-21 05:30:00", tz = "EST")
philly_time_end= ymd_hms("2019-06-21 18:30:00", tz = "EST")

temptime = philly_time_start            #initiating a dyanmic object; starts the loop as (temptime = sunrise_time), ends the loop as (temptime = sunset_time)
philly_existing_shadows = list()        #setting an empty list to store sunlight times
sunanglelist = list()                   #setting an empty list to story sun angle calculations
counter = 1                             #'counter' is used like 'i' in for-loops

 while(temptime < philly_time_end) {    #while temptime is not yet equal to sunset
   sunanglelist[[counter]] = suncalc::getSunlightPosition(date = temptime, lat = 39.9526, lon = -75.1652)[4:5]*180/pi   #sunanglelist[[counter]] - sunanglelist[[i]] is used to assigns a value in the i-th row in the list empty list 'sunanglelist'
   print(temptime)   #print temptime to indicate progress of while loop; [above note continued] given time and place, getSunlightPosition() stores altitude and azimuth in the first and second column of the sunanglelist matrix list
   philly_existing_shadows[[counter]] = ray_shade(building_mat_small,            #using the original shrunken building-matrix to make a rayshaded matrix at temptime-o'clock
                                   sunangle = sunanglelist[[counter]]$azimuth+180,  
                                   sunaltitude = sunanglelist[[counter]]$altitude,
                                   lambert = FALSE, zscale = 2,  #'zscale' definition according to rayshader documentation:  "Default '1'. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation is in units of meters and the grid values are separated by 10 meters, zscale would be 10."
                                   multicore = TRUE)
   temptime = temptime + duration("3600s")     #duration() is useful for time math; 3600s is to calculate shadows in 1hr intervals
   counter = counter + 1
 }

#Downloading the pre-computed data to save time

##download.file("https://dl.dropboxusercontent.com/s/ipgcg51ct8esg3w/philly_existing_shadows.Rds", 
##              destfile = "philly_existing_shadows.Rds")
# download.file("https://www.tylermw.com/data/philly_existing_shadows.Rds", 
#               destfile = "philly_existing_shadows.Rds")

##philly_existing_shadows = readRDS("philly_existing_shadows.Rds")
str(philly_existing_shadows)

plot_map(philly_existing_shadows[[2]])

# Pithy "data science" way
shadow_coverage = Reduce(`+`, philly_existing_shadows)/length(philly_existing_shadows)   #"Reduce() reduces a vector, x , to a single value by recursively calling a function, f , two arguments at a time. It combines the first two elements with f , then combines the result of that call with the third element, and so on."

## Verbose "programmer" way
#shadow_coverage = matrix(0, nrow(philly_existing_shadows), ncol(philly_existing_shadows))
#for(i in seq_len(length(philly_existing_shadows)) {
#  shadow_coverage = shadow_coverage + philly_existing_shadows[[i]]
#}
#shadow_coverage = shadow_coverage/length(philly_existing_shadows)

shadow_coverage %>%
  reshape2::melt(varnames = c("x","y"), value.name = "light") %>%
  ggplot() + 
  geom_raster(aes(x = x,y = y,fill = light)) +
  scale_fill_viridis_c("Daily Light\nCoverage") + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) +
  coord_fixed()


#download.file("https://dl.dropboxusercontent.com/s/gqnfs71t0u3pmu7/sunangles.Rds",
#               destfile = "sunangles.Rds")
## Alternate Link
# download.file("https://www.tylermw.com/data/sunangles.Rds",
#               destfile = "sunangles.Rds")
# 
# sunangles = readRDS("sunangles.Rds")
# NOTE: Artifact from original MusaMasterclass README - mixed use of variable names 'sunangles' and 'sunanglelist'

str(sunanglelist)
 

# 3D rotating animation as the shadows move through the day
transition_frames = round(seq(1,360,length.out = 14))[-14]      #[-14] deletes the 14th element; 'transition_frames' is list of 13, corresponding to length of 'sunanglelist'
transition_frames
counter = 1
for(i in 1:360) {
 if(i %in% transition_frames) {
   rgl::rgl.clear()
   building_mat_small %>%                                           #'building_mat_small' replaces 'philly_with_building_mat_small'; the ladder height map matrix (reduced size version) is used in the original README
   sphere_shade(colorintensity = 5, sunangle = sunanglelist[[counter]]$azimuth+180) %>%
   add_shadow(philly_existing_shadows[[counter]],0.3) %>%           #'philly_new_shadows' in used in the original README; replace with shadow matrix ('philly_existing_shadows')
   add_water(detect_water(building_mat_small, zscale = 4,           #'building_mat_small' replaces 'philly_with_building_mat_small'
                          max_height= 10, cutoff = .93, min_area = 1000)) %>%
   plot_3d(building_mat_small, zscale = 2,                          #'building_mat_small' replaces 'philly_with_building_mat_small'
           zoom = 0.5,phi = 30,fov = 70,
           windowsize = 1000, background = "#d9e7ff", shadowcolor = "#313947")
   counter = counter + 1
 }
 render_camera(theta = i)
 render_snapshot(glue::glue("rotating{i}"))
}    #DO NOT CLOSE RGL WINDOW PREMATURELY! IT'S RENDERING THE PNGs! 360 snapshots, and that pause where the map takes a sec to reload is the if-statement running to insert 1 of the 13 transition frames

av::av_encode_video(glue::glue("rotating{1:360}.png"), output = "westphilly_movie.mp4",           #package 'glue'?
                   framerate = 30)      #original README used 'glue::glue("rotating{1:60}.png")'; "glues" together 60 of the 360 frames
### [1] "C:\\Users\\jaria\\Documents\\R\\Capstone-410\\sds400\\westphilly_movie.mp4"

# Suggestions from README:
#   | you could improve the analysis by stepping down to a much smaller 
#   | timescale (e.g. temptime = temptime + duration("180s") for three 
#   | minute intervals). You could also remove the reduce_matrix_size() 
#   | step for increased resolution.
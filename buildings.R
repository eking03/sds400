#download.file("https://northamptonma.gov/DocumentCenter/View/15913/buildings_2017.zip","buildings_2017.zip")
#unzip("buildings_2017.zip", exdir = ".")
library(tidyverse)
library(sf)

campus_footprints <- st_read("C:/Users/jaria/Documents/R/Capstone-410/sds400/Campus_Building_Footprints_2020/campus_blgs_2020.shp") %>%
  st_transform(8748)

buildings_2017 <- st_read("C:/Users/jaria/Documents/R/Capstone-410/sds400/buildings_2017/buildings_2017.shp") %>%
  st_transform(8748) %>%
  st_crop(c(xmin = 345214.82, ymin = 2940022.52, xmax = 350525.83, ymax = 2947382.60))

overlap <- st_overlaps(buildings_2017, campus_footprints)
buildings_2017$smith <- map_lgl(overlap, function(x) {
  if (length(x) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})

ggplot() +
  geom_sf(data = buildings_2017, aes(fill = smith)) + 
  #geom_sf(data = campus_footprints) +
  coord_sf()

#ggplot(buildings_2017) + geom_sf() + coord_sf()
ggplot(campus_footprints) + geom_sf() + coord_sf()

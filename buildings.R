#download.file("https://northamptonma.gov/DocumentCenter/View/15913/buildings_2017.zip","buildings_2017.zip")
#unzip("buildings_2017.zip", exdir = ".")
library(tidyverse)
library(sf)

campus_footprints <- st_read("C:/Users/jaria/Documents/R/Capstone-410/sds400/Campus_Building_Footprints_2020/campus_blgs_2020.shp") %>%
  st_transform(8748)

buildings_2017 <- st_read("C:/Users/jaria/Documents/R/Capstone-410/sds400/buildings_2017/buildings_2017.shp") %>%
  st_transform(8748) %>%
  st_crop(c(xmin = 345214.82, ymin = 2940022.52, xmax = 350525.83, ymax = 2945560.64))

overlap <- st_overlaps(buildings_2017, campus_footprints)
buildings_2017$smith <- map_lgl(overlap, function(x) {
  if (length(x) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})


roof <- campus_footprints %>%
  filter(OBJECTID == 116 | OBJECTID == 131 | OBJECTID == 141 | OBJECTID == 148 ) %>%
  st_point_on_surface()

ggplot() +
  geom_sf(data = campus_footprints, fill = "white", alpha = 0.8) +
  geom_sf(data = roof) +
  coord_sf(
    xlim = c(346242.477, 347338.812), 
    ylim = c(2943359.757, 2944438.415),
    expand = FALSE,
    crs = st_crs(8748))


library(devtools)
library(tidyverse)
library(sf)
library(ggplot2)
library(raster)
library(viridis)
library(suncalc)
library(lidR)
library(rayshader)
# library(remotes)
# remotes::install_github("PhanstielLab/BentoBox")
# BiocManager::install("plotgardener")
# library("plotgardener")
# library(whitebox)
library(lubridate)
library(pdftools)
# library(stars)
# library(magick)
# library(tesseract)
# library(OpenImageR)
# library(grDevices)
# library(imager)
library(terra)

las.dir <- "C:/Users/jaria/Downloads/2015_ME_MA_lidar_Job847219/"
file.names <- list.files(path = las.dir, pattern = "Job847219_42072_*", full.names = TRUE)

campus_footprints <- st_read("C:/Users/jaria/Documents/R/Capstone-410/sds400/Campus_Building_Footprints_2020/campus_blgs_2020.shp") %>%
  st_transform(8748)

# naip <- raster::raster("C:/Users/jaria/Downloads/2016_4BandImagery_Job943597/2016_4BandImagery_J943597.tif")
# naip_rgb <- raster::stack("C:/Users/jaria/Downloads/2016_4BandImagery_Job943597/2016_4BandImagery_J943597.tif")

#las67 <- lidR::readLAScatalog(file.names[c(6, 7)], progress = FALSE, filter = "-keep_first -drop_z_below 0 -drop_z_above 500")
#las <- lidR::clip_rectangle(las67, 345214.82, 2940022.52, 350525.83, 2945560.64)

#las7 <- lidR::readLAScatalog(file.names[7], progress = FALSE, filter = "-keep_first -drop_z_below 0 -drop_z_above 350")
#lasQuad <- lidR::clip_rectangle(las7, 346242.477, 2943359.757, 347338.812, 2944438.415)

las7 <- lidR::readLAScatalog(file.names[7], progress = FALSE, filter = "-drop_z_below 0 -drop_z_above 350")
lasQuad <- lidR::clip_rectangle(las7, 346242.477, 2943359.757, 347338.812, 2944438.415)
# lasF <- lidR::filter_firstofmany(lasQuad)

roofQuad <- campus_footprints %>%
  filter(OBJECTID == 116 | OBJECTID == 131 | OBJECTID == 141 | OBJECTID == 148 )
roofQuad <- st_transform(roofQuad, st_crs(lasQuad))

# roofWilder <- roofQuad %>%
#   filter(OBJECTID == 116) %>%
#   dplyr::select(geometry) %>%
#   st_zm(drop = TRUE)
# 
# lasWilder <- lidR::clip_roi(lasF, roofWilder)
# fullWilder <- lidR::clip_roi(lasQuad, roofWilder)
# outWilder <- lidR::clip_roi(lasQuad, sf::st_minimum_rotated_rectangle(roofWilder))

# plot(lasWilder, pal = viridis::magma, bg = "white")

roofQuad <- st_zm(roofQuad, drop = TRUE)
quadLAS <- lidR::merge_spatial(lasQuad, roofQuad, attribute = "RefName")
# quadLAS@data %>% group_by(RefName) %>% summarise(n = n())
quadLAS@data$z_perc <- quadLAS@data %>% group_by(RefName) %>% mutate(z_perc = percent_rank(Z)) %>% mutate(z_perc = ifelse((z_perc >= 0.95) | (z_perc <= 0.02), 1, 2)) %>% ungroup() %>% dplyr::select(z_perc) 
quadLAS@data$z_perc <- as.integer(quadLAS@data$z_perc)  ##maybe try 98th percentile
# quadLAS@data %>% group_by(RefName, z_perc) %>% summarise(n=n())

# quadLAS <- lidR::clip_roi(lasQuad, st_union(st_zm(roofQuad, drop = TRUE)$geometry))
quadLAS <- filter_poi(quadLAS, Classification != 18)  ## this should happen before z_perc, bc if class==18, z definitely is aboze 95th percentile, bc the girl is an outlier
quadLAS <- filter_poi(quadLAS, !is.na(RefName))  ## allows me to remove points not in polygon without having to clip_roi (since after merge_spatial, NA points are outside roi)
# quadLAS <- filter_poi(quadLAS, ReturnNumber == 1)
# lidR::filter_poi(quadLAS, Classification != 2) %>% plot(pal = viridis::magma, bg = "white", size = 4)
plot(quadLAS, color="z_perc", bg = "white", size=2)


quad_rgb <- raster::crop(naip_rgb, lasQuad)
names(quad_rgb) <- c("r", "g", "b", "ir")

quad_r <- rayshader::raster_to_matrix(quad_rgb$r)
quad_g <- rayshader::raster_to_matrix(quad_rgb$g)
quad_b <- rayshader::raster_to_matrix(quad_rgb$b)

quad_array <- array(0, dim = c(nrow(quad_r), ncol(quad_r), 3))

quad_array[,,1] <- quad_r/255 
quad_array[,,2] <- quad_g/255 
quad_array[,,3] <- quad_b/255 

quad_array <- aperm(quad_array, c(2, 1, 3))
quad_color <- scales::rescale(quad_array, to = c(0,1))
#plot_map(quad_color)

# quad_krig <- lidR::rasterize_terrain(lasQuad, algorithm = kriging(k = 40))
# took ~15-25 min to complete kriging

quad_dtm <- lidR::rasterize_canopy(lasQuad, res = 1, algorithm = dsmtin(), pkg = "raster")
wilder_dtm <- crop(quad_dtm, roofWilder) #%>% mask(roofWilder)
# mask(wilder_dtm, roofWilder) %>% plot()


# x <- plot(fullWilder, bg = "white", size = 3)
# add_dtm3d(x, wilder_dtm)

quad_matrix <- rayshader::raster_to_matrix(quad_dtm)
# quad_matrix2 <- t(quad_matrix)

# plot_3d(quad_color, quad_matrix)
# render_points(extent(lasWilder), 
#               lat = unlist(fullWilder@data$Y), 
#               long = unlist(fullWilder@data$X), 
#               altitude = unlist(fullWilder@data$Z),
#               size = 8, color = plotgardener::mapColors(vector = fullWilder@data$NumberOfReturns, palette = viridis::magma))

# render_points(extent(lasQuad), lat = 2943561, long = 346555, altitude = 218, size = 20, color = "red")

# quad_matrix %>%
#   sphere_shade() #%>% plot_map()
#   add_shadow(ray_shade(quad_matrix, zscale = 1), 0.5) %>% plot_map()

#   add_shadow(ambient_shade(quad_matrix), 0) %>%
#   plot_3d(quad_matrix, zscale = 1, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))
# Sys.sleep(0.2)
# render_snapshot()

# h <- quad_matrix %>% ray_shade() %>% plot_map()
# quad_matrix %>% sphere_shade() %>% plot_map()
# quad_matrix %>% sphere_shade() %>% add_shadow(ray_shade(quad_matrix), rescale_original = FALSE) %>% plot_map()
# r <- ray_shade(quad_matrix)
# h <- sphere_shade(quad_matrix)
# 
# 
# lamb_shade(quad_matrix) %>% plot_map()
# ray_shade(quad_matrix) %>% plot_map()
# sphere_shade(quad_matrix) %>% plot_map()
# 
# plot_3d(h, quad_matrix)
# 
# flight <- ymd_hms("2016-08-04 14:09:00", tz = "America/New_York")
flight <- ymd_hms("2016-08-04 14:09:00", tz = "UTC")
ma44 <- c(42.3251, -72.6412)  #c(346790.7, 2943899)
light_source <- suncalc::getSunlightPosition(date = flight, lat = ma44[1], lon = ma44[2])[4:5]*180/pi
naip_shadows <- ray_shade(quad_matrix, 
                          sunangle = light_source$azimuth+180, 
                          sunaltitude = light_source$altitude, 
                          lambert = FALSE, zscale = 1, multicore = TRUE)
 naip_shadows %>% 
   #add_shadow(ray_shade(quad_matrix), rescale_original = FALSE) %>% 
   plot_map()

 plot_map(quad_color)

# ggplot() +
#   geom_sf(data = roofWilder, color = "darkorange") +
#   coord_sf()


lasP <- segment_shapes(lasQuad, shp_plane(k = 20), "plane")
lasHP <- segment_shapes(lasQuad, shp_hplane(k = 20), "hplane")
lasL <- segment_shapes(lasQuad, shp_line(k = 20), "line")
lasHL <- segment_shapes(lasQuad, shp_hline(k = 20), "hline")
lasVL <- segment_shapes(lasQuad, shp_vline(k = 20), "vline")
plot(lasQuad)
plot(lasP, color = "plane")
plot(lasHP, color = "hplane")
plot(lasL, color = "line")
plot(lasHL, color = "hline")
plot(lasVL, color = "vline")



# VOXEL
m <- voxel_metrics(lasQuad, length(Z), 8)
m <- voxel_metrics(lasQuad, mean(Intensity), 14)
m <- voxel_metrics(lasQuad, length(Z), 40)
plot(m, color = "V1", pal = heat.colors(50), breaks = 'quantile')
plot(m, color = "V1", pal = heat.colors(20), breaks = 'quantile', voxel = TRUE)

f = function(x) {list(mean = sqrt(mean(x^2)))}
m <- voxel_metrics(lasQuad, f(Intensity), 30)
plot(m, color = "mean", pal = heat.colors(50), breaks = 'quantile', voxel = TRUE)


plot(lasQuad, pal = viridis::magma, bg = "white")
plot(fullWilder, pal = viridis::magma, bg = "white")

# lasF <- lidR::filter_firstofmany(lasQuad)
plot(lasQuad, color = "NumberOfReturns", pal = viridis::magma, bg = "white", size = 4)

lidR::filter_poi(lasQuad, NumberOfReturns > 2) %>%
  plot(color = "NumberOfReturns", pal = viridis::magma, bg = "white", size = 4)
lidR::filter_poi(lasF, NumberOfReturns > 2) %>%
  plot(color = "NumberOfReturns", pal = viridis::magma, bg = "white", size = 4)

lidR::filter_poi(lasF, NumberOfReturns > 2 & Intensity < 10000) %>%
  plot(color = "Intensity", pal = viridis::magma, bg = "white", size = 4)

lidR::filter_poi(lasWilder, NumberOfReturns > 2 & Intensity < 10000) %>%
  plot(color = "Intensity", pal = viridis::magma, bg = "white", size = 4)

lidR::filter_poi(lasWilder, NumberOfReturns > 2) %>%
  plot(color = "Intensity", pal = viridis::magma, bg = "white", size = 4)

fullWilder %>% 
  filter_poi(Intensity < 10000) %>%
  #plot(color = "Intensity", pal = viridis::magma, bg = "white", size = 4)
  plot(color = "Intensity", pal = viridis::magma, bg = "white", size = 4)

plot(lasQuad, color = "NumberOfReturns", pal = viridis::magma, bg = "white", size = 4)
#plot(lasQuad, color = "NumberOfReturns", pal = viridis::magma, alpha = "NumberOfReturns", bg = "white", size = 4)
plot(lasF, color = "Intensity", pal = viridis::magma, bg = "white")

lidR::filter_poi(lasQuad, Intensity > 10000) %>%
  plot(color = "Intensity", pal = viridis::magma, bg = "white")

ttopsQuad <- lidR::locate_trees(lasQuad, lidR::lmf(ws = 5))
#lidR::plot(ttopsQuad, color =  )
plot(lasQuad, color = "NumberOfReturns", pal = viridis::magma, bg = "white", size = 4) %>% 
  lidR::add_treetops3d(ttopsQuad, color = "blue", size = 26) 
plot(sf::st_geometry(ttopsQuad))


wilson_roof <- lidR::clip_circle(lasQuad, roof$x[1], roof$y[1], 1)
hist(wilson_roof@data$Z)

gardiner_roof <- lidR::clip_circle(lasQuad, roof$x[3], roof$y[3], 1)
hist(gardiner_roof@data$Z)

#lidR::plot(lasQuad, pal = viridis::magma, bg = "white", axis = FALSE, legend = TRUE)
#lidR::plot(lasCenter, pal = viridis::magma, bg = "white", axis = FALSE, legend = TRUE)

lasQuad2 <- lasQuad
lasQuad2$ReturnNumber <- as.factor(lasQuad2$ReturnNumber)

lidR::plot(lasQuad, bg = "white", color = "ReturnNumber", axis = FALSE)
#plot(lasQuad$X, lasQuad$Y, bg = "white", col = lasQuad$Z)


gnd <- lidR::filter_ground(lasQuad)
plot(gnd, size = 3, bg = "white", color = "Classification")

gndWilder <- classify_ground(outWilder, csf(TRUE, 1, 1, time_step = 1))
plot(gndWilder, color = "Classification")

noiseWilder <- classify_noise(gndWilder, sor())
plot(noiseWilder, color = "Classification", pal = viridis::magma)

lidR::filter_poi(noiseWilder, Classification == 18) %>%
  plot(color = "Intensity", pal = viridis::magma, bg = "white", size = 4)

lidR::filter_poi(noiseWilder, Classification != 18) %>%
  plot(color = "Intensity", pal = viridis::magma, bg = "white", size = 4)

rw <- st_minimum_rotated_rectangle(roofWilder) %>%
  st_coordinates()

las_tr <- clip_transect(gndWilder, rw[3,1:2], rw[2,1:2], width = 20, xz = TRUE) #right/parking_side
las_tr <- clip_transect(gndWilder, rw[1,1:2], rw[2,1:2], width = 20, xz = TRUE) #front/tower_face
las_tr <- clip_transect(gndWilder, rw[4,1:2], rw[2,1:2], width = 20, xz = TRUE) #invalid/interior
las_tr <- clip_transect(gndWilder, rw[3,1:2], rw[4,1:2], width = 20, xz = TRUE) #back
las_tr <- clip_transect(gndWilder, rw[1,1:2], rw[4,1:2], width = 20, xz = TRUE) #left/lawn_side
plot(las_tr, color = "Classification")

ggplot(las_tr@data, aes(X,Z, color = as.factor(Classification))) + 
  geom_point(size = 0.5) + 
  coord_equal() + 
  theme_minimal()

tr_tin <- rasterize_canopy(las_tr, res = 0.5, algorithm = p2r()) #res = 1, algorithm = tin()
plot_dtm3d(tr_tin, bg = "white") 

lidR::filter_poi(las_tr, Classification != 2) %>%
  plot(color = "Intensity", pal = viridis::magma, bg = "white", size = 4)
# add_dtm3d(tr_tin)
plot(tr_tin)


q <- st_cast(roofWilder, "LINESTRING")[2,] %>%
  st_minimum_rotated_rectangle() %>%
  st_coordinates()

# > a <- q[c(1,4),]
# > plot(a)
# > plot(st_cast(roofWilder, "LINESTRING")[2,], add = TRUE)

las_in <- clip_transect(noiseWilder, #lidR::filter_poi(noiseWilder, Classification != 18), 
                        q[1,1:2], q[4,1:2], width = 20, xz = TRUE)  # best scan angle profile (left interior)
plot(las_in, color = "Classification")


ab2 <- nngeo::st_segments(roofWilder, progress = FALSE)
ab2[,2] <- as.factor(1:nrow(ab2))
plot(ab2, col = ab2$V2)
plot(st_cast(roofWilder, "POINT"), add = TRUE)

#library(whitebox)

las.dir <- "C:/Users/jaria/Downloads/2015_ME_MA_lidar_Job847219/"
file.names <- list.files(path = las.dir, pattern = "Job847219_42072_*", full.names = TRUE)

wbt_lidar_info(file.names[7],
               "./smith.html")

wbt_clip_lidar_to_polygon(file.names[7],
                          bbox(lasQuad),
                          "./quad.laz")

wbt_classify_buildings_in_lidar(
  lasQuad,
  roofQuad,
  newQuad,
  wd = NULL,
  verbose_mode = NULL,
  compress_rasters = NULL,
  command_only = FALSE
)

whitebox::wbt_classify_lidar(file.names[7])

wbt_lidar_block_maximum(
  outWilder,
  output = NULL,
  resolution = 1,
  wd = NULL,
  verbose_mode = NULL,
  compress_rasters = NULL,
  command_only = FALSE
)

wbt_lidar_ransac_planes(
  outWilder,
  wbtWilder,
  radius = 2,
  num_iter = 50,
  num_samples = 5,
  threshold = 0.35,
  model_size = 8,
  max_slope = 80,
  classify = FALSE,
  last_returns = FALSE,
  wd = NULL,
  verbose_mode = NULL,
  compress_rasters = NULL,
  command_only = FALSE
)

las <- segment_shapes(las_tr, shp_plane(k = 15), "Coplanar")
plot(las, color = "Coplanar", bg = "white", size = 5)

wilderPath <- "C:/Users/jaria/Downloads/Floor Plans/Garden Neighborhood/Wilder House Floor Plan (Updated 2023).pdf"
wilder1 <- pdf_convert(wilderPath, format = "tiff", pages = 1)
wilder1 <- raster::stack(wilder1)
# u1_0 <- wilder1$Wilder.House.Floor.Plan..Updated.2023._1_1
# u1_0[u1_0[] == 255] = NA
# plot(u1_0)
plot(wilder1$Wilder.House.Floor.Plan..Updated.2023._1_1)


hist(wilder1, maxpixels = ncell(wilder1))
#wilder_vec <- st_contour(wilder1, contour_lines = TRUE) 
raster::contour(wilder1)

u1 <- unique(wilder1$Wilder.House.Floor.Plan..Updated.2023._1_1)

u1_50 <- wilder1$Wilder.House.Floor.Plan..Updated.2023._1_1
u1_50[u1_50[] > 50] = NA
par(mar = c(0,0,0,0), oma = c(0,0,0,0))
plot(u1_50, axes = FALSE, legend = FALSE, colNA = "black")

u1_100 <- wilder1$Wilder.House.Floor.Plan..Updated.2023._1_1
u1_100[u1_100[] <= 50 | u1_100[] > 100] = NA
par(mar = c(0,0,0,0), oma = c(0,0,0,0))
plot(u1_100, axes = FALSE, legend = FALSE, colNA = "black")

u1_150 <- wilder1$Wilder.House.Floor.Plan..Updated.2023._1_1
u1_150[u1_150[] <= 100 | u1_150[] > 150] = NA
par(mar = c(0,0,0,0), oma = c(0,0,0,0))
plot(u1_150, axes = FALSE, legend = FALSE, colNA = "black")

u1_200 <- wilder1$Wilder.House.Floor.Plan..Updated.2023._1_1
u1_200[u1_200[] <= 150 | u1_200[] > 200] = NA
par(mar = c(0,0,0,0), oma = c(0,0,0,0))
plot(u1_200, axes = FALSE, legend = FALSE, colNA = "black")

u1_250 <- wilder1$Wilder.House.Floor.Plan..Updated.2023._1_1
u1_250[u1_250[] <= 200] = NA
# u1_250[u1_250[] <= 254] = NA
par(mar = c(0,0,0,0), oma = c(0,0,0,0))
plot(u1_250, axes = FALSE, legend = FALSE)

u1_who <- wilder1$Wilder.House.Floor.Plan..Updated.2023._1_1
# u1_who[u1_who[] <= 146 | u1_who[] > 147] = NA
u1_who[u1_who[] <= 86 | u1_who[] > 90] = NA
par(mar = c(0,0,0,0), oma = c(0,0,0,0))
plot(u1_who, axes = FALSE, legend = FALSE, colNA = "black")

ubw <- wilder1$Wilder.House.Floor.Plan..Updated.2023._1_1
ubw[ubw[] == 255] = NA
ubw[!is.na(ubw[])] = 1
par(mar = c(0,0,0,0), oma = c(0,0,0,0))
plot(ubw, col = "black", legend = FALSE, axes = FALSE)
u1 <- unique(ubw)

# wilderText <- tesseract::ocr_data(wilderPg, engine = tesseract("eng"))
# wilderText <- pdftools::pdf_ocr_data(wilderPath, pages = 1)


im <- readImage("C:/Users/jaria/Documents/Wilder House Floor Plan (Updated 2023)_output_1.png")

res_slic = superpixels(input_image = im,
                       method = "slic",
                       superpixel = 200, 
                       compactness = 20,
                       return_slic_data = TRUE,
                       return_labels = TRUE, 
                       write_slic = "", 
                       verbose = TRUE)

res_slico = superpixels(input_image = im,
                        method = "slico",
                        superpixel = 200, 
                        return_slic_data = TRUE,
                        return_labels = TRUE, 
                        write_slic = "", 
                        verbose = TRUE)

par(mfrow=c(1,2), mar = c(0.2, 0.2, 0.2, 0.2))

plot_slic = OpenImageR::NormalizeObject(res_slic$slic_data)
plot_slic = grDevices::as.raster(plot_slic)
graphics::plot(plot_slic)

plot_slico = OpenImageR::NormalizeObject(res_slico$slic_data)
plot_slico = grDevices::as.raster(plot_slico)
graphics::plot(plot_slico)



# fknn <- function(X,Xp,cl,k=1)
# {
#   out <- nabor::knn(X,Xp,k=k)
#   cl[as.vector(out$nn.idx)] %>% matrix(dim(out$nn.idx)) %>% rowMeans
# }

img <- load.image("C:/Users/jaria/Documents/Wilder House Floor Plan (Updated 2023)_output_1.png")

# segmented_img <- grabcut(im, iter = 5)
#plot(segmented_im)

d <- sRGBtoLab(im) %>% as.data.frame(wide="c")%>%
  dplyr::select(-x,-y)
#Run k-means with 2 centers
km <- kmeans(d,2)
#Turn cluster index into an image
seg <- as.cimg(km$cluster,dim=c(dim(im)[1:2],1,1))
plot(im,axes=FALSE)
highlight(seg==1)

b <- blur_anisotropic(im, ampl=1e4, sharp=1)

im <- load.image(system.file('extdata/Leonardo_Birds.jpg',package='imager'))
im.noisy <- (im + 80*rnorm(prod(dim(im))))
blur_anisotropic(im,ampl=1e8, sharpness = 10) %>% plot
plot(im)

p <- rast("C:/Users/jaria/Documents/Wilder House Floor Plan (Updated 2023)_output_1.png")
p1 <- terra::as.polygons(p)
p2 <- plot(p1)
ubwS <- rast(ubw)
ubw1 <- terra::as.polygons(ubwS)
ubw2 <- sf::st_as_sf(ubw1)
ubw3 <- sf::st_cast(ubw2, "POLYGON")
ubw4 <- ubw3 %>% mutate(size = st_area(geometry))
hist(ubw4$size)

ggplot() +
  geom_sf(data = ubw2, fill = "white") +
  coord_sf()

ubw4 %>% 
  filter(size > 2e4) %>%
  ggplot() +
  geom_sf(fill = "white") +
  coord_sf()

f <- system.file("ex/lux.shp", package="terra")
v <- vect(f)
w <- simplifyGeom(v, .02, makeValid=FALSE)
e <- erase(w)
g <- gaps(e)
plot(e, lwd=5, border="light gray")
polys(g, col="red", border="red")

p3 <- simplifyGeom(p1, 20)
p4 <- plot(p3)

p5 <- simplifyGeom(p1, 200)
p6 <- plot(p5)

ps <- st_as_sf(p1)


## Foreground/background segmentation using imager
## Section 2. Gradient-based algorithm
## https://dahtah.github.io/imager/foreground_background.html

comstockPNG <- load.image("C:/Users/jaria/Downloads/comstock.png")
# plot(comstockPNG)
grad <- imgradient(comstockPNG,"xy")
# layout(t(1:2))
# plot(grad$x,main="Gradient along x")
# plot(grad$y,main="Gradient along y")
grad.sq <- grad %>% map_il(~ .^2)
# layout(t(1:2))
# plot(sqrt(grad.sq$x),main="Gradient magnitude along x")
# plot(sqrt(grad.sq$y),main="Gradient magnitude along y")
# plot(sqrt(grad.sq))
# plot(sqrt(edges),main="Detected edges")
grad.sq <- add(grad.sq) 
# plot(sqrt(grad.sq))
edges <- imsplit(grad.sq,"c") %>% add
# plot(sqrt(edges),main="Detected edges")
detect.edges <- function(im,sigma=1)
{
  isoblur(im,sigma) %>% imgradient("xy") %>% enorm %>% imsplit("c") %>% add
}

edges <- detect.edges(comstockPNG,2) %>% sqrt 
# plot(edges)
pmap <- 1/(1+edges) 
# plot(pmap,main="Priority map")
seeds <- imfill(dim=dim(pmap)) #Empty image
seeds[400,50,1,1] <- 1 #Background pixel 
seeds[600,450,1,1] <- 2 #Foreground pixel
wt <- watershed(seeds,pmap)
# plot(wt,main="Watershed segmentation")
mask <- add.colour(wt) #We copy along the three colour channels
# layout(t(1:2))
# plot(comstockPNG*(mask==1),main="Background")
# plot(comstockPNG*(mask==2),main="Foreground")

nshad <- naip_shadows
nshad[nshad[] != 1] = NA
nshad[nshad[] == 1] = 0
nshad[is.na(nshad[])] = 1

# un <- hillshader::matrix_to_raster(nshad, extent(quad_rgb), crs(quad_rgb))
# plot(un)

# un <- rast(nrow=nrow(quad_dtm), ncol=ncol(quad_dtm), extent=ext(quad_dtm))
# values(un) <- t(nshad)

#https://github.com/tylermorganwall/rayshader/blob/master/R/flipfunctions.R
fliplr = function(x) {
  if(length(dim(x)) == 2) {
    x[,ncol(x):1]
  } else {
    x[,ncol(x):1,]
  }
}

un <- rast(fliplr(t(nshad)))
ext(un) <- ext(quad_dtm)
crs(un) <- crs(quad_dtm)

uuu <- rast(quad_color)
ext(uuu) <- ext(quad_rgb)
crs(uuu) <- crs(quad_rgb)
# plotRGB(uuu, scale = 1)

qc <- "+proj=lcc +lat_0=41 +lon_0=-71.5 +lat_1=42.6833333333333 +lat_2=41.7166666666667 +x_0=200000.0001016 +y_0=750000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +vunits=us-ft +no_defs "

#u11 <- terra::disagg(uuu, fact = 1.97) 
# can't disagg; need to resample

footID <- c(56, 57, 63, 64, 65, 67, 72, 80, 81, 83, 85, 87, 90, 94, 96, 99, 101, 103, 104, 110, 113, 116, 120, 122, 123, 124, 127, 128, 129, 130, 131, 133, 137, 141, 144, 148, 155, 156) 
# campus_footprints$OBJECTID
feet <- campus_footprints %>%
  filter(OBJECTID %in% footID)
shoe <- st_bbox(feet)

#las67 <- lidR::readLAScatalog(file.names[c(6, 7)], progress = TRUE, filter = "-keep_first -drop_z_below 0 -drop_z_above 350")
lascamp <- lidR::clip_rectangle(las7, shoe[1], shoe[2], shoe[3], shoe[4])
las6 <- lidR::readLAScatalog(file.names[6], progress = FALSE, filter = "-drop_z_below 0 -drop_z_above 350")
lascampB <- lidR::clip_rectangle(las6, shoe[1], shoe[2], shoe[3], shoe[4])
#plot(lascamp, pal = viridis::magma, bg = "white")

# compound houses
# c(99, 113, 116, 129, 131, 141, 148) 
feet <- st_transform(feet, st_crs(lascamp)) %>% st_zm()
twin <- lidR::clip_roi(lascampB, feet$geometry[16])
plot(twin, pal = viridis::magma, bg = "white", axis = TRUE)

only <- lidR::clip_roi(lascamp, feet$geometry[6])
plot(only, pal = viridis::magma, bg = "white")

lasP <- segment_shapes(twin, shp_plane(k = 50), "plane")
plot(lasP, color = "plane", bg = "white", size = 3)

gndCSF <- classify_ground(outWilder, csf())
gndPMF <- classify_ground(outWilder, pmf(ws = 5, th = 3))
# gndMCC <- classify_ground(outWilder, mcc())
# Error: Package 'RMCC' needed for this function to work. Please install it.

plot(outWilder, color = "Classification", pal = rainbow,size = 2)
plot(lasQuad, color = "Classification", pal = rainbow,size = 2)
plot(gndPMF, color = "Classification", pal = rainbow,size = 2)
plot(gndCSF, color = "Classification", pal = rainbow,size = 2)




jpath <- "C:/Users/jaria/Downloads/Floor Plans/Paradise Neighborhood/Jordan House Floor Plan (Updated 2023).pdf"

j1 <- pdf_convert(jpath, format = "tiff", pages = 1)
j1 <- raster::stack(j1)
j1 <- j1$Jordan.House.Floor.Plan..Updated.2023._1_1
j1[j1[] == 255] = NA
j1[!is.na(j1[])] = 1
j1 <- rast(j1)

plot(j1)

j1_vect <- terra::as.polygons(j1) %>%
  sf::st_as_sf() %>%
  sf::st_cast("POLYGON")

ggplot() +
  geom_sf(data = j1_vect, fill = "white") +
  coord_sf()

j1_geo <- j1_vect %>% mutate(size = st_area(geometry))
# hist(j1_geo$size)
# max(j1_geo$size)

j1_geo %>% 
  filter(size > 2e4) %>%
  ggplot() +
  geom_sf(fill = "white") +
  geom_sf(data = st_boundary(j1_geo), color = "blue") +
  coord_sf()

j1_blank <- j1_geo %>% filter(size > 2e4)
ggplot() +
  geom_sf(data = j1_blank, fill = "white") +
  # geom_sf(data = st_buffer(j1_blank, dist = 10), color = "blue") +
  # geom_sf(data = st_boundary(st_buffer(j1_blank)), color = "red") +
  coord_sf()

j2 <- pdf_convert(jpath, format = "tiff", pages = 2)
j2 <- raster::stack(j2)
j2 <- j2$Jordan.House.Floor.Plan..Updated.2023._2_1
j2[j2[] == 255] = NA
j2[!is.na(j2[])] = 1
# plot(j2)

j2 <- rast(j2)
j2_vect <- terra::as.polygons(j2) %>%
  sf::st_as_sf() %>%
  sf::st_cast("POLYGON")

ggplot() +
  geom_sf(data = j2_vect, fill = "white") +
  coord_sf()

j2_geo <- j2_vect %>% mutate(size = st_area(geometry))
# hist(j2_geo$size)
# max(j2_geo$size)

j2_geo %>% 
  filter(size > 2e4) %>%
  ggplot() +
  geom_sf(fill = "white") +
  coord_sf()

j2_blank <- j2_geo %>% filter(size > 2e4)
ggplot() +
  geom_sf(data = st_buffer(j2_blank, dist = 4), fill = "khaki3", color = "khaki4") +
#  geom_sf(data = st_boundary(st_buffer(j2_blank)), color = "red") +
#  geom_sf(data = j2_blank, fill = "white") +
  coord_sf() + 
  theme_void()

ggplot() +
  geom_sf(data = j1_vect, fill = "red") +
  geom_sf(data = j2_vect, fill = "blue") +
  coord_sf()

j2_wk <- wk_transform(j2_blank, wk_affine_scale(scale_x = 1.135742, scale_y = 1.135742))
j2_wk <- wk_transform(j2_wk, wk_affine_translate(dx = -6, dy = -79))
# j2_wk <- wk_transform(j2_wk, wk_affine_rotate(-90))

# (to normalize floors in a single floor_plan.pdf):

# explanation for << scale_xy = 1.125742 >>
# > st_bbox(j1_blank)
#   xmin ymin xmax ymax 
#     28  315 1191  714 
# > 1191-28
#   [1] 1163
# > st_bbox(j2_blank)
#   xmin ymin xmax ymax 
#     39  266 1063  651 
# > 1063-39
#   [1] 1024
# > 1163/1024
#   [1] 1.135742


# alternative method of getting scale factor 
# - get diameter of an inscribed circle that spans trunk of the building  
# - note !! j2_line is defined on line 718
# ggplot() + 
#   geom_sf(data = j2_line$geometry[2], color = "royalblue") + 
#   geom_sf(data = st_inscribed_circle(st_cast(j2_line$geometry[2], "POLYGON"), dTolerance = 36)) + 
#   coord_sf()

ggplot() +
  geom_sf(data = j1_blank, fill = "red") +
  geom_sf(data = j2_wk, fill = "blue") +
  coord_sf()

#lidR::filter_poi(lasQuad, NumberOfReturns > 2 & Intensity < 10000) %>%
plot(color = "Intensity", pal = viridis::magma, bg = "white", size = 4)
plot(lasQuad, color = "NumberOfReturns", pal = viridis::magma, bg = "white", size = 4)

lidR::filter_poi(lasQuad, ReturnNumber == 1) %>%
  plot(color = "NumberOfReturns", pal = viridis::magma, bg = "white", size = 4)

lidR::filter_duplicates(lasQuad) %>%
  plot(color = "NumberOfReturns", pal = viridis::magma, bg = "white", size = 4)

qnoise <- lidR::classify_noise(lasQuad, sor(k = 10, m = 3, quantile = FALSE)) 
lidR::filter_poi(qnoise, Classification != LASNOISE) %>%
  plot(color = "NumberOfReturns", pal = viridis::magma, bg = "white", size = 4)


j2_line <- j2_wk
j2_line <- st_cast(j2_line, "LINESTRING")
j2_line$size <- st_length(j2_line$geometry)

# ggplot() +
#   geom_sf(data = j2_line$geometry[which.max(j2_line$size)], color = "royalblue") +
#   coord_sf()

j2_line <- j2_line[order(j2_line$size, decreasing = TRUE),]

ggplot() +
  geom_sf(data = j2_line$geometry[2], color = "royalblue") +
#  geom_sf(data = j1_blank, fill = "gold") +
  coord_sf()

# lidR, 3. Ground Classification
# 3.1 Progressive Morphological Filter -------------
ws <- seq(3, 12, 3)
th <- seq(0.1, 1.5, length.out = length(ws))

lascamp2 <- lascamp %>%
  classify_ground(algorithm = pmf(ws = ws, th = th))
# --------------------------------------------------
mycsf <- csf(sloop_smooth = TRUE, class_threshold = 1.6, cloth_resolution = 1.6)
# Sys.time()
lascamp2 <- classify_ground(lascamp, mycsf)
# Sys.time()
# plot(lascamp2, color = "Classification", size = 3, bg = "white") 
# Sys.time()
lascampB2 <- classify_ground(lascampB, mycsf)
# Sys.time()
# plot(lascampB2, color = "Classification", size = 3, bg = "white") 
# Sys.time()
lidR::filter_poi(lascamp2, Classification != 2) %>%
  plot(color = "NumberOfReturns", pal = viridis::magma, bg = "white", size = 2)
Sys.time()
lidR::filter_poi(lascamp2, Classification != 2 & NumberOfReturns == 1) %>%
  plot(color = "Intensity", pal = viridis::magma, bg = "white", size = 2)
Sys.time()

# lidR, 8. Derived Metrics
# 8.4.2 Fractal dimensions -----------------------------
fd = function(X,Y,Z) {
  M = cbind(X,Y,Z)
  Rdimtools::est.boxcount(M)$estdim
}
Sys.time()
surfB2 <- pixel_metrics(lascampB2, func = ~fd(X,Y,Z), 5)
Sys.time()
# ------------------------------------------------------
Sys.time()
M <- lidR::point_eigenvalues(lascampB2, k = 25, filter = ~Classification != 2, metrics = TRUE)
W <- lidR::point_eigenvalues(lascamp2, k = 25, filter = ~Classification != 2, metrics = TRUE)
# Sys.time()
# M$is_planar = M$eigen_medium > (25*M$eigen_smallest) & (6*M$eigen_medium) > M$eigen_largest
# lascampB2 <- add_attribute(lascampB2, FALSE, "is_planar")
# lascampB2$is_planar[M$pointID] <- M$is_planar
# filter_poi(lascampB2, Classification != 2) %>%
#   plot(color = "is_planar", bg = "white")
# 
# lascampB2 <- add_attribute(lascampB2, FALSE, "planarity")
# lascampB2$planarity[M$pointID] <- M$planarity
# filter_poi(lascampB2, Classification != 2) %>%
#   plot(color = "planarity", pal = viridis::magma, bg = "white")

# lascampB2 <- add_attribute(lascampB2, FALSE, "linearity")
lascampB2 <- add_attribute(lascampB2, FALSE, "sphericity")
lascampB2 <- add_attribute(lascampB2, FALSE, "anisotropy")
# lascampB2$linearity[M$pointID] <- M$linearity
lascampB2$sphericity[M$pointID] <- M$sphericity
lascampB2$anisotropy[M$pointID] <- M$anisotropy

lascamp2 <- add_attribute(lascamp2, FALSE, "sphericity")
lascamp2 <- add_attribute(lascamp2, FALSE, "anisotropy")
lascamp2$sphericity[W$pointID] <- W$sphericity
lascamp2$anisotropy[W$pointID] <- W$anisotropy


# lampB <- lidR::merge_spatial(lascampB2, feet, "dorm") %>%
#   filter_poi(Classification != 2)
# lampB2 <- filter_poi(lampB, dorm == TRUE)
# plot(lampB, color = "anisotropy", pal = viridis::magma, bg = "white")

# hist(lascampB2$anisotropy[(lascampB2$Classification != 2) &
#                             (lascampB2$anisotropy > 0.6)])
lidR::filter_poi(lascampB2, Classification != 2 & anisotropy > 0.7) %>% 
  plot(color = "anisotropy", pal = viridis::magma, bg = "white", legend = TRUE, size = 2)

# hist(lascampB2$sphericity[(lascampB2$Classification != 2) &
#                             (lascampB2$sphericity < 0.2)])
lidR::filter_poi(lascampB2, Classification != 2 & sphericity < 0.2) %>% 
  plot(color = "sphericity", pal = viridis::magma, bg = "white", legend = TRUE, size = 2)

# hist(lascamp2$anisotropy[(lascamp2$Classification != 2) &
#                             (lascamp2$anisotropy > 0.6)])
lidR::filter_poi(lascamp2, Classification != 2 & anisotropy > 0.7) %>% 
  plot(color = "anisotropy", pal = viridis::magma, bg = "white", legend = TRUE, size = 2)

# hist(lascamp2$sphericity[(lascamp2$Classification != 2) &
#                             (lascamp2$sphericity < 0.4)])
lidR::filter_poi(lascamp2, Classification != 2 & sphericity < 0.25) %>% 
  plot(color = "sphericity", pal = viridis::magma, bg = "white", legend = TRUE, size = 2)


pixB2A <- lascampB2 %>%
  filter_poi(Classification != 2) %>%
  pixel_metrics(~mean(anisotropy), res = 5)

pix2A <- lascamp2 %>%
  filter_poi(Classification != 2 & anisotropy > 0.7) %>%
  pixel_metrics(~mean(anisotropy), res = 5)

pixB2A[pixB2A < 0.75] = NA
# lidR::plot(pixB2A)

pixB2N <- lascampB2 %>%
  filter_poi(Classification != 2) %>%
  pixel_metrics(~length(Z), res = 5)

pix2N <- lascamp2 %>%
  filter_poi(Classification != 2) %>%
  pixel_metrics(~length(Z), res = 5)

pixB2N[pixB2N > 100] = NA
# lidR::plot(pixB2N)


# feet[16] = chase;  feet[31] = jordan;  feet[6] = tyler
west_dtm <- lidR::rasterize_canopy(lascamp, res = 1, algorithm = dsmtin(), pkg = "raster")
east_dtm <- lidR::rasterize_canopy(lascampB, res = 1, algorithm = dsmtin(), pkg = "raster")

roofJordan <- st_as_sf(feet$geometry[31]) %>% st_zm(drop = TRUE)
roofChase <- st_as_sf(feet$geometry[16]) %>% st_zm(drop = TRUE)
roofTyler <- st_as_sf(feet$geometry[6]) %>% st_zm(drop = TRUE)

jordan_dtm <- crop(west_dtm, roofJordan) #%>% mask(feet$geometry[31])
chase_dtm <- crop(east_dtm, roofChase) #%>% mask(feet$geometry[16])

col <- height.colors(25)
plot(jordan_dtm, col = col)
plot(chase_dtm, col = col)

lasJordan <- lidR::clip_roi(lascamp, roofJordan)
lasChase <- lidR::clip_roi(lascampB, roofChase)

hist(lasJordan$Z)
hist(lasChase$Z)


plot(lasJordan, pal = viridis::magma, bg = "white", legend = TRUE, size = 2)

ggJ <- as.data.frame(cbind(pID = lasJordan$PointSourceID, Z = lasJordan$Z))



ggJ %>%
  mutate(Z = round(Z)) %>%
  group_by(Z) %>%
  summarise(N = n()) %>%
  ggplot(aes(Z, N)) +
  geom_point(color = "darkorange")
  
hist(ggJ$Z)

jvox <- voxel_metrics(lasJordan, length(Z), 4)
# hist(jvox$V1)
# nrow(jvox[jvox$V1 > 20])
# jvox <- jvox %>% filter(V1 < 30)
plot(jvox, bg = "white", pal = viridis::magma(20), voxel = TRUE, legend = TRUE)



N1 <- lidR::point_metrics(lascampB2, ~sd(anisotropy, na.rm = TRUE), k = 30, filter = ~Classification != 2)
names(N1)[2] <- "sdA"
# N2 <- lidR::point_metrics(lascampB2, ~sd(sphericity, na.rm = TRUE), r = 5, filter = ~Classification != 2)
# names(N2)[2] <- "sdS"

# lascampB2 <- add_attribute(lascampB2, NA, "sdA")
# lascampB2 <- add_attribute(lascampB2, NA, "sdS")
lascampB2$sdA[N1$pointID] <- N1$sdA
# lascampB2$sdS[N2$pointID] <- N2$sdS

lidR::filter_poi(lascampB2, Classification != 2 & sdA < 0.12) %>% 
  plot(color = "sdA", pal = viridis::magma, bg = "white", legend = TRUE, size = 2)
# lidR::filter_poi(lascampB2, Classification != 2 & sdS < 0.02) %>% 
#   plot(color = "sdS", pal = viridis::magma, bg = "white", legend = TRUE, size = 2)

M$eigentropy <- (-1*M[,2]*log(M[,2])) + (-1*M[,3]*log(M[,3])) + (-1*M[,4]*log(M[,4])) 
lascampB2 <- add_attribute(lascampB2, NA, "eigentropy")
lascampB2$eigentropy[M$pointID] <- M$eigentropy
lidR::filter_poi(lascampB2, Classification != 2 & eigentropy > -10) %>% 
  plot(color = "eigentropy", pal = viridis::magma, bg = "white", legend = TRUE, size = 2)

# M$omnivariance <- (M[,2]*M[,3]*M[,4])^(1/3)
lascampB2 <- add_attribute(lascampB2, NA, "lambda1")
lascampB2 <- add_attribute(lascampB2, NA, "lambda3")
lascampB2$lambda1[M$pointID] <- M$eigen_largest
lascampB2$lambda3[M$pointID] <- M$eigen_smallest
lidR::filter_poi(lascampB2, Classification != 2 & lambda1 < 5) %>% 
  plot(color = "lambda1", pal = viridis::magma, bg = "white", legend = TRUE, size = 2)
lidR::filter_poi(lascampB2, Classification != 2 & lambda3 < 1) %>% 
  plot(color = "lambda3", pal = viridis::magma, bg = "white", legend = TRUE, size = 2)

mm <- lidR::point_eigenvalues(lascampB2, k = 25, filter = ~Classification != 2, metrics = TRUE)



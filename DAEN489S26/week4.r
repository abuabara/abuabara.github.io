### VECTOR ###
setwd("/Users/abuabara/Downloads/")

library(sf)
library(dplyr)

parcels_2025 <- read_sf("/Users/abuabara/Downloads/Public_Parcel_Boundary_certified_2025/Public_Parcel_Boundary_certified.shp")

head(parcels_2025)

glimpse(parcels_2025)

unique(parcels_2025$state_cd) %>% sort()

subdivisions <- read_sf("/Users/abuabara/Downloads/Public_Parcel_Boundary_certified_2025/Subdiv_Boundary.shp")

head(subdivisions)

join <- st_join(parcels_2025, subdivisions)
# join2 <- st_join(parcels_2025, subdivisions, join = st_within)
# join3 <- st_join(parcels_2025, subdivisions, join = st_within, largest = TRUE)
# join4 <- st_join(parcels_2025, subdivisions, join = st_contains)

glimpse(join)

unique(join$SUBDIV_NAM)

join$state_cd_2 <- substr(join$state_cd, 1, 1)

unique(join$state_cd_2)

join %>% filter(SUBDIV_NAM == "RAINTREE") %>% count()

st_join(parcels_2025, subdivisions) |>
  st_make_valid() |>
  st_drop_geometry() |>
  transmute(SUBDIV_NAM,
            Imprv_Val,
            state_cd_2 = substr(state_cd, 1, 1)) |>
  filter(state_cd_2 == "A") |>
  group_by(SUBDIV_NAM) |>
  summarize(n = n(),
            Imprv_Val = sum(Imprv_Val, na.rm = TRUE)
            ) |>
  arrange(desc(n)) |>
  filter(n>1000) |> adorn_totals("row")

library(janitor)

# st_write(join, "join.shp")

### RASTER ###

library(terra)

r1 <- rast("/Users/abuabara/Downloads/usgs17-70cm-brazos-freestone-robertson_3096301_dem/usgs17-1m_14RQU515870.img")
r2 <- rast("/Users/abuabara/Downloads/usgs17-70cm-brazos-freestone-robertson_3096301_dem/usgs17-1m_14RQU515885.img")
r3 <- rast("/Users/abuabara/Downloads/usgs17-70cm-brazos-freestone-robertson_3096301_dem/usgs17-1m_14RQU515900.img")

plot(r1)
plot(r2)
plot(r3)

r1
print(r1)

crs(r1, describe = TRUE)
ext(r1)
(xmax(r1) - xmin(r1))
ncol(r1)
(ymax(r1) - ymin(r1))
nrow(r1) 

crs(r1, describe = TRUE)$units
# but i know that UTM zones use METERS

cat(crs(r1, proj = TRUE))

res(r1)

(ymax(r1) - ymin(r1)) / nrow(r1)  # = 1500 / 1500 = 1 meter

# number of bands
nlyr(r1)

# band names
names(r1)

# detailed metadata
describe(r1)

# basic info
print(r1)

# wavelength information (if available)
metags(r1)

plotRGB(r1, r = 1, g = 1, b = 1, stretch = "lin")

summary(r1)

summary(r1[[1]])

hist(r1[[c(1)]])

# Common satellite band orders
#  Landsat 8/9 (typical):
#   Band 1: Coastal/Aerosol
#   Band 2: Blue
#   Band 3: Green
#   Band 4: Red
#   Band 5: NIR
#   Band 6-7: SWIR
#  Sentinel-2 (typical):
#   Band 2: Blue
#   Band 3: Green
#   Band 4: Red
#   Band 8: NIR

# mathematical operations
# add rasters
sum_raster <- r1 + r2 + r3

# average multiple rasters
mean_raster <- mean(c(r1, r2, r3))

# using app() for custom functions
result <- app(c(r1, r2, r3), fun = mean, na.rm = TRUE)

# stacking/layering rasters >> combine into multi-layer raster
stacked <- c(r1, r2, r3)

# no worries --> i know it doesn"t work, we will do this on data cubes!

# merging/mosaicking rasters >> or if they"re adjacent with no overlap
merged <- merge(r1, r2)

plot(merged)

merged <- merge(merged, r3)

plot(merged)

# merge rasters that don"t overlap or overlap slightly
merged <- mosaic(r1, r2, r3, fun = "mean")

plot(merged)

# LOOP
# get list of all raster files (adjust folder and pattern for your file type)
raster_files <- list.files("/Users/abuabara/Downloads/usgs17-70cm-brazos-freestone-robertson_3096301_dem/",
                           pattern = "\\.(tif|img|grd)$",
                           full.names = TRUE)

raster_files

# read all rasters into a list
raster_list <- lapply(raster_files, rast)

# mosaic all rasters together
merged_raster <- do.call(mosaic, raster_list)

plot(merged_raster)

# save
writeRaster(merged_raster, "merged_output.tif", overwrite = TRUE)

###############################

# define NDVI function
# ndvi_fun = function(nir, red) {
#   (nir - red) / (nir + red)
# }

# apply to specific bands (e.g., band 4 = NIR, band 3 = Red)
# ndvi <- lapp(r1[[c(4, 3)]], fun = ndvi_fun)

# plot(ndvi, main = "NDVI")

###############################

plot(r1)

w = matrix(1, nrow=3, ncol=3)

w

focal(r1,
      w = w,
      fun = min)

terrain(r1, v="slope")

plot(terrain(r1, v="slope"))

plot(c(r1, terrain(r1, v = "slope")),
     main = c("Original", "Slope"))

# elevation variability
# describes how much elevation changes around a location
# quantifies how rough or rugged the terrain is within a defined neighborhood

# high TRI -> large differences between a cell and its neighbors (mountains, cliffs)
# low TRI -> little elevation change (flat plains)

tri <- terrain(r1, v = "TRI", neighbors = 8)
# how many neighboring cells to use to compute slope or aspect with
# either 8 (queen case) or 4 (rook case)

plot(tri, main = "Terrain Ruggedness Index")

# relative topographic position
# describes whether a location is higher or lower than the surrounding area
# compares a cell elevation to the mean elevation of its neighborhood

# positive TPI -> higher than surroundings (ridge, hilltop)
# negative TPI -> lower than surroundings (valley, depression)
# near zero -> similar to surroundings (mid-slope or flat area)

tpi <- terrain(r1, v = "TPI", neighbors = 8)

plot(tpi, main = "Topographic Position Index")

# from internet
# TRI answers: How uneven is the terrain here?
# TPI answers: Is this spot relatively high or low compared to nearby terrain?
# both are scale-dependent, meaning the interpretation changes with neighborhood size

# this is for next class ... need another raster to define "zones"

# z = zonal(
#   r1,
#   zones,
#   fun = "mean"
# )
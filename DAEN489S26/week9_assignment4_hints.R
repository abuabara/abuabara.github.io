library(sf)

# copy and paste from Google Maps
lon <- -96.3398
lat <- 30.6212

# geometry point
point_geom <- st_point(c(lon, lat))

# convert geometry to an sf object
point_sf <- st_sf(
  location = "Texas A&M",
  geometry = st_sfc(point_geom),
  crs = 4326 # standard GPS coordinate system
)

locations <- data.frame(
  name = c("Texas A&M", "Austin", "Houston"),
  lon = c(-96.3398, -97.7431, -95.3698),
  lat = c(30.6212, 30.2672, 29.7604)
)

points_sf <- st_as_sf(
  locations,
  coords = c("lon","lat"),
  crs = 4326
)

plot(st_geometry(points_sf))

st_write(points_sf, "points_sf.parquet", driver = "Parquet")


library(dplyr)
points_sf %>% 
  st_buffer(200) %>%
  mutate(geometry = st_as_binary(geometry))

# st_write(points_sf, "points_sf.shp")
# st_write(st_buffer(points_sf, 200), "points_sf_buff.shp")

library(tmap)

tmap_mode("view") 

tm_shape(points_sf) +
  tm_dots(fill = "maroon", size = 1)

st_distance(points_sf[1,], points_sf[2,])

library(units)

# distance in miles
set_units(st_distance(points_sf[1,], points_sf[2,]), "miles") # CS-Austin

set_units(st_distance(points_sf[1,], points_sf[3,]), "miles") # CS-Houston

library(sf)

# create two points (longitude, latitude)
p1 <- st_sfc(st_point(c(-73.9352, 40.7306)), crs = 4326) # NYC
p2 <- st_sfc(st_point(c(-118.2437, 34.0522)), crs = 4326) # LA

# calculate distance
dist <- st_distance(p1, p2)
print(dist)

# for instance ...
cities_df <- data.frame(
  city = c("New York", "London", "Tokyo", "Paris", "Sydney"),
  lon = c(-74.006, -0.1278, 139.6503, 2.3522, 151.2093),
  lat = c(40.7128, 51.5074, 35.6762, 48.8566, -33.8688),
  population = c(8336000, 8982000, 13960000, 2161000, 5312000)
)

# convert to an sf object
# 'coords' specifies the columns; 'crs = 4326' sets it to WGS84 (Lat/Lon)
cities_sf <- st_as_sf(cities_df, coords = c("lon", "lat"), crs = 4326)

# calculate distances between all points in 'cities_sf'
dist_matrix <- st_distance(cities_sf)

dist_matrix

# get the distance to the nearest neighbor only
min_dist <- apply(st_distance(cities_sf), 1, function(x) min(x[x > 0]))

min_dist

# install.packages("crsuggest")
library(crsuggest)

suggested_crs <- suggest_crs(pts_geo)
# this returns a tibble of the best projections for your area!

library(terra)

locations <- data.frame(
  name = c("ETB", "Zach", "HEB"),
  lon = c(-96.33955098537538, -96.34029241018128, -96.31873420556894),
  lat = c(30.62288537944505, 30.621198717730277, 30.6129458794491)
)

points_sf <- st_as_sf(
  locations,
  coords = c("lon","lat"),
  crs = 4326
)

plot(points_sf)

library(ggplot2)
library(ggspatial)

ggplot(data = points_sf) +
  annotation_map_tile(type = "osm", zoom = 15) +
  geom_sf(color = "#500000", size = 4) +
  # using the original 'locations' dataframe for the labels instead
  geom_text(data = locations, aes(x = lon, y = lat, label = name), 
            nudge_y = 0.0005, fontface = "bold") +
  theme_minimal() + coord_sf()

# distance in yards/miles
set_units(st_distance(points_sf[1,], points_sf[2,]), "yards") # ETB-Zach

set_units(st_distance(points_sf[1,], points_sf[3,]), "km")    # ETB-HEB Texas Av.

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/TAMU/Teaching/2025:2026/2026 Spring/DAEN ISEN 489/Assignment4_sol")

ls2021day <- rast("./landscan-usa/landscan-usa-2021-conus-day.tif")

library(tigris)

tx <- states(cb = TRUE) |> dplyr::filter(STUSPS == "TX")

tx <- st_transform(tx, crs(ls2021day))

ls2021day_crop <- crop(ls2021day, tx)

ls2021day_mask <- mask(ls2021day_crop, tx)

plot(ls2021day)

plot(ls2021day_mask)

plot(st_geometry(points_sf), add = TRUE, col = "red", pch = 19)

extract(ls2021day_mask, points_sf)
#   ID landscan-usa-2021-conus-day
# 1  1                         346
# 2  2                         507
# 3  3                        1575

plot(crop(ls2021day_mask, st_buffer(points_sf[1,],100)))
plot(st_geometry(points_sf[1,]), col = "red", pch = 19, add = TRUE)

plot(crop(ls2021day_mask, st_buffer(points_sf[3,],100)))
plot(st_geometry(points_sf[3,]), col = "red", pch = 19, add = TRUE)

ls2019day <- rast("./landscan-usa/landscan-usa-2019-conus-day.tif")
ls2020day <- rast("./landscan-usa/landscan-usa-2020-conus-day.tif")

ls2019day_crop <- crop(ls2019day, tx)
ls2019day_mask <- mask(ls2019day_crop, tx)

ls2020day_crop <- crop(ls2020day, tx)
ls2020day_mask <- mask(ls2020day_crop, tx)

cube <- c(ls2019day_mask, ls2020day_mask, ls2021day_mask)

names(cube) <- c("ls2019day", "ls2020day", "ls2021day")

extract(cube, points_sf)
#   ID ls2019day ls2020day ls2021day
# 1  1        55        55       346
# 2  2         0         0       507
# 3  3      1445      1445      1575

buffered_points <- st_buffer(points_sf, 100)

ggplot(data = buffered_points) +
  annotation_map_tile(type = "osm", zoom = 15) +
  geom_sf(color = "#500000", size = 4) +
  geom_text(data = locations, aes(x = lon, y = lat, label = name), 
            nudge_y = 0.0005, fontface = "bold") +
  theme_minimal()

extract(cube, buffered_points)
#    ID ls2019day ls2020day ls2021day       
# 1   1         0         0         1
# 2   1         0         0         0
# 3   1        55        55       346
# 4   1         0         0       182
# 5   1         0         0        34
# 6   2         0         0       190
# 7   2        95        95       126
# 8   2         0         0       507
# 9   2       164       164       247
# 10  2       159       159       168
# 11  3       186       186       129
# 12  3       468       468       917
# 13  3      1445      1445      1575
# 14  3        51        51        15
# 15  3       788       788      1212

extract(cube, buffered_points, fun=sum)
#   ID ls2019day ls2020day ls2021day        
# 1  1        55        55       563
# 2  2       418       418      1238
# 3  3      2938      2938      3848

extract(cube, buffered_points, fun=mean)
#   ID ls2019day ls2020day ls2021day        
# 1  1      11.0      11.0     112.6
# 2  2      83.6      83.6     247.6
# 3  3     587.6     587.6     769.6

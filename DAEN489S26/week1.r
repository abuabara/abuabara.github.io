# install.packages(tidyverse)

# install.packages(sf)

############################################################

library(tidyverse)
library(sf)

nc <- system.file("gpkg/nc.gpkg", package="sf") |> read_sf()
# system.file("gpkg/nc.gpkg", package="sf") |> read_sf() -> nc

glimpse(nc)

nc.32119 <- st_transform(nc, 'EPSG:32119')

nc.32119 |> select(BIR74) |> plot(graticule = TRUE, axes = TRUE)

############################################################

library(tmap)

tmap_mode("view")

qtm(nc.32119, fill = "BIR74", fill_alpha = .5)

############################################################

# install.packages(stars)

############################################################

library(stars)

par(mfrow = c(1, 2))

tif <- system.file("tif/L7_ETMs.tif", package = "stars")

x <- read_stars(tif)[,,,1]

image(x, main = "(a)")

image(x[,1:10,1:10], text_values = TRUE, border = 'grey', main = "(b)")

############################################################

# install.packages(tigris)

# install.packages(tidycensus)

############################################################

par(mar = c(2, 2, 2, 2), mfrow = c(1, 1), pty = "s")


library(dplyr)
library(sf)

sf_use_s2(FALSE) # spherical geometry

library(tmap)

tmap_mode("plot") # static maps
tmap_options(frame = FALSE)

library(units)

# Define the grid extent (4 wide, 4 tall)
xmin <- 0
xmax <- 40
ymin <- 0
ymax <- 40

# Create the grid
grid <-
  st_make_grid(
    st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)),
    cellsize = 10, # 10 units cells
    square = TRUE,
    what = "polygons"
  )

print(grid, n=20)

plot(grid)
plot(grid[1], col = "red", add = TRUE)
plot(grid[2], col = "green", add = TRUE)
plot(grid[3], col = "blue", add = TRUE)
plot(grid[6], col = "yellow", add = TRUE)
plot(grid[16], col = "brown", add = TRUE)
axis(1, at = seq(0, 40, 10))
axis(2, at = seq(0, 40, 10))
grid()

# Convert to sf object
grid_sf      <- st_sf(geometry = grid)
grid_sf$id   <- 1:nrow(grid_sf)  # Add IDs
grid_sf$area <- st_area(grid_sf) # Area
grid_sf
plot(grid_sf["id"], border = "red", col = "white", main = "")

tm_shape(grid_sf) + 
  tm_borders(col = "red", lwd = 1)

st_crs(grid_sf)

st_crs(4326)
st_set_crs(grid_sf, 4326)

st_crs(32610) # example: UTM Zone 10N
st_crs(32614) # example: UTM Zone 14N
st_set_crs(grid_sf, "+proj=cart +ellps=sphere") # sphere
st_set_crs(grid_sf, "+proj=cart +ellps=WGS84") # ellipse

st_crs("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")
st_set_crs(grid_sf, "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")

grid_sf_projected <- st_set_crs(grid_sf, 4326)

# These coordinates are longitude/latitude on the WGS-84 Earth model.
# Technically: Longitude: −180° to +180°
#              Latitude: −90° to +90°
# EPSG code: 4326

# | Task                           | CRS you should use                    |
# | ------------------------------ | ------------------------------------- |
# | Mapping / web display          | WGS84 (EPSG:4326)                     |
# | Distance, area, buffers, grids | Projected CRS (UTM, Albers, etc.)     |
# | Texas                          | EPSG:3083 (Texas Albers) or UTM 14/15 |

tm_shape(grid_sf_projected) + 
  tm_borders(col = "red", lwd = 1) + 
  tm_crs(crs = "+proj=robin")

data(World)

st_crs(World)

tm_shape(World) +
  tm_fill(fill = "grey") +
  tm_borders(col = "blue", lwd = .3) +
  tm_shape(grid_sf_projected) + 
  tm_borders(col = "red", lwd = 2) + 
  tm_scalebar(position = c("left", "bottom"), text.size = .9) +
  tm_crs(crs = "+proj=robin") +
  # tm_crs(crs = 4326) +
  # tm_crs(crs = 32610) +
  # tm_crs(crs = 32614) +
  tm_graticules(col = "gray70", lwd = 0.8, lty = "dotted")

grid_sf_projected$area_4326 <- st_area(grid_sf_projected)
grid_sf_projected

grid_sf_projected$area_4326 <- set_units(grid_sf_projected$area_4326, km^2)
grid_sf_projected

grid_sf_projected$area_4326 <- set_units(grid_sf_projected$area_4326, mi^2)
grid_sf_projected

grid_sf_projected$area_32610 <- set_units(st_area(st_transform(grid_sf_projected, crs = 32610)), mi^2)
grid_sf_projected |> st_drop_geometry()

# Four projection systems ----------------
albers_crs   <- "+proj=aea  +lat_0=23         +lat_1=29.5       +lat_2=45.5        +lon_0=-96   +datum=WGS84 +units=m"
equidist_crs <- "+proj=eqdc +lat_0=31.1666667 +lat_1=27.4166667 +lat_2=34.9166667  +lon_0=-100  +datum=WGS84 +units=m"
lcc_crs      <- "+proj=lcc  +lat_0=31.1666667 +lat_1=27.4166667 +lat_2=34.9166667  +lon_0=-100  +datum=WGS84 +units=m"
aeqd_crs     <- "+proj=aeqd +lat_0=30.7                                            +lon_0=-96.3 +datum=WGS84 +units=m"

grid_sf_projected$area_albers <- set_units(st_area(st_transform(grid_sf_projected, crs = albers_crs)), mi^2)
grid_sf_projected$area_equidist <- set_units(st_area(st_transform(grid_sf_projected, crs = equidist_crs)), mi^2)
grid_sf_projected$area_lcc <- set_units(st_area(st_transform(grid_sf_projected, crs = lcc_crs)), mi^2)
grid_sf_projected$area_aeqd <- set_units(st_area(st_transform(grid_sf_projected, crs = aeqd_crs)), mi^2)
grid_sf_projected |> st_drop_geometry()

grid_sf_projected$area_true <- set_units(st_area(st_transform(grid_sf_projected, 102022)), mi^2)

africa_aea <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +datum=WGS84 +units=m +no_defs"
# Error: crs not found

grid_sf_projected$area_true <- set_units(st_area(st_transform(grid_sf_projected, africa_aea)), mi^2)
grid_sf_projected |> st_drop_geometry()

tm_shape(World) +
  tm_fill(fill = "grey") +
  tm_borders(col = "blue", lwd = .3) +
  tm_shape(grid_sf_projected) +
  tm_borders(col = "red", lwd = 2) +
  tm_scalebar(position = c("left", "bottom"), text.size = .9) +
  tm_crs(crs = africa_aea) +
  tm_graticules(col = "gray70", lwd = 0.8, lty = "dotted")

# Census ----------------------------------
library(tigris)
us     <- states(cb = FALSE, year = 2022) # %>% filter(NAME == "Texas")
texas  <- counties(state = "TX", cb = FALSE, year = 2022) # %>% filter(NAME == "Texas")
brazos <- tracts(county = "Brazos", state = "TX", cb = FALSE, year = 2022)
st_crs(brazos)
# 4269

brazos$area_calc <- st_area(brazos)
brazos$diff <- brazos$ALAND + brazos$AWATER - as.numeric(brazos$area_calc)
sum(brazos$diff)
# -71.27524

brazos$area_4203 <- st_area(st_transform(brazos, 4203))
brazos$diff <- brazos$ALAND + brazos$AWATER - as.numeric(brazos$area_4203)
sum(brazos$diff)
# -10994.88

# NAD83 / Texas Central (FIPS 4203) --> EPSG: 32139
# Purpose	Engineering / cadastral mapping
# Preserves	shape and angles
# Does not preserve	area: Lambert Conformal Conic preserves angles, not area.
# Area bias across Brazos County runs about 0.15–0.30% — small, but real and systematic.
# For scientific, legal, or statistical area work, this is considered unacceptable.
brazos$area_32139 <- st_area(st_transform(brazos, 32139))
brazos$diff <- brazos$ALAND + brazos$AWATER - as.numeric(brazos$area_32139)
sum(brazos$diff)
# 298551.4

# sum(brazos$diff) / sum(brazos$ALAND + brazos$AWATER) * 100
# 0.0195

tm_shape(brazos) +
  tm_borders(col = "red", lwd = 2) +
  tm_crs(crs = 32139)

brazos$area_3083 <- st_area(st_transform(brazos, 3083))
brazos$diff <- brazos$ALAND + brazos$AWATER - as.numeric(brazos$area_3083)
sum(brazos$diff)
# -71.90936

brazos |> st_drop_geometry()

tmap_arrange(tm_shape(brazos) +
               tm_borders(col = "red", lwd = 2) +
               tm_crs(crs = 4269) +
               tm_graticules(col = "gray70", lwd = 0.8, lty = "dotted") +
               tm_title("Original"),
             
             tm_shape(brazos) +
               tm_borders(col = "red", lwd = 2) +
               tm_crs(crs = 32139) +
               tm_graticules(col = "gray70", lwd = 0.8, lty = "dotted") +
               tm_title("32139 for Shape"),
             
             tm_shape(brazos) +
               tm_borders(col = "red", lwd = 2) +
               tm_crs(crs = 3083) +
               tm_graticules(col = "gray70", lwd = 0.8, lty = "dotted") +
               tm_title("3083 for Area"),
             
             ncol = 3, sync = TRUE)

brazos <- texas %>% filter(NAME == "Brazos")

# two versions with their coordinate systems "baked in"
brazos_1 <- brazos["geometry"] %>% st_transform(4269) %>% st_set_crs(NA)
brazos_2 <- brazos["geometry"] %>% st_transform(32139) %>% st_set_crs(NA)

plot(brazos_1, border = "red", col = NA, main = "", axes = TRUE)
plot(brazos_2, border = "blue", col = NA, main = "", axes = TRUE)

normalize_geom <- function(g) {
  bb <- st_bbox(g)
  sx <- 1 / (bb["xmax"] - bb["xmin"])
  sy <- 1 / (bb["ymax"] - bb["ymin"])
  
  g2 <- g
  g2$geometry <- lapply(st_geometry(g), function(geom) {
    coords <- st_coordinates(geom)
    coords[,1] <- (coords[,1] - bb["xmin"]) * sx
    coords[,2] <- (coords[,2] - bb["ymin"]) * sy
    st_polygon(list(coords[,1:2]))
  })
  st_sf(geometry = st_sfc(g2$geometry))
}

b1n <- normalize_geom(brazos_1)
b2n <- normalize_geom(brazos_2)

plot(b1n, border="red", col=NA)
plot(b2n, border="blue", col=NA, add=TRUE)


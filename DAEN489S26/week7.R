library(sf)
library(terra)
library(dplyr)

srtm = rast(system.file("raster/srtm.tif", package = "spDataLarge"))
zion = read_sf(system.file("vector/zion.gpkg", package = "spDataLarge"))

zion = st_transform(zion, st_crs(srtm))

plot(srtm)

plot(zion[1])

srtm_cropped = crop(srtm, zion)

plot(srtm_cropped)

srtm_masked = mask(srtm, zion)

plot(srtm_masked)

srtm_cropped = crop(srtm, zion)

plot(srtm_cropped)

srtm_final = mask(srtm_cropped, zion)

plot(srtm_final)

srtm_inv_masked = mask(srtm, zion, inverse = TRUE)

plot(srtm_inv_masked)

data("zion_points", package = "spDataLarge")

elevation = terra::extract(srtm, zion_points)

plot(elevation)

zion_points = cbind(zion_points, elevation)

zion_transect = cbind(c(-113.2, -112.9), c(37.45, 37.2)) |>
  st_linestring() |> 
  st_sfc(crs = crs(srtm)) |>
  st_sf(geometry = _)

zion_transect$id = 1:nrow(zion_transect)

zion_transect = st_segmentize(zion_transect, dfMaxLength = 250)

zion_transect = st_cast(zion_transect, "POINT")

zion_transect = zion_transect |> 
  group_by(id) |> 
  mutate(dist = st_distance(geometry)[, 1]) 

zion_elev = terra::extract(srtm, zion_transect)

zion_transect = cbind(zion_transect, zion_elev)

zion_srtm_values = terra::extract(x = srtm, y = zion)

group_by(zion_srtm_values, ID) |> 
  summarize(across(srtm, list(min = min, mean = mean, max = max)))

#> # A tibble: 1 × 4
#>      ID srtm_min srtm_mean srtm_max
#>   <dbl>    <int>     <dbl>    <int>
#> 1     1     1122     1818.     2661

nlcd = rast(system.file("raster/nlcd.tif", package = "spDataLarge"))

zion2 = st_transform(zion, st_crs(nlcd))

zion_nlcd = terra::extract(nlcd, zion2)

zion_nlcd |> 
  group_by(ID, levels) |>
  count()

#> # A tibble: 7 × 3
#> # Groups:   ID, levels [7]
#>      ID levels         n
#>   <dbl> <fct>      <int>
#> 1     1 Developed   4205
#> 2     1 Barren     98285
#> 3     1 Forest    298299
#> 4     1 Shrubland 203701
#> # ℹ 3 more rows

cycle_hire_osm = spData::cycle_hire_osm

cycle_hire_osm_projected = st_transform(cycle_hire_osm, "EPSG:27700")

raster_template = rast(ext(cycle_hire_osm_projected), resolution = 1000,
                       crs = crs(cycle_hire_osm_projected))

ch_raster1 = rasterize(cycle_hire_osm_projected, raster_template)

ch_raster2 = rasterize(cycle_hire_osm_projected, raster_template, 
                       fun = "length")

ch_raster3 = rasterize(cycle_hire_osm_projected, raster_template, 
                       field = "capacity", fun = sum, na.rm = TRUE)

library(spData)

california = dplyr::filter(us_states, NAME == "California")

california_borders = st_cast(california, "MULTILINESTRING")

plot(california_borders[1])

raster_template2 = rast(ext(california), resolution = 0.5,
                        crs = st_crs(california)$wkt)

california_raster1 = rasterize(california_borders, raster_template2,
                               touches = TRUE)

plot(california_raster1)

elev = rast(system.file("raster/elev.tif", package = "spData"))

elev_point = as.points(elev) |> 
  st_as_sf()

plot(elev_point)

dem = rast(system.file("raster/dem.tif", package = "spDataLarge"))

cl = as.contour(dem) |> 
  st_as_sf()

plot(dem, axes = FALSE)
plot(cl, add = TRUE)

grain = rast(system.file("raster/grain.tif", package = "spData"))

grain_poly = as.polygons(grain) |> 
  st_as_sf()

plot(grain_poly)

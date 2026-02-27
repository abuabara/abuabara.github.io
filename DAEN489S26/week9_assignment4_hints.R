library(sf)
library(tigris)

brazos <- tracts(county = "Brazos", state = "TX", cb = FALSE, year = 2022)

library(terra)

ls2019day <- rast("/Users/abuabara/Downloads/GIS_Refs/Data/landscan-usa/landscan-usa-2019-conus-day.tif")
ls2020day <- rast("/Users/abuabara/Downloads/GIS_Refs/Data/landscan-usa/landscan-usa-2020-conus-day.tif")
ls2021day <- rast("/Users/abuabara/Downloads/GIS_Refs/Data/landscan-usa/landscan-usa-2021-conus-day.tif")

brazos_projected <- st_transform(brazos, st_crs(ls2019day))

ls2019day_cropped = crop(ls2019day, brazos_projected)
ls2020day_cropped = crop(ls2020day, brazos_projected)
ls2021day_cropped = crop(ls2021day, brazos_projected)

ls2019day_masked = mask(ls2019day_cropped, brazos_projected)
ls2020day_masked = mask(ls2020day_cropped, brazos_projected)
ls2021day_masked = mask(ls2021day_cropped, brazos_projected)

# plot(ls2019day)
# plot(ls2019day_cropped)
plot(ls2019day_masked)

c(ls2019day_masked, ls2020day_masked, ls2021day_masked)

diff = ls2021day_masked-ls2020day_masked

diff[diff == 0] <- NA
# diff <- reclassify(diff, cbind(0, NA))

plot(brazos_projected[1], col = NA, border = "black", lwd = .5)

r <- range(values(diff), na.rm = TRUE)
lim <- max(abs(r))

plot(
  diff,
  col = colorRampPalette(c("red", "white", "blue"))(50),
  add = TRUE
)
# plot(diff, add = TRUE)

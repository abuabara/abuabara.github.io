# ALEXANDER

library(dplyr)
library(janitor)
library(sf)
library(tmap)

filter <- dplyr::filter
rename <- dplyr::rename
select <- dplyr::select
summarise <- dplyr::summarise
`%nin%` = Negate(`%in%`)

# setwd("/Users/alexander/Downloads/Data")
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/TAMU/Teaching/2025:2026/2026 Spring/DAEN ISEN 489/Assignment3_sol")

# FLOODING
# https://floodmaps.fema.gov/NFHL/status.shtml
floodplain <- st_read("./48041C_20250122/S_FLD_HAZ_AR.shp")

glimpse(floodplain)

floodplain <-
  floodplain %>%
  filter(!st_is_empty(.)) %>%
  st_make_valid() %>%
  clean_names() %>%
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  select("fld_zone", "zone_subty")

glimpse(floodplain)

table(floodplain$fld_zone)

floodplain %>%
  st_drop_geometry() %>%
  count(fld_zone, zone_subty)

#   fld_zone                         zone_subty    n
# 1        A                               <NA>  138
# 2       AE                           FLOODWAY   47
# 3       AE                               <NA>  812
# 4        X 0.2 PCT ANNUAL CHANCE FLOOD HAZARD 1756
# 5        X            1 PCT FUTURE CONDITIONS   15
# 6        X       AREA OF MINIMAL FLOOD HAZARD  139

floodplain_class <-
  floodplain %>%
  mutate(
    fld_class = case_when(
      fld_zone %in% c("A", "AE") ~ "100yr",
      fld_zone == "X" & zone_subty == "0.2 PCT ANNUAL CHANCE FLOOD HAZARD" ~ "500yr",
      TRUE ~ "Out"
    )
  ) %>% group_by(fld_class) %>%
  summarise() %>%
  filter(fld_class != "Out")

# 14795264 / 26431976
# 0.5597487

# qtm(floodplain_class, fill = "fld_class")

# st_write(floodplain_class, "inclass_floodplain_class.shp")

# floodplain_class_simp <-
# smoothr::smooth(floodplain_class, method = "spline")

# object.size(floodplain_class_simp)

# 72845288/14795264
# 4.923554

# PARCELS
# https://brazoscad.org/tax-information/gis/
parcels <- st_read("./Public_Parcel_Boundary_certified_2025/Public_Parcel_Boundary_certified.shp")

glimpse(parcels)

parcels_inclass <-
  parcels %>%
  select("Imprv_Val", "state_cd") %>%
  filter(!st_is_empty(.)) %>%
  st_make_valid() %>%
  clean_names() %>%
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))

parcels_inclass$state_cd2 <- substr(parcels_inclass$state_cd, 1, 1)

glimpse(parcels_inclass)

table(parcels_inclass$state_cd2, useNA = "always") %>% addmargins()
#     A     B     C     D     E     F     J     M  <NA>   Sum 
# 55222  3270  6125  2408  4743  3702    49     3   130 75652

parcels_inclass <- st_transform(parcels_inclass, crs = st_crs(floodplain_class))

parcels_inclass_centroid <- st_point_on_surface(parcels_inclass)

# join_test <- st_join(parcels_inclass, floodplain_class)

# ANALYSIS
parcels_floodplain_join <- st_join(parcels_inclass_centroid, floodplain_class)

st_within(parcels_inclass_centroid, floodplain_class)

table(parcels_floodplain_join$fld_class,
      useNA = "always") %>% addmargins()
# 100yr 500yr  <NA>   Sum 
#  2495   451 72706 75652

table(parcels_floodplain_join$fld_class,
      parcels_floodplain_join$state_cd2,
      useNA = "always") %>% addmargins()
#     A     B     C     D     E     F     J     M  <NA>   Sum
# 100yr   848    72   442   518   491   112     4     0     8  2495
# 500yr   336    23    54     1    10    24     0     0     3   451
# <NA>  54038  3175  5629  1889  4242  3566    45     3   119 72706
# Sum   55222  3270  6125  2408  4743  3702    49     3   130 75652

# save.image("~/Library/Mobile Documents/com~apple~CloudDocs/TAMU/Teaching/2025:2026/2026 Spring/DAEN ISEN 489/Assignment3_sol/Inclass3.RData")

# ...
# ALTERNATIVELLY ...
floodplain_100yr <- floodplain_class |>
  filter(fld_class == "100Yr")

parcels_A <- parcels_inclass_centroid |>
  filter(state_cd_2 == "A") |>
  st_transform(st_crs(floodplain_class))

# DON'T DO, VERY SLOW
# sf_use_s2(FALSE)
# As_and_100yr = st_intersection(parcels_A, floodplain_100yr)

# POINT-POLYGON

# st_intersects is typically used for point-in-polygon
# st_intersects(parcels_A, floodplain_100yr)

# st_within returns TRUE for points inside, FALSE for points on the boundary
# st_within(parcels_A, floodplain_100yr)

# NOT POINT-POLYGON
# st_touches will be FALSE
# st_touches(parcels_A, floodplain_100yr)
# ...
# st_overlaps(parcels_A, floodplain_100yr) 
# st_contains(parcels_A, floodplain_100yr)

vector <- lengths(st_within(parcels_A, floodplain_100yr)) > 0

table(vector)

parcels_touching <- parcels_A[vector, ]

# OR
# As_and_100yr = st_intersection(filter(parcels_point, state_cd_2 == "A"),
#                                st_transform(filter(floodplain_class_simp, fld_class == "100Yr"), st_crs(parcels_point)))

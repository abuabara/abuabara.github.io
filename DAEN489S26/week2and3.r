library(sf)

methods(class = "sf")

p1 <- st_point(c(7.35, 52.42))
p2 <- st_point(c(7.22, 52.18))
p3 <- st_point(c(7.44, 52.19))

sfc <- st_sfc(list(p1, p2, p3), crs = 'OGC:CRS84')

st_sf(elev = c(33.2, 52.1, 81.2), 
      marker = c("Id01", "Id02", "Id03"), geom = sfc)

plot(sfc)

us_counties <- st_read("/Users/abuabara/Downloads/DAEN489_data/tl_2025_us_county/tl_2025_us_county.shp")

class(us_counties)

dim(us_counties)

glimpse(us_counties)

library(dplyr) # tidyverse

glimpse(us_counties)

filter(us_counties, STATEFP == 48)

tx_counties <- filter(us_counties, STATEFP == 48)

# plot(tx_counties)

# plot(st_geometry(tx_counties))

library(ggplot2)

ggplot() + 
  geom_sf(data = tx_counties, aes(fill = AWATER)) + 
  scale_y_continuous(breaks = 34:36)

library(tmap)

tm_shape(tx_counties) +
  tm_polygons("AWATER", 
              palette = "blues",
              title = "Area of Water")

tm_shape(tx_counties) +
  tm_polygons(fill = "AWATER",
              fill.scale = tm_scale_continuous(
                values = "brewer.blues"),
              fill.legend = tm_legend(
                title = "Area of Water"))

library(tmap)
qtm(tx_counties)

tmap_mode("view")
tm_shape(tx_counties) + tm_polygons("AWATER", palette = sf.colors(5), alpha = .5)

tx_counties[3:4, ]       # subset rows by position
tx_counties[, 3:4]       # subset columns by position
tx_counties[, c(1,4,7)]  # subset columns by position
tx_counties[4:6, 3:6]    # subset rows and columns by position
tx_counties[, c("GEOID", "NAME")] # columns by name
tx_counties[, c(F, T)]   # by logical indices
tx_counties[, 888]       # an index representing a non-existent column

glimpse(tx_counties)

tx_counties$AREA = tx_counties$ALAND + tx_counties$AWATER

st_crs(tx_counties)

tx_counties$AREA_km2 = tx_counties$AREA / 1e+6

tx_counties = mutate(tx_counties, Area2 = (ALAND + AWATER) / 1e+6)  

i_small = tx_counties$AREA_km2 < 1000

summary(i_small)

i_small = tx_counties$AREA_km2 < 1000

summary(i_small) # a logical vector

small_tx_counties = tx_counties[i_small, ]

summary(tx_counties$AREA_km2)

small_tx_counties = tx_counties[tx_counties$AREA_km2 < 1000, ]

small_tx_counties = subset(tx_counties, AREA_km2 < 1000, )

small_tx_counties = filter(tx_counties, AREA_km2 < 1000)

filter(us_counties, STATEFP == 48)

small_tx_counties = us_counties |>
  filter(STATEFP == 48) |>
  mutate(AreaKm2 = (ALAND + AWATER) / 1e+6) |>
  select(GEOID, Name = NAME, AreaKm2) |>
  # slice(1:5)
  filter(AreaKm2 < 1000)

small_tx_counties <-
  slice(
    select(
      filter(us_counties, STATEFP == 48),
      Name = NAME, GEOID),
    1:5)

tx_counties_filtered <- filter(us_counties, STATEFP == 48)

tx_counties_areaed <- mutate(tx_counties_filtered, AreaKm2 = (ALAND + AWATER) / 1e+6)

tx_counties_selected <- select(tx_counties_areaed, Name = NAME, GEOID, AreaKm2)

small_tx_counties <- filter(tx_counties_selected, AreaKm2 < 1000)

small_tx_counties <-
  slice(
    select(
      filter(us_counties, STATEFP == 48),
      Name = NAME, GEOID),
    1:5)

###########

library(sf)
library(spData)
library(dplyr)

glimpse(world)

world_agg1 = aggregate(pop ~ continent,
                       FUN = sum,
                       data = world,
                       na.rm = TRUE)

world_agg1

world_agg2 = aggregate(world["pop"],
                       by = list(world$continent),
                       FUN = sum,
                       na.rm = TRUE)

world_agg2

world_agg5 = world |>
  # st_drop_geometry() |>
  select(pop, continent, area_km2) |>
  group_by(Continent = continent) |>
  summarize(
    Pop  = sum(pop, na.rm = TRUE),
    Area = sum(area_km2),
    N    = n()
  ) |>
  mutate(Density = round(Pop / Area)) |>
  slice_max(Pop, n = 3) |>
  arrange(desc(N))

world_agg5


world_agg4 = world |>
  group_by(continent) |>
  summarize(
    Pop  = sum(pop, na.rm = TRUE),
    Area = sum(area_km2),
    N    = n()
  )

library(tmap)
qtm(world_agg4)

tm_shape(world_agg4) +
  tm_polygons("Pop", style = "cont")

tm_shape(world_agg4) +
  tm_polygons("Pop",
              style   = "cont",
              palette = "viridis",
              title   = "Population by Continent",
              alpha   = 0.9,
              lwd     = 0.5,
              border.col = "white") +
  tm_legend(position = c("RIGHT", "bottom"),
            orientation = "horizontal") +
  tm_layout(frame = FALSE,
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.outside.size = 0.25,
            legend.title.size = 1,
            legend.text.size  = 0.8)

tm_shape(world_agg4) +
  tm_polygons("Pop",
              style   = "cont",
              palette = "viridis",
              title   = "Population",
              colorNA = "red") +
  tm_layout(
    frame = FALSE,
    legend.outside = TRUE,
    legend.outside.position = "bottom",
    legend.outside.size = 0.15,
    legend = FALSE
  )

tm_shape(world_agg4) +
  tm_polygons("Pop",
              style   = "cont",
              palette = "viridis",
              title   = "Population",
              # colorNA = "red",
              textNA  = "",
              legend.show = TRUE,
              legend.frame = FALSE) +
  tm_layout(
    frame = FALSE,
    legend.outside = TRUE,
    legend.outside.position = "bottom",
    legend.outside.size = 0.15,
    legend.bg.color = NA,
    legend.frame = FALSE
  )

tm_shape(world_agg4) +
  tm_polygons("Pop",
              style   = "cont",
              palette = "viridis",
              title   = "Population",
              textNA  = "",
              legend.show = TRUE,
              legend.reverse = TRUE) +
  tm_layout(
    frame = FALSE,
    legend.outside = TRUE,
    legend.outside.position = "bottom",
    legend.outside.size = 0.15,
    legend.frame.lwd = 0
  )

tm_shape(world_agg4) +
  tm_polygons("Pop",
              style   = "cont",
              palette = "viridis",
              title   = "Population",
  ) +
  tm_legend(position = c("right", "bottom"),
            orientation = "horizontal") +
  tm_layout(frame = FALSE,
            legend.outside = TRUE,
            outer.margins = c(0, 0, 0, 0.2))

us_counties_df <- st_drop_geometry(us_counties)

class(us_counties_df)

dim(us_counties_df)

ncol(us_counties_df)

nrow(us_counties_df)

glimpse(us_counties_df)

library(dplyr) # tidyverse

glimpse(us_counties_df)

plot(st_geometry(us_counties))

plot(us_counties$STATEFP)

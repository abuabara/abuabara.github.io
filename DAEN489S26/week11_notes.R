library(tigris)
library(ggplot2)

options(tigris_use_cache = TRUE)

us_states <- states(cb = TRUE, resolution = "20m") 

us_states_shifted <- shift_geometry(us_states)

ggplot(data = us_states_shifted) +
  geom_sf(fill = "whitesmoke", color = "darkgray") +
  theme_void() +
  labs(title = "U.S. State Basemap")

###############

library(tigris)
library(ggplot2)
library(sf)
library(dplyr)

options(tigris_use_cache = TRUE)

county_bound <- counties(state = "TX", cb = TRUE) %>% 
  filter(NAME == "Travis")

county_water <- area_water(state = "TX", county = "Travis")

county_roads <- primary_secondary_roads(state = "TX") %>%
  st_intersection(county_bound)

ggplot() +
  geom_sf(data = county_bound, fill = "#fdfdfd", color = "black", size = 0.5) +
  geom_sf(data = county_water, fill = "#a2d2ff", color = "transparent") +
  geom_sf(data = county_roads, color = "#6c757d", size = 0.2) +
  theme_void() + 
  labs(
    title = "Basemap of Travis County, TX",
    subtitle = "Including Waterways and Primary/Secondary Roads",
    caption = "Source: US Census Bureau"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA)
  )

library(ggspatial)

ggplot() +
  geom_sf(data = county_bound, fill = "wheat", color = "black", size = 0.5) +
  geom_sf(data = county_water, fill = "#a2d2ff", color = "transparent") +
  geom_sf(data = county_roads, color = "#6c757d", size = 0.2) +
  annotation_scale(
    location = "bl",
    width_hint = 0.4,
    style = "ticks"
  ) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(0.2, "in"), 
    pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering()
  ) +
  theme_void() + 
  labs(
    title = "Basemap of Travis County, TX",
    caption = "Caption: Scale and orientation provided by ggspatial"
  )

###############

large_water <- county_water %>%
  mutate(area = as.numeric(st_area(.))) %>%
  filter(area > 1000000)

road_labels <- county_roads %>%
  filter(!is.na(FULLNAME)) %>%
  group_by(FULLNAME) %>%
  summarize() %>%
  st_centroid() %>%
  ungroup() %>%
  slice_sample(n = 10)

road_labels <- county_roads %>%
  filter(!is.na(FULLNAME)) %>%
  group_by(FULLNAME) %>%
  summarize() %>%
  mutate(road_length = st_length(geometry)) %>% 
  arrange(desc(road_length)) %>%
  slice_head(n = 7) %>%
  st_point_on_surface()

###############

library(ggrepel)

ggplot() +
  geom_sf(data = county_bound, fill = "#fdfdfd", color = "black") +
  geom_sf(data = county_water, fill = "#a2d2ff", color = "transparent") +
  geom_sf(data = county_roads, color = "#6c757d", size = 0.2) +
  geom_sf_text(data = large_water, aes(label = FULLNAME), 
               color = "#0077b6", fontface = "italic", size = 3, check_overlap = TRUE) +
  geom_sf_text(data = road_labels, aes(label = FULLNAME), 
               color = "black", size = 2.5, check_overlap = TRUE) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr", style = north_arrow_minimal()) +
  theme_void()

###############

library(sf)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)

library(tmap)
library(leaflet)
library(ggplot2)

nz_elev = rast(system.file("raster/nz_elev.tif", package = "spDataLarge"))

tm_shape(nz) +
  tm_fill() 
tm_shape(nz) +
  tm_borders() 
tm_shape(nz) +
  tm_fill() +
  tm_borders() 

###############

library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)
library(viridis)

polys <- st_sfc(
  st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol=2, byrow=TRUE))), # Bottom Left
  st_polygon(list(matrix(c(1,0, 2,0, 2,1, 1,1, 1,0), ncol=2, byrow=TRUE))), # Bottom Right
  st_polygon(list(matrix(c(0,1, 2,1, 2,2, 0,2, 0,1), ncol=2, byrow=TRUE)))  # Top (Large)
)

df <- data.frame(
  name = c("Rural County", "Suburban County", "Urban County"),
  total_incidents = c(50, 100, 500),
  population = c(5000, 50000, 1000000),
  incident_rate = c(10.0, 2.0, 0.5)
)

comparison_data <- st_sf(df, geometry = polys)

comparison_data <- comparison_data %>%
  mutate(
    centroid = st_centroid(geometry),
    lon = st_coordinates(centroid)[,1],
    lat = st_coordinates(centroid)[,2]
  )

map_count <- ggplot(comparison_data) +
  geom_sf(aes(fill = total_incidents), color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "rocket", begin = 0.3, end = 1) +
  geom_text(aes(x = lon, y = lat, label = name), 
            fontface = "bold", nudge_y = 0.2, check_overlap = TRUE) +
  geom_text(aes(x = lon, y = lat, label = paste("Count:", total_incidents)), 
            size = 3.5, nudge_y = -0.1) +
  geom_text(aes(x = lon, y = lat, label = paste("Pop:", format(population, big.mark=","))), 
            size = 3.5, nudge_y = -0.3) +
  theme_void() +
  labs(
    title = "Map 1: Wrongful Showing (Raw Count)",
    subtitle = "Darker colors focus only on magnitude, ignoring population size.",
    fill = "# of\nIncidents"
  ) +
  theme(
    plot.title = element_text(color = "red", face = "bold", size = 14),
    legend.position = "right",
    plot.margin = margin(10,10,10,10)
  )

map_rate <- ggplot(comparison_data) +
  geom_sf(aes(fill = incident_rate), color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "mako", direction = -1) +
  geom_text(aes(x = lon, y = lat, label = name), 
            fontface = "bold", nudge_y = 0.2, check_overlap = TRUE) +
  geom_text(aes(x = lon, y = lat, label = paste("Rate:", incident_rate)), 
            size = 3.5, nudge_y = -0.1, color = "white") +
  theme_void() +
  labs(
    title = "Map 2: Correct Showing (Normalized Rate)",
    subtitle = "Darker colors focus on intensity, revealing the true hotspot.",
    fill = "Rate per\n1,000 Pop."
  ) +
  theme(
    plot.title = element_text(color = "green4", face = "bold", size = 14),
    legend.position = "right",
    plot.margin = margin(10,10,10,10)
  )

comparison_plot <- map_count / map_rate
comparison_plot <- map_count + map_rate

comparison_plot

###############

library(shiny)
library(leaflet)
library(spData)
ui = fluidPage(
  sliderInput(inputId = "life", "Life expectancy", 49, 84, value = 80),
  leafletOutput(outputId = "map")
)
server = function(input, output) {
  output$map = renderLeaflet({
    leaflet() |> 
      addProviderTiles("OpenStreetMap") |>
      addPolygons(data = world[world$lifeExp < input$life, ])})
}
shinyApp(ui, server)

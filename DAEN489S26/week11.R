# 1
# Packages for reading data and data carpentry
library(readr)
library(dplyr)

# Packages for handling spatial data and for geospatial carpentry
library(sf)
library(ggspatial)
library(rnaturalearth)

# Packages for mapping and visualisation
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(cowplot)

# https://github.com/maczokni/crime_mapping/tree/master/data

setwd("~/GitHub/abuabara.github.io/DAEN489S26")

# 2
# read in geojson polygon for Hungary
hungary <- st_read("w11_data/hungary.geojson")

#read in drink driving data 
drink_driving <- read_csv("w11_data/drink_driving.csv")

#join the csv (attribute) data to the polygons
hu_dd <- left_join(hungary, drink_driving, by = c("name" = "name"))

# 3
library(ggplot2)

map <- ggplot(data = hu_dd) +             # specify data to use
  geom_sf(aes(fill = total_breath_tests)) # specify aestetics 

map

# 4
map <- map + 
  theme_void()# remove grid 

map

# 5
ggplot(data = hu_dd) + 
  geom_sf(aes(fill = total_breath_tests), lwd = 0) + # specify line width
  theme_void()

# 6
ggplot(data = hu_dd) + 
  geom_sf(aes(fill = total_breath_tests), lwd = 0.5, 
          col = "white") + # specify border colour
  theme_void()

# 7
# create new variable for quantiles
hu_dd <- hu_dd %>% 
  mutate(total_quantiles = cut(total_breath_tests, 
                               breaks = round(quantile(total_breath_tests),0),
                               include.lowest = TRUE, dig.lab=10))

# plot this new variable
ggplot(data = hu_dd) + 
  geom_sf(aes(fill = total_quantiles), lwd = 0.5, col = "white") + 
  theme_void()

# 8
ggplot(data = hu_dd) + 
  geom_sf() + 
  geom_sf(data = st_centroid(hu_dd),        # get centroids
          aes(size = total_breath_tests)) + # variable for size
  theme_void()

# 9
ggplot(data = hu_dd) + 
  geom_sf(fill = "light yellow", # specify polygon fill colour
          col = "white") +       # specify border colour
  geom_sf(data = st_centroid(hu_dd), 
          aes(size = total_breath_tests), 
          col = "orange") +      # specify symbol colour
  theme_void()

# 10
ggplot(data = hu_dd) + 
  geom_sf(fill = "light yellow", 
          col = "white") + 
  geom_sf(data = st_centroid(hu_dd), 
          aes(size = total_breath_tests), 
          col = "orange", 
          shape = 17) + # set shape to be a triangle
  theme_void()

# 11
hu_dd <- hu_dd %>% 
  mutate(pos_rate = round(positive_breath_tests/total_breath_tests*100,1))

# 12
ggplot(data = hu_dd) + 
  geom_sf(aes(fill = pos_rate), lwd = 0.5, col = "white") + 
  theme_void()

# 13
library(RColorBrewer)

display.brewer.all()

display.brewer.pal(n = 5, "Spectral")

display.brewer.all(colorblindFriendly = TRUE)

# 14
ggplot(data = hu_dd) + 
  geom_sf(aes(fill = total_quantiles), 
          lwd = 0.5, 
          col = "white") + 
  scale_fill_brewer(type = "seq",         # pick pallette type
                    palette = "YlOrRd") + # specify pallette by name
  theme_void()

# 15
ggplot(data = hu_dd) +
  geom_sf(aes(fill = total_quantiles), lwd = 0.5, col = "white") + 
  scale_fill_grey() + # use greyscale colour scheme for fill
  theme_void()

# 16
ggplot(data = hu_dd) + 
  geom_sf(aes(fill = total_quantiles), lwd = 0.5, col = "white") + 
  scale_fill_brewer(type = "seq", palette = "Greens") + 
  theme_void()

# 17
map <- ggplot(data = hu_dd) + 
  geom_sf(aes(fill = total_quantiles), 
          lwd = 0.5, col = "white") + 
  scale_fill_brewer(type = "seq", palette = "Greens") + 
  theme_void()

# 18
map <- map + 
  # specify both title and subtitle: 
  ggtitle(label = "Number of breathalyser tests per county in Hungary", 
          subtitle = "January 2020")

map

# 19
map <- map + 
  scale_fill_brewer(type = "seq", palette = "Greens", 
                    name = "Total tests (quantiles)") # desired legend title

map

# 20
# create object new_levels with desired labels
new_levels <- gsub(","," - ",levels(hu_dd$total_quantiles))

# 21
map <- map + 
  scale_fill_brewer(type = "seq", palette = "Greens", 
                    name = "Total tests (quantiles)", 
                    labels = new_levels) # specify our new labels

map

# 22
new_levels <- c("< 4796", "4796 to < 10785", "10785to < 15070", "> 15070")

# 23
map +
  scale_fill_brewer(type = "seq", palette = "Greens", 
                    name = "Total tests (quantiles)", 
                    labels = new_levels) # again specify labels object

# 24
map+ 
  geom_sf_label(aes(label = name)) # add layer of labels from the name column

# 25
library(ggrepel)

map + 
  geom_label_repel(data = hu_dd,             # add repel layer, specify dataframe
                   aes(label = name,         # specify where to find label (name column)
                       geometry = geometry), # specify geometry
                   stat = "sf_coordinates",  # transformation to use on the data
                   min.segment.length = 0)   # don't draw segments shorter than this

# 26
# create new dataframe with only top counties
labs_df <- hu_dd %>% filter(total_breath_tests >= 15070)

# add to map
map +
  geom_sf_label(data = labs_df, # specify to use the labels df
                aes(label = name),
                # fill = "white",
                fill = NA,
                label.size = NA
  )

# 27
# create new labels dataframe
labs_df <- hu_dd %>% filter(name == "Budapest")

map +
  geom_sf_label(data = labs_df, aes(label = name), fill = NA, label.size = NA, # label from name column
                nudge_y = 0.9,  # move label on y axis
                nudge_x = -0.1) # move label on x axis

# 28
# get x coordinate
bp_x <- labs_df %>% 
  mutate(cent_lng = st_coordinates(st_centroid(.))[,1]) %>% pull(cent_lng)

# get y coordinate
bp_y <- labs_df %>% 
  mutate(cent_lat = st_coordinates(st_centroid(.))[,2]) %>% pull(cent_lat)

# 29
map <-
  map + 
  geom_curve(x = bp_x - 0.1,  # starting x coordinate (the label)
             y = bp_y + 0.85, # starting y coordinate (the label)
             xend = bp_x ,    # ending x coordinate (BP centroid)
             yend = bp_y,     # ending y coordinate (BP centroid)
             arrow = arrow(length = unit(2, "mm"))) +
  geom_sf_label(data = labs_df, 
                aes(label = name),
                fill = NA,
                label.size = NA,
                nudge_y = 0.9, 
                nudge_x = -0.1)

map

# 30
caption_text <- paste("Map created by Solymosi", 
                      "Contains data from Police Hungary",
                      "http://www.police.hu/hu/a-rendorsegrol/",
                      "statisztikak/kozrendvedelem", 
                      "Map data copyrighted OpenStreetMap contributors", 
                      "available from https://www.openstreetmap.org", 
                      sep = "\n")

map <- map + labs(caption = caption_text) # include production notes here

map

# 31
library(ggspatial)

map + annotation_north_arrow(height = unit(7, "mm"), # specify arrow height
                             width = unit(5, "mm"))  # specify arrow width

# 32
map <- map + annotation_north_arrow(height = unit(7, "mm"),        # specify arrow height
                                    width = unit(5, "mm"), 
                                    style = north_arrow_minimal()) # specify arrow style

map

# 33
map <- map +annotation_scale(line_width = 0.5,       # add scale and specify width
                             height = unit(1, "mm"), # specify height
                             pad_x = unit(6, "cm"))  # adjust on x axis

map

# 34
ggplot(data = hu_dd) + 
  # annotation_map_tile() + # add basemap layer first
  annotation_map_tile(zoom = 8) +
  geom_sf(aes(fill = total_quantiles), lwd = 0.5, col = "white") + 
  scale_fill_brewer(type = "seq", palette = "Greens", name = "Total tests")

# 35
library(rnaturalearth)

europe_countries <- st_as_sf(countries110) %>%       # get geom for all countries
  filter(REGION_UN=="Europe" &                       # select Europe
           NAME != "Russia" & NAME != "Iceland") %>% # remove Russia and Iceland
  pull(NAME)                                         # get only the names in a list

europe <- ne_countries(geounit = europe_countries, # get geoms for countries in list
                       type = 'map_units',         # country type as map_units
                       returnclass = "sf")         # return sf object (not sp)

# 36
inset_map <- ggplot() +                                # create new ggplot 
  geom_sf(data = europe,                               # add europe map as first layer
          fill = "white") +                            # white fill 
  geom_sf(data = europe %>% filter(name == "Hungary"), # new layer only Hungary
          fill = "white" ,                             # white fill
          col = "red",                                 # make the border red
          lwd = 1) +                                   # make border line thick
  theme_void() +                                       # strip grid elements
  theme(panel.border = element_rect(colour = "black",  # draw border around map
                                    fill=NA))

# 37
library(cowplot)

hu_dd_with_inset <- ggdraw() + # set layer
  draw_plot(map) +             # draw the main map
  draw_plot(inset_map,         # draw inset map
            x = 0.75,          # specify location on x axis
            y = 0,             # specify location on y axis
            width = 0.35,      # specify width
            height = 0.35)     # specify height

hu_dd_with_inset

# Using tmap
library(sf)
library(dplyr)
library(readr)
library(tmap)
# Make sure you have v4 loaded
# remotes::install_github("r-tmap/tmap")
library(rnaturalearth)
library(grid)

setwd("~/Desktop/Week11/")

# 1. Load and join data
hungary <- st_read("data/hungary.geojson")
drink_driving <- read_csv("data/drink_driving.csv")

hu_dd <- left_join(hungary, drink_driving, by = c("name" = "name")) %>%
  mutate(pos_rate = round(positive_breath_tests / total_breath_tests * 100, 1))

# 2. Main Choropleth Map (v3 syntax)
map_main <- tm_shape(hu_dd) +
  tm_polygons("total_breath_tests", 
              style = "quantile", 
              n = 4, 
              palette = "Greens", 
              border.col = "white", 
              lwd = 0.5,
              title = "Total tests (quantiles)",
              labels = c("< 4796", "4796 to < 10785", "10785 to < 15070", "> 15070")) +
  tm_layout(frame = FALSE,
            main.title = "Number of breathalyser tests per county in Hungary",
            main.title.size = 1,
            title = "January 2020",
            title.size = 0.8,
            legend.position = c("left", "bottom"))

# 3. Proportional Symbols
labs_df <- hu_dd %>% filter(name == "Budapest")

final_map <- map_main +
  tm_shape(labs_df) +
  tm_layout(legend.position = c(0.85, 0.45)) +
  tm_text("name", ymod = 1, xmod = -0.5) +
  tm_compass(type = "4star", position = c("right", "top"), size = 1.5) +
  tm_scale_bar(position = c("right", "bottom"), breaks = c(0,10,50,100)) +
  tm_credits(paste("Map created by Solymosi", 
                   "Contains data from Police Hungary", 
                   "Map data copyrighted OpenStreetMap contributors", sep = "\n"), 
             position = c("left", "bottom"), size = 0.7)

final_map

# 4. Inset Map
europe <- ne_countries(continent = "Europe", returnclass = "sf")

europe <- europe %>% filter(region_un=="Europe" & name != "Russia" & name != "Iceland")

europe_cropped <- st_crop(europe, xmin = -20, ymin = 34, xmax = 45, ymax = 73)

inset_map <- tm_shape(europe_cropped) +
  tm_polygons(col = "white", border.col = "lightgrey") +
  tm_shape(europe %>% filter(name == "Hungary")) +
  tm_polygons(col = "white", border.col = "red", lwd = 2) +
  tm_layout(frame = TRUE)

inset_map

tmap_mode("plot")

# 5. Final Output
final_map

print(inset_map, vp = viewport(x = 0.1, y = 0.75, width = 0.2, height = 0.2))

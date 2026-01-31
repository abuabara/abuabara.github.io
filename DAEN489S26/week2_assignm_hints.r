library(sf)

target_crs <- 4326  # WGS84, change as needed/appropriated

parcels <- list.files(
  path = "/Users/abuabara/Downloads/DAEN489_data/parcels",
  pattern = "\\.shp$",
  full.names = TRUE,
  recursive = TRUE
)

parcels_all <- do.call(
  rbind,
  lapply(
    parcels,
    function(f) {
      x <- st_read(f, quiet = TRUE)
      
      if (is.na(st_crs(x))) {
        st_crs(x) <- target_crs
      }
      
      x <- st_transform(x, target_crs)
      x$source_file <- basename(f)
      x
    }
  )
)

parcels_all$STAT_LAND_2 <- substr(parcels_all$STAT_LAND_, 1, 1)

library(dplyr)

agg_parcels <- parcels_all %>% 
  filter(STAT_LAND_ == "A") %>%
  st_set_geometry(NULL) %>% 
  group_by(COUNTY) %>% 
  summarise(
    GEOID = unique(FIPS),
    total_imp_value = sum(IMP_VALUE, na.rm = TRUE),
    parcel_count = n()
  )

library(tidyr)

parcels_all %>% 
  st_drop_geometry() %>% 
  filter(!is.na(IMP_VALUE) & IMP_VALUE > 0) %>%
  group_by(COUNTY, STAT_LAND_2) %>% 
  summarise(n = n(),
            IMP_VALUE = sum(IMP_VALUE, na.rm = TRUE),
            .groups = 'drop') %>% 
  arrange(desc(n)) %>%
  pivot_wider(names_from = STAT_LAND_2, 
              values_from = n, 
              values_fill = 0)

library(tidycensus)

census_api_key("YOUR API GOES HERE")

pop_data <- get_acs(geography = "county",
                    # county = "Brazos",
                    county = unique(agg_parcels$COUNTY),
                    output = "wide",
                    geometry = FALSE,
                    variables = c(pop = "B01001_001"),
                    state = "TX",
                    year = 2023)

agg_parcels %>%
  left_join(pop_data, by = "GEOID") %>%
  mutate(
    imp_value_per_capita = total_imp_value / popE
  )

library(tmap)

tm_shape(parcels_all) +
  tm_polygons(fill = "STAT_LAND_2", lwd = 0)

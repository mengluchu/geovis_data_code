
# Note: Learning and running this code is optional for this assignment. The R users are encouraged to also learn the online tools: Kepler and Rawgraph. However, it is strongly recommended to try this code out to further improve your R skill and see the power of R.  
# ------------------------------
# Lab 3 visualization for exporation
# ------------------------------

# Load necessary libraries
library(ggplot2)
library(readr)
library(dplyr)

#A. Time series data exploration
# Load dataset
data_ts <- read_csv("~/work/teaching_2025/Geovisualisation/data/DENL17_selected_hr.csv")

# Use Longitude to identify each monitoring station
# set color to "AirQualityStationType" to show air pollution in background, industrial, traffic
ggplot(data_ts, aes(x = hours, y = wkd_hr_value, group = Longitude, color = AirQualityStationType)) +
  geom_line(alpha = 0.6) +
  labs(title = "NO2 Levels Over Time by Station",
       x = "Time of Day (hour)",
       y = "NO2 (µg/m³)",
       color = "Country") +
  theme_minimal()

ggplot(data_ts, aes(x = hours, y = wkd_hr_value, group = Longitude, color = population_1000)) +
  geom_line(alpha = 0.6) +
  labs(title = "NO2 Levels Over Time by Station",
       x = "Time of Day (hour)",
       y = "NO2 (µg/m³)",
       color = "Country") +
  theme_minimal()

ggplot(data_ts, aes(x = hours, y = wkd_hr_value, group = Longitude)) +
  geom_line(aes(color = population_1000), alpha = 0.7) +
  scale_color_viridis_c() +
  facet_wrap(~ Countrycode) +
  labs(title = "NO2 Levels by Time of Day, Faceted by Country",
       x = "Hour of Day",
       y = "NO2 (µg/m³)",
       color = "Population (/1000)") +
  theme_minimal()

# ------------------------------# 
# B Spatial Data Exploration
# B1 Display data geographically. 
# ------------------------------

# Load spatial dataset
data_sp <- read_csv("~/work/teaching_2025/Geovisualisation/data/DENL17_selected_sp.csv")

# Bubble plot: NO2 by location, color by population
ggplot(data_sp, aes(x = Longitude, y = Latitude, size = wkd_day_value, color = population_1000)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c() +
  labs(title = "Daytime NO2 by Location", x = "Longitude", y = "Latitude") +
  theme_minimal()

# Alternative mappings
# Size = population, Color = NO2
ggplot(data_sp, aes(x = Longitude, y = Latitude, size = wkd_day_value, color = population_1000)) +
  geom_point(alpha = 0.7) +
  labs(title = "Population Size and NO2 Levels", x = "Longitude", y = "Latitude") +
  theme_minimal()

# Size = NO2, Color = primary road density
ggplot(data_sp, aes(x = Longitude, y = Latitude, size = wkd_day_value, color = road_class_2_300)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c() +
  labs(title = "NO2 and Road Density", x = "Longitude", y = "Latitude") +
  theme_minimal()

# ------------------------------
# B2 Voronoi Diagram (Bonus Exercise)
# ------------------------------

# Libraries for Voronoi
library(deldir)

library(sf)
library(tidyverse)

# Load the data
data_sp <- read_csv("~/work/teaching_2025/Geovisualisation/data/DENL17_selected_sp.csv")


# Create Voronoi tesselation using deldir
voronoi <- deldir(data_sp$Longitude, data_sp$Latitude)

# Convert to spatial polygons
tiles <- tile.list(voronoi)
polys <- lapply(tiles, function(tile) {
  coords <- cbind(tile$x, tile$y)
  coords <- rbind(coords, coords[1, ])  # close the polygon
  st_polygon(list(coords))
})

# Combine into an sf object
voronoi_sf <- st_sf(geometry = st_sfc(polys), data = data_sp)

# Plot Voronoi diagram, colored by country code
ggplot() +
  geom_sf(data = voronoi_sf, aes(fill = data.Countrycode), color = "black", size = 0.2) +
  labs(title = "Voronoi Diagram of Monitoring Stations",
       fill = "Country Code") +
  theme_minimal()

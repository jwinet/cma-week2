library("readr")
library("sf")
library("dplyr")
library("tmap")
library("ggplot2")

GPS_data <- read_delim("datasets/posmo_2026-03-05T00_00_00+01_00-2026-04-22T23_59_59+02_00.csv", ",") |> 
  st_as_sf(coords = c("lon_x","lat_y"), crs = 4326)

GPS_data <- st_transform(GPS_data, 2056)

ggplot(GPS_data) + 
  geom_sf() +
  theme(legend.position = "none") +
  coord_sf(datum = 2056)

tmap_mode("view")

tm_shape(GPS_data) +
  tm_dots()

st_layers("datasets/Test_20250513.gpx")

# Test swisstopo
swisstopo_Test <- st_read("datasets/Test_20250513.gpx", layer = "track_points")

ggplot(swisstopo_Test) + 
  geom_sf() +
  theme(legend.position = "none") +
  coord_sf(datum = 2056)

tmap_mode("view")

tm_shape(swisstopo_Test) +
  tm_dots()

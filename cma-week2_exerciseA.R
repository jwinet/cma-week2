library("readr")
library("sf")
library("dplyr")

## Task 1

wildschwein_BE <- read_delim("datasets/wildschwein_BE_2056.csv", ",")

wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056)

##Task 2

difftime_secs <- function(later, now){
  as.numeric(difftime(later, now, units = "secs"))
}

wildschwein_BE <- wildschwein_BE |> 
  group_by(TierID) |> 
  mutate(
    timelag = difftime_secs(lead(DatetimeUTC), DatetimeUTC)
  )

# Answers

n <- n_distinct(wildschwein_BE)
n # 51246 animales were tracked


summary <- wildschwein_BE |> 
  st_drop_geometry() |> 
  group_by(TierID) |> 
  summarise(
    start = min(DatetimeUTC),
    end = max(DatetimeUTC),
    duration = as.numeric(difftime(end, start, units = "days"))
  )
print(summary)
# 002A was tracked for 339 days.
# 016A was tracked for 235 days.
# 018A was tracked for 262 days.
# Tracking for 002A started first and then tracking for 016A and 018A started at the same time.
# Tracking ended for 016A and for 002A and 018A nearly a month later. 


sampling <- wildschwein_BE |>
  st_drop_geometry() |>
  group_by(TierID) |>
  summarise(
    median_interval_mins = median(timelag, na.rm = TRUE) / 60,
    mean_interval_mins = mean(timelag, na.rm = TRUE) / 60
  )

print(sampling)
# median temporal sampling interval
# 002A 15.0 min
# 016A 15.0 min
# 018A 15.1 min

## Task 3

distance_by_element <- function(later, now){
  as.numeric(
    st_distance(later, now, by_element = TRUE) # by_element must be set to TRUE
  )
}  

wildschwein_BE <- wildschwein_BE |> 
  group_by(TierID) |> 
  mutate(
    steplength = distance_by_element(lag(geometry), geometry)
  )

## Task 4

wildschwein_BE <- wildschwein_BE |> 
  group_by(TierID) |> 
  mutate(
    speed = steplength / timelag
  )

## Task 5

wildschwein_sample <- wildschwein_BE |>
  filter(TierName == "Sabi") |> 
  head(100)

library(tmap)
tmap_mode("view")

tm_shape(wildschwein_sample) + 
  tm_dots()

wildschwein_sample_line <- wildschwein_sample |> 
  # dissolve to a MULTIPOINT:
  summarise(do_union = FALSE) |> 
  st_cast("LINESTRING")

tmap_options(basemap.server = "OpenStreetMap")

tm_shape(wildschwein_sample_line) +
  tm_lines() +
  tm_shape(wildschwein_sample) + 
  tm_dots()

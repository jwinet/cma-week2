library("readr")
library("sf")
library("dplyr")
library("ggplot2")
library("tidyr")

difftime_secs <- function(x, y){
  as.numeric(difftime(x, y, units = "secs"))
}

distance_by_element <- function(later, now){
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}

caro <- read_delim("datasets/caro60.csv", ",") |>
  st_as_sf(coords = c("E","N"), crs = 2056) |> 
  select(DatetimeUTC)

## Task 1

caro <- caro |> 
  mutate(
    timelag = difftime_secs(lead(DatetimeUTC), lag(DatetimeUTC)),
    steplength = distance_by_element(lead(geometry), lag(geometry)),
    speed = steplength / timelag
  )
head(caro)

## Task 2

caro <- caro |> 
  mutate(
    timelag2 = difftime_secs(lead(DatetimeUTC, n = 2), lag(DatetimeUTC, n = 2)),
    steplength2 = distance_by_element(lead(geometry, n = 2), lag(geometry, n = 2)),
    speed2 = steplength2 / timelag2
  )

caro |> 
  # drop geometry and select only specific columns
  # to display relevant data only
  st_drop_geometry() |> 
  select(timelag2, steplength2, speed2) |> 
  head()

## Task 3

caro <- caro |> 
  mutate(
    timelag3 = difftime_secs(lead(DatetimeUTC, n = 4), lag(DatetimeUTC, n = 4)),
    steplength3 = distance_by_element(lead(geometry, n = 4), lag(geometry, n = 4)),
    speed3 = steplength3 / timelag3
  )

caro |> 
  st_drop_geometry() |> 
  select(timelag3, steplength3, speed3) |> 
  head()

## Task 4

caro |> 
  st_drop_geometry() |> 
  select(DatetimeUTC, speed, speed2, speed3)

# before pivoting, let's simplify our data.frame
caro2 <- caro |> 
  st_drop_geometry() |> 
  select(DatetimeUTC, speed, speed2, speed3)

caro_long <- caro2 |> 
  pivot_longer(c(speed, speed2, speed3))

head(caro_long)

ggplot(caro_long, aes(name, value)) +
  # we remove outliers to increase legibility, analogue
  # Laube and Purves (2011)
  geom_boxplot(outliers = FALSE)

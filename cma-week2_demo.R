
## Different tool for CMA

### Difftime

now <- as.POSIXct("2024-04-26 10:20:00")
later <- as.POSIXct("2024-04-26 11:35:00")

# with difftime, we can be specific about our units of output
# with as.numberic we turn the output into a numeric value
time_difference <- as.numeric(difftime(later, now, units = "secs"))

str(time_difference)
class(time_difference)

# creating a function that automates the steps above
difftime_secs <- function(spaeter, jetzt){
  as.numeric(difftime(spaeter, jetzt, units = "secs"))
}

# testing the function with our two objects
difftime_secs(later, now)

### offsets with lead and lag

library(dplyr)

numbers <- 1:10

# offset the numbers to the "left" # first n values will be discarded (default = 1)
lead(numbers, n = 2)

# offset the numbers to the "right"
lag(numbers, n = 5)


### Offset with dataframes

wildschwein <- tibble(       # aka data.frame
  TierID = rep(c("Hans", "Klara"), each = 5),
  DatetimeUTC = rep(as.POSIXct("2015-01-01 00:00:00", tz = "UTC") + 0:4 * 15 * 60, 2)
)

now <- wildschwein$DatetimeUTC
later <- lead(now) # lead(wildschwein$DatetimeUTC)

wildschwein$timelag <- difftime_secs(later, now)

wildschwein$timelag <- NULL
# Doing the sam esteps above (calculating timedifference between
# observations), but this time with mutate and grouping
wildschwein <- mutate(
  wildschwein, 
  timediff = difftime_secs(lead(DatetimeUTC), DatetimeUTC),
  .by = TierID # group by TierID
)

wildschwein <- wildschwein |> #Pipe with Ctrl + Shift + M
  group_by(TierID) |> 
  mutate(
    timediff = difftime_secs(lead(DatetimeUTC), DatetimeUTC)
  )

wildschwein |> 
  group_by(TierID) |> 
  summarise(
    mean = mean(timediff, na.rm = TRUE)
  )


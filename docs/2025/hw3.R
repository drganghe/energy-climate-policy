#Here is the code prepared by Gang He for HW3 to 
#First, average 5 minute actual load data to hourly load
#Second, combine forecast hourly load with averaged actual hourly load
#Third, extract month, day of month, hour of day, and day of week information from timestamp
#rm(list = ls())
# Load libraries
library(dplyr)
library(lubridate)

# Read the datasets
real_time_data <- read.csv("OASIS_Real_Time_Dispatch_Actual_Load.csv")
day_ahead_forecast <- read.csv("OASIS_Day_Ahead_Market_ISO_Load_Forecast.csv")

# Convert timestamp columns to POSIXct
real_time_data$timestamp <- as.POSIXct(strptime(real_time_data$`RTD.End.Time.Stamp`, "%Y/%m/%d %H:%M"), format="%m/%d/%y %H:%M")
day_ahead_forecast$timestamp <- as.POSIXct(day_ahead_forecast$`Eastern.Date.Hour`, format="%m/%d/%y %H:%M")

# Aggregate 5-minute data to hourly averages
hourly_actual_load <- real_time_data %>%
  # Round timestamps to the nearest hour
  mutate(hour_timestamp = floor_date(timestamp, unit = "hour")) %>%
  # Group by the hourly timestamp
  group_by(hour_timestamp) %>%
  # Calculate average actual load
  summarise(RTD_Actual_Load_Hourly = mean(`RTD.Actual.Load`, na.rm = TRUE)) %>%
  ungroup()


# Merge the datasets
combined_data <- left_join(
  day_ahead_forecast, 
  hourly_actual_load, 
  by = c("timestamp"= "hour_timestamp")
)


# Add columns show month, day of month, hour of day, and day of week
combined_data <- combined_data %>%
  mutate(
    Month = month(`timestamp`),
    Day_of_Month = day(`timestamp`),
    Hour_of_Day = hour(`timestamp`),
    Day_of_Week = wday(`timestamp`, label = TRUE),
    Day_of_Week_Numeric = wday(`timestamp`),
    # Add weekday/weekend column
    Is_Weekday = ifelse(Day_of_Week_Numeric %in% c(2,3,4,5,6), 1, 0)
  )


# Optional: Write the combined dataset to a new CSV
write.csv(combined_data, "Combined_Load_Forecast_Actual_Data.csv", row.names = FALSE)

# Print summary of the combined dataset
summary(combined_data)




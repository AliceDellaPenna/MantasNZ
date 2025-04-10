## PREP ========================================================================
# Load required libraries
library(suncalc); library(geosphere); library(dplyr); library(lubridate); library(readr)

#Set working directory
setwd("/Users/tamsin/Files/Manuscript/RDA_files") 
# Load in the data from 02 State Space Modelling output
load("horizontal02.RDA")

## WIND DATA ===================================================================
# Wind data is retrieved from the following Metservice stations:
# Great Mercury Is NZGMW - Latitude: 36.587°S, Longitude: 175.756°E, Station Height: 80 m
# Slipper Island NZSLW - Latitude: 37.049°S, Longitude: 175.944°E, Station Height: 103 m
# Tiritiri Light NZTTW - Latitude: 36.600°S, Longitude: 174.883°E, Station Height: 110 m
# Mokohinau Island NZMUX - Latitude: 35.903°S, Longitude: 175.115°E, Station Height: 69 m
# Channel Island NZCLW - Latitude: 36.422°S, Longitude: 175.331°E, Station Height: 80 m
# Waihau Bay NZUQF- Latitude: 37.619°S, Longitude: 177.906°E, Station Height: 20 m

# This data is collected in UTC - Important that data is also in UTC. 
# Import data frame including weather stations with lat lon coordinates
weather_stations <- read.csv("/Users/tamsin/Files/Manuscript/Data/Environmental/Wind/wind_stations.csv")

# Find closest weather station for each manta GPS point: -----------------------
# Function to calculate distance in meters between two GPS points
calculate_distance <- function(lat1, lon1, lat2, lon2) {
  distVincentyEllipsoid(c(lon1, lat1), c(lon2, lat2))
}

# Create an empty column to store the closest weather station for each GPS point
manta_df$Closest_Station <- NA

# Loop through each GPS point in the animal tracking data and assign closest station
for (i in 1:nrow(manta_df)) {
  # Calculate the distances to each weather station
  distances <- sapply(1:nrow(weather_stations), function(j) {
    calculate_distance(
      manta_df$lat[i],
      manta_df$lon[i],
      weather_stations$lat[j],
      weather_stations$lon[j]
    )
  })
  
  # Find the index of the closest weather station
  closest_index <- which.min(distances)
  
  # Assign the closest weather station to the corresponding row in the data frame
  manta_df$Closest_Station[i] <- weather_stations$Station[closest_index]
}

# Extract wind data from closest station: --------------------------------------
# Load in wind data for each weather station 
input_dir <- "/Users/tamsin/Files/Manuscript/Data/Environmental/Wind/"
NZGMW <- read.csv(file = paste0(input_dir, "wind_NZGMW.csv"))
NZSLW <- read.csv(file = paste0(input_dir, "wind_NZSLW.csv"))
NZCLW <- read.csv(file = paste0(input_dir, "wind_NZCLW.csv"))
NZMUX <- read.csv(file = paste0(input_dir, "wind_NZMUX.csv"))
NZTTW <- read.csv(file = paste0(input_dir, "wind_NZTTW.csv"))
NZUQF <- read.csv(file = paste0(input_dir, "wind_NZUQF.csv"))

NZGMW$datetime <- as.POSIXct(NZGMW$datetime, format = "%d/%m/%y %H:%M")
NZSLW$datetime <- as.POSIXct(NZSLW$datetime, format = "%d/%m/%y %H:%M")
NZTTW$datetime <- as.POSIXct(NZTTW$datetime, format = "%d/%m/%y %H:%M")
NZMUX$datetime <- as.POSIXct(NZMUX$datetime, format = "%d/%m/%y %H:%M")
NZCLW$datetime <- as.POSIXct(NZCLW$datetime, format = "%d/%m/%y %H:%M")
NZUQF$datetime <- as.POSIXct(NZUQF$datetime, format = "%d/%m/%y %H:%M")

# Filter manta_df data for each wind station individually
for (station_name in unique(manta_df$Closest_Station)) {
  # Get the wind data frame for the current weather station
  current_wind_data <- get(paste0(station_name))
  
  # Subset the manta_df data for the current weather station
  manta_subset <- manta_df[manta_df$Closest_Station == station_name, ]
  
  # Initialize empty vectors to store the extracted wind values
  wind_speed <- c()
  wind_direction <- c()
  max_hourly_gust <- c()
  wind_datetimes <- c()
  
  # Loop through each GPS point in the current manta_df subset
  for (i in 1:nrow(manta_subset)) {
    # Find the index of the datetime in the wind data frame
    match_index <- which(current_wind_data$datetime == manta_subset$datetime[i])
    
    # If there's a match, extract wind values, else fill with NA
    if (length(match_index) > 0) {
      wind_speed_value <- current_wind_data$speed[match_index]
      wind_direction_value <- current_wind_data$dir[match_index]
      max_hourly_gust_value <- current_wind_data$max_hourly_gust[match_index]
    } else {
      wind_speed_value <- NA
      wind_direction_value <- NA
      max_hourly_gust_value <- NA
    }
    
    # Append the values to the vectors
    wind_speed <- c(wind_speed, wind_speed_value)
    wind_direction <- c(wind_direction, wind_direction_value)
    max_hourly_gust <- c(max_hourly_gust, max_hourly_gust_value)
    wind_datetimes <- c(wind_datetimes, manta_subset$datetime[i])
  }
  
  # Convert wind_datetimes to the desired format with UTC timezone
  formatted_wind_datetimes <- as.POSIXct(wind_datetimes, origin = "1970-01-01", tz = "UTC")
  formatted_wind_datetimes <- format(formatted_wind_datetimes, format = "%Y-%m-%d %H:%M", tz = "UTC")
  
  # Add the extracted wind data back into the current manta_df subset
  manta_subset$wind_datetime <- formatted_wind_datetimes
  manta_subset$wind_speed <- wind_speed
  manta_subset$wind_direction <- wind_direction
  manta_subset$max_hourly_gust <- max_hourly_gust
  
  # Save the updated wind data frame to a new data frame with a distinct name
  assign(paste0("wind_", station_name, "_matched"), manta_subset)
}

# Combine all matched wind data frames into one
manta_df <- rbind(
  wind_NZGMW_matched,
  wind_NZSLW_matched,
  wind_NZTTW_matched,
  wind_NZMUX_matched,
  wind_NZCLW_matched,
  wind_NZUQF_matched
)

## TIDE DATA ===================================================================
# Tidal data is retrieved from tide "stations" using the NIWA Tide Forecaster application
# This data is collected in GMT/UTC - Important that data is also in UTC.

# Import data frame with tidal station coordinates
tidal_stations <- read.csv("/Users/tamsin/Files/Manuscript/Data/Environmental/Tide/Tide_Stations.csv")

# Calculate closest tide station for each manta GPS point
calculate_distance <- function(lat1, lon1, lat2, lon2) { # Calculate Distances
  distVincentyEllipsoid(c(lon1, lat1), c(lon2, lat2))
}

manta_df$Closest_Tide_Station <- sapply(1:nrow(manta_df), function(i) { # Find Closest Station
  distances <- sapply(1:nrow(tidal_stations), function(j) {
    calculate_distance(
      manta_df$lat[i], manta_df$lon[i],
      tidal_stations$lat[j], tidal_stations$lon[j]
    )
  })
  tidal_stations$Station[which.min(distances)]  # Assign station name
})

### Load in tide data:----------------------------------------------------------
# Define the directory where tide CSV files are located
input_dir <- "/Users/tamsin/Files/Manuscript/Data/Environmental/Tide/tide_compiled_csv/"

# Initialize an empty list to store the data frames
tide_data <- list()

# Loop through each file and read it into a data frame
for (i in 1:37) {
  # Read CSV file for each tide station
  tide_data[[i]] <- read.csv(file.path(input_dir, paste0("tide_", sprintf("%02d", i), ".csv")))
  
  # Convert datetime column to POSIXct
  tide_data[[i]]$datetime <- as.POSIXct(tide_data[[i]]$datetime, format = "%d/%m/%Y %H:%M", tz = "UTC")
  
  # Subset data to remove rows where value < 1 (high tide)
  tide_data[[i]] <- tide_data[[i]][tide_data[[i]]$value >= 1, ]
}

# Filter manta_df data for each tide station individually
for (station_num in 1:37) {  # Assuming you have 21 tidal stations
  # Get the data frame for the current tide station
  current_tide_data <- manta_df[manta_df$Closest_Tide_Station == station_num, ]
  
  # Initialize empty vectors to store the extracted tide values
  closest_tide_values <- c()
  closest_tide_datetimes <- c()
  time_to_hightide <- c()  # New vector to store time differences in hours
  
  # Loop through each GPS point in the current tide data
  for (i in 1:nrow(current_tide_data)) {
    # Get the closest tide datetime from the corresponding tide station data frame
    closest_datetime <- tide_data[[station_num]]$datetime[which.min(abs(difftime(current_tide_data$datetime[i], tide_data[[station_num]]$datetime, units = "secs")))]
    
    # Find the corresponding tide value for the closest datetime
    closest_tide_value <- tide_data[[station_num]]$value[tide_data[[station_num]]$datetime == closest_datetime]
    
    # Calculate the time difference in hours
    time_diff <- as.numeric(difftime(current_tide_data$datetime[i], closest_datetime, units = "hours"))
    
    # Append the values to the vectors
    closest_tide_values <- c(closest_tide_values, closest_tide_value)
    closest_tide_datetimes <- c(closest_tide_datetimes, closest_datetime)
    time_to_hightide <- c(time_to_hightide, time_diff)
  }
  
  # Convert closest_tide_datetimes to the desired format with UTC timezone
  formatted_tide_datetimes <- as.POSIXct(closest_tide_datetimes, origin = "1970-01-01", tz = "UTC")
  formatted_tide_datetimes <- format(formatted_tide_datetimes, format = "%Y-%m-%d %H:%M", tz = "UTC")
  
  # Add the extracted tide data and time difference back into the current tide data frame
  current_tide_data$closest_tide_datetime <- formatted_tide_datetimes
  current_tide_data$closest_tide_value <- closest_tide_values
  current_tide_data$time_to_hightide <- time_to_hightide
  
  # Save the updated tide data frame to a new data frame with a distinct name
  assign(paste0("tide_", station_num, "_matched"), current_tide_data)
}

# The updated data frames for each tidal station are now saved as tide_1_matched, tide_2_matched, etc.

# Combine all tide dataframes into one big dataframe
manta_df <- do.call(rbind, lapply(1:37, function(x) get(paste0("tide_", x, "_matched"))))

### Categorise tide: -----------------------------------------------------------
# Create a new column 'tide_category' to categorise data as either 'falling' 'rising' or 'slack' tide
manta_df$tide_category <- ifelse(manta_df$time_to_hightide <= -0.5, 'falling',
                                 ifelse(manta_df$time_to_hightide >= 0.5, 'rising', 'slack'))

## MOON PHASE ==================================================================
manta_df$date <- as.Date(manta_df$datetime, tz = "UTC")

# Use moonillumination function from suncalc package to extract fraction and phase
moonfraction <- getMoonIllumination(date = manta_df$datetime, keep = c("fraction"))
moonphase <- getMoonIllumination(date = manta_df$datetime, keep = c("phase"))

# Merge the new columns to manta_df
manta_df$fraction <- moonfraction$fraction
manta_df$phase <- moonphase$phase


## SUN =========================================================================

# Create a new dataframe with necessary columns
sun_df <- manta_df[, c("datetime", "lat", "lon")]

# Rename the "datetime" column as "date"as is required by the package
sun_df <- rename(sun_df, date = datetime)

# Use getSunlightPosition function from suncalc package to extract sun altitude
sun <- getSunlightPosition(data = sun_df, keep = c("altitude"))

# Convert altitude from radians to degrees
sun$altitude <- sun$altitude * (180 / pi)

# Merge altitude column from sun dataframe to manta_df based on matching values of date, lat, and lon
manta_df <- merge(manta_df, sun[, c("date", "lat", "lon", "altitude")], by.x = c("datetime", "lat", "lon"), by.y = c("date", "lat", "lon"), all.x = TRUE)



## CLEAN AND SAVE DATA  ========================================================
setwd("/Users/tamsin/Files/Manuscript/RDA_Files/") #Set working directory to save RDA file
save(manta_df, file = "horizontal03.RDA")


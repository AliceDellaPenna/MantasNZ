## PREP ========================================================================
# Load in packages
library(dplyr); library(lubridate); library(geosphere)

## CLEAN DATA ==================================================================
# Load in data + 
# Remove unnecessary columns + 
# Filter based on residual and time error, and apply speed filter (5ms-1):

# List all files in the directory
file_list <- list.files("All_Fastloc_GPS")

# Initialize the 'removed' dataframe
removed <- data.frame()

# Initialize a list to store processed data frames
cleaned_data <- list()

# Loop through each file in the list
for(file in file_list) {
  # Construct the full file path
  file_path <- file.path("All_Fastloc_GPS", file)
  # Read the data frame from the file
  df <- read.csv(file_path) 
  
  # Remove additional columns
  columns_to_keep <- c("Name", "Day", "Time", "Latitude", "Longitude", "Residual", "Time.Error")
  df <- df[, columns_to_keep]
  # Rename columns
  df <- df %>%
    rename(date = Day, id = Name, time = Time, lat = Latitude, lon = Longitude)
  # Convert date to class = Date
  df$date <- as.Date(df$date, format = "%d-%b-%Y", tz = "UTC")
  # Create datetime column
  df <- df %>%
    mutate(datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  
  # Filter rows based on conditions
  df <- df %>%
    filter(Residual <= 30, Time.Error >= -5, Time.Error <= 5)
  # Remove Residual and Time.Error columns
  df <- df %>%
    dplyr::select(-Residual, -Time.Error)
  # Calculate distance between points
  df <- df %>%
    mutate(distance = distVincentySphere(cbind(lon, lat), lag(cbind(lon, lat))))
  # Calculate time difference in seconds
  df <- df %>%
    mutate(time_diff = as.numeric(difftime(datetime, lag(datetime), units = "secs")))
  # Calculate speed
  df <- df %>%
    mutate(speed = distance / time_diff)
  # Store removed rows for evaluation
  removed <- bind_rows(removed, df %>%
                         filter(!is.na(speed) & speed >= 5))
  # Filter rows based on speed
  df <- df %>%
    filter(is.na(speed) | speed < 5)
  # Store processed data frame
  cleaned_data[[file]] <- df
}

# Rename data frames
# Iterate over each processed dataframe in the list
for(file in names(cleaned_data)) {
  # Extract the first six digits from the file name
  new_name <- paste0("df_", substr(file, 1, 6))
  
  # Rename the dataframe
  cleaned_data[[new_name]] <- cleaned_data[[file]]
  cleaned_data[[file]] <- NULL
}

# Move to main environment
df_197235 <- cleaned_data$"df_197235"
df_204511 <- cleaned_data$"df_204511"
df_238016 <- cleaned_data$"df_238016"
df_238018 <- cleaned_data$"df_238018"
df_238019 <- cleaned_data$"df_238019"
df_252520 <- cleaned_data$"df_252520"
df_252525 <- cleaned_data$"df_252525"
df_252778 <- cleaned_data$"df_252778"

## SUBSET TO DATA OF INTEREST ==================================================
# Remove data following detachment, or exit from study boundaries (visually identified)

# Select cutoff points 
# (Based on detatch time / exit from study boundaries visually identified in QGIS:)
cutoff_204511 <- as.POSIXct("2022-02-07 08:07:00", tz = "UTC")
cutoff_238018 <- as.POSIXct("2023-01-31 20:42:00", tz = "UTC")
cutoff_238019 <- as.POSIXct("2023-03-28 19:15:00", tz = "UTC")
cutoff_252520 <- as.POSIXct("2024-02-03 12:00:00", tz = "UTC") 
cutoff_252525 <- as.POSIXct("2024-02-29 03:08:00", tz = "UTC")
cutoff_252778 <- as.POSIXct("2024-02-20 10:00:00", tz = "UTC")

# Subset the data based on cutoff:
df_204511 <- df_204511 %>% filter(datetime <= cutoff_204511)
df_238018 <- df_238018 %>% filter(datetime <= cutoff_238018)
df_238019 <- df_238019 %>% filter(datetime <= cutoff_238019)
df_252520 <- df_252520 %>% filter(datetime <= cutoff_252520)
df_252525 <- df_252525 %>% filter(datetime <= cutoff_252525)
df_252778 <- df_252778 %>% filter(datetime <= cutoff_252778)

## REMOVE VISUALLY IDENTIFIED POINTS =====================================
# This point was on land so must be removed
df_197235 <- df_197235[-c(4), ]

## SAVE FILE ===================================================================
setwd("RDA_files") #Set working directory to save RDA file

save(df_197235,
     df_204511,
     df_238016,
     df_238018,
     df_238019,
     df_252520,
     df_252525,
     df_252778, 
     all_data,
     file = "horizontal01.RDA")


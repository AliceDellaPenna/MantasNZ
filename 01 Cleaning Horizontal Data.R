## PREP ========================================================================
# Load in packages
library(dplyr); library(lubridate); library(geosphere)


## CLEAN DATA ==================================================================
# Load in data + 
# Remove unnecessary columns + 
# Filter based on residual and time error, and apply speed filter (5ms-1):

# List all files in the directory
file_list <- list.files("/Users/tamsin/Desktop/Manuscript/All_Fastloc_GPS")
# Initialize the 'removed' dataframe
removed <- data.frame()

# Initialize a list to store processed data frames
cleaned_data <- list()

# Loop through each file in the list
for(file in file_list) {
  # Construct the full file path
  file_path <- file.path("/Users/tamsin/Desktop/Manuscript/All_Fastloc_GPS", file)
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
    select(-Residual, -Time.Error)
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
df_252520 <- cleaned_data$"df_252520"
df_252522 <- cleaned_data$"df_252522"
df_252525 <- cleaned_data$"df_252525"
df_252528 <- cleaned_data$"df_252528"
df_252778 <- cleaned_data$"df_252778"
df_252779 <- cleaned_data$"df_252779"

## SUBSET TO DATA OF INTEREST ==================================================
# Remove data following detachment, or exit from study boundaries (visually identified)

# Set datetime to POSIXct class: 
for(file in names(cleaned_data)) {
  # Convert datetime column to POSIXct class
  cleaned_data[[file]]$datetime <- as.POSIXct(cleaned_data[[file]]$datetime, tz = "UTC")
}

# Select cutoff points 
# (Based on detatch time / exit from study boundaries visually identified in QGIS:)
cutoff_252520 <- as.POSIXct("2024-02-03 12:00:00", tz = "UTC") 
cutoff_252522 <- as.POSIXct("2024-03-14 02:14:20", tz = "UTC") 
cutoff_252525 <- as.POSIXct("2024-02-29 03:08:00", tz = "UTC")
cutoff_252528 <- as.POSIXct("2024-03-18 20:00:00", tz = "UTC")
cutoff_252778 <- as.POSIXct("2024-02-20 10:00:00", tz = "UTC")
cutoff_252779 <- as.POSIXct("2024-02-29 21:00:00", tz = "UTC") 

# Subset the data based on cutoff:
df_252520 <- df_252520 %>% filter(datetime <= cutoff_252520)
df_252522 <- df_252522 %>% filter(datetime <= cutoff_252522)
df_252525 <- df_252525 %>% filter(datetime <= cutoff_252525)
df_252528 <- df_252528 %>% filter(datetime <= cutoff_252528)
df_252778 <- df_252778 %>% filter(datetime <= cutoff_252778)
df_252779 <- df_252779 %>% filter(datetime <= cutoff_252779)


## EXPORT AND SAVE DATA ========================================================

# Merge all data frames
df_all <- bind_rows(df_252520, df_252525, df_252778) # Only including data with enough locations (>15)

# Write each data frame to a CSV file
output_dir <- "/Users/tamsin/Desktop/Manuscript/Data/Cleaned_GPS/"
write.csv(df_252520, file = paste0(output_dir, "df_252520.csv"), row.names = FALSE)
write.csv(df_252522, file = paste0(output_dir, "df_252522.csv"), row.names = FALSE)
write.csv(df_252525, file = paste0(output_dir, "df_252525.csv"), row.names = FALSE)
write.csv(df_252528, file = paste0(output_dir, "df_252528.csv"), row.names = FALSE)
write.csv(df_252778, file = paste0(output_dir, "df_252778.csv"), row.names = FALSE)
write.csv(df_252779, file = paste0(output_dir, "df_252779.csv"), row.names = FALSE)
write.csv(df_all, file = paste0(output_dir, "df_all.csv"), row.names = FALSE)

setwd("/Users/tamsin/Desktop/Manuscript/RDA_files") #Set working directory to save RDA file
save(df_252520,df_252525,df_252778, df_all, file = "horizontal01.RDA")

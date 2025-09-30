## PREP ========================================================================
# Load in packages
library(dplyr); library(lubridate); library(geosphere)

## CLEAN DATA ==================================================================
# Load in data + 
# Remove unnecessary columns + 
# Filter based on residual and time error, and apply speed filter (5ms-1):

# List all files in the directory
file_list <- list.files("Fastloc_Raw")

# Initialize the 'removed' dataframe
removed <- data.frame()

# Initialize a list to store processed data frames
cleaned_data <- list()

# Loop through each file in the list
for(file in file_list) {
  # Construct the full file path
  file_path <- file.path("Fastloc_Raw", file)
  # Read the data frame from the file
  df <- read.csv(file_path) 
  
  # Remove additional columns
  columns_to_keep <- c("Name", "Day", "Time", "Latitude", "Longitude", "Residual", "Time.Error")
  df <- df[, columns_to_keep]
  # Rename columns
  df <- df %>%
    rename(date = Day, id = Name, time = Time, lat = Latitude, lon = Longitude)
  
  # Convert 'id' to class = character
  df$id <- as.character(df$id)
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
df_177767 <- cleaned_data$"df_177767"
df_177768 <- cleaned_data$"df_177768"
df_197235 <- cleaned_data$"df_197235"
df_197236 <- cleaned_data$"df_197236"
df_204511 <- cleaned_data$"df_204511"
df_212690 <- cleaned_data$"df_212690"
df_215016 <- cleaned_data$"df_215016"
df_226828 <- cleaned_data$"df_226828"
df_238014 <- cleaned_data$"df_238014"
df_238015 <- cleaned_data$"df_238015"
df_238016 <- cleaned_data$"df_238016"
df_238018 <- cleaned_data$"df_238018"
df_238019 <- cleaned_data$"df_238019"
df_252520 <- cleaned_data$"df_252520"
df_252522 <- cleaned_data$"df_252522"
df_252524 <- cleaned_data$"df_252524"
df_252525 <- cleaned_data$"df_252525"
df_252526 <- cleaned_data$"df_252526"
df_252528 <- cleaned_data$"df_252528"
df_252778 <- cleaned_data$"df_252778"

# Minimum observations required for analysis = 15, remove tags which do not have this 

## SUBSET TO DATA OF INTEREST ==================================================
# Remove data following detachment, or exit from study boundaries (visually identified)
# Define the file path directory
path <- "Fastloc_Processing/"

# Save each dataframe as CSV to inspect movement relative to study boundaries
write.csv(df_177767, paste0(path, "df_177767.csv"), row.names = FALSE)
write.csv(df_177768, paste0(path, "df_177768.csv"), row.names = FALSE)
write.csv(df_197235, paste0(path, "df_197235.csv"), row.names = FALSE)
write.csv(df_197236, paste0(path, "df_197236.csv"), row.names = FALSE)
write.csv(df_204511, paste0(path, "df_204511.csv"), row.names = FALSE)
write.csv(df_212690, paste0(path, "df_212690.csv"), row.names = FALSE)
write.csv(df_215016, paste0(path, "df_215016.csv"), row.names = FALSE)
write.csv(df_226828, paste0(path, "df_226828.csv"), row.names = FALSE)
write.csv(df_238014, paste0(path, "df_238014.csv"), row.names = FALSE)
write.csv(df_238015, paste0(path, "df_238015.csv"), row.names = FALSE)
write.csv(df_238016, paste0(path, "df_238016.csv"), row.names = FALSE)
write.csv(df_238018, paste0(path, "df_238018.csv"), row.names = FALSE)
write.csv(df_238019, paste0(path, "df_238019.csv"), row.names = FALSE)
write.csv(df_252520, paste0(path, "df_252520.csv"), row.names = FALSE)
write.csv(df_252522, paste0(path, "df_252522.csv"), row.names = FALSE)
write.csv(df_252524, paste0(path, "df_252524.csv"), row.names = FALSE)
write.csv(df_252525, paste0(path, "df_252525.csv"), row.names = FALSE)
write.csv(df_252526, paste0(path, "df_252526.csv"), row.names = FALSE)
write.csv(df_252528, paste0(path, "df_252528.csv"), row.names = FALSE)
write.csv(df_252778, paste0(path, "df_252778.csv"), row.names = FALSE)

# Select cutoff points 
# (Based on detatch time / exit from study boundaries visually identified in QGIS:)
cutoff_177767 <- as.POSIXct("2021-03-18 00:00:00", tz = "UTC")
cutoff_177768 <- as.POSIXct("2021-03-12 05:29:00", tz = "UTC")
cutoff_204511 <- as.POSIXct("2022-02-07 08:07:00", tz = "UTC")
cutoff_212690 <- as.POSIXct("2022-02-21 05:18:00", tz = "UTC")
cutoff_215016 <- as.POSIXct("2022-03-01 13:11:00", tz = "UTC")
cutoff_226828 <- as.POSIXct("2022-03-14 00:00:00", tz = "UTC")
cutoff_238014 <- as.POSIXct("2023-02-13 05:41:00", tz = "UTC")
cutoff_238015 <- as.POSIXct("2023-02-02 06:48:00", tz = "UTC")
cutoff_238018 <- as.POSIXct("2023-01-31 20:42:00", tz = "UTC")
cutoff_238019 <- as.POSIXct("2023-03-28 19:15:00", tz = "UTC")
cutoff_252522 <- as.POSIXct("2024-03-15 19:53:00", tz = "UTC")
cutoff_252524 <- as.POSIXct("2024-03-14 04:05:00", tz = "UTC")
cutoff_252525 <- as.POSIXct("2024-02-29 03:08:00", tz = "UTC")
cutoff_252528 <- as.POSIXct("2024-03-12 04:35:00", tz = "UTC")

# Subset the data based on cutoff:
df_177767 <- df_177767 %>% filter(datetime <= cutoff_177767)
df_177768 <- df_177768 %>% filter(datetime <= cutoff_177768)
df_204511 <- df_204511 %>% filter(datetime <= cutoff_204511)
df_212690 <- df_212690 %>% filter(datetime <= cutoff_212690)
df_215016 <- df_215016 %>% filter(datetime <= cutoff_215016)
df_226828 <- df_226828 %>% filter(datetime <= cutoff_226828)
df_238014 <- df_238014 %>% filter(datetime <= cutoff_238014)
df_238015 <- df_238015 %>% filter(datetime <= cutoff_238015)
df_238018 <- df_238018 %>% filter(datetime <= cutoff_238018)
df_238019 <- df_238019 %>% filter(datetime <= cutoff_238019)
df_252522 <- df_252522 %>% filter(datetime <= cutoff_252522)
df_252524 <- df_252524 %>% filter(datetime <= cutoff_252524)
df_252525 <- df_252525 %>% filter(datetime <= cutoff_252525)
df_252528 <- df_252528 %>% filter(datetime <= cutoff_252528)

## SAVE FILE ===================================================================
# Only saving the files which will be used for analysis:
# Must follow criteria = 15+ locations within study area

# Define the file path directory
path <- "Fastloc_Cleaned/"

# Save each dataframe individually
write.csv(df_204511, paste0(path, "df_204511.csv"), row.names = FALSE)
write.csv(df_238014, paste0(path, "df_238014.csv"), row.names = FALSE)
write.csv(df_238016, paste0(path, "df_238016.csv"), row.names = FALSE)
write.csv(df_238018, paste0(path, "df_238018.csv"), row.names = FALSE)
write.csv(df_238019, paste0(path, "df_238019.csv"), row.names = FALSE)
write.csv(df_252520, paste0(path, "df_252520.csv"), row.names = FALSE)
write.csv(df_252522, paste0(path, "df_252522.csv"), row.names = FALSE)
write.csv(df_252524, paste0(path, "df_252524.csv"), row.names = FALSE)
write.csv(df_252525, paste0(path, "df_252525.csv"), row.names = FALSE)
write.csv(df_252526, paste0(path, "df_252526.csv"), row.names = FALSE)
write.csv(df_252528, paste0(path, "df_252528.csv"), row.names = FALSE)
write.csv(df_252778, paste0(path, "df_252778.csv"), row.names = FALSE)

# Save data as one dataframe 
all_data <- bind_rows(
  df_204511,
  df_238014,
  df_238016,
  df_238018,
  df_238019,
  df_252520,
  df_252522,
  df_252524,
  df_252525,
  df_252526,
  df_252528,
  df_252778, 
)

write.csv(all_data, paste0(path, "all_data.csv"), row.names = FALSE)

# as RDA file for working
setwd("RDA_files") #Set working directory to save RDA file

save(
     df_204511,
     df_238014,
     df_238016,
     df_238018,
     df_238019,
     df_252520,
     df_252522,
     df_252524,
     df_252525,
     df_252526,
     df_252528,
     df_252778, 
     all_data,
     file = "horizontal01.RDA")




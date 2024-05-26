## ===== INSTALL PACKAGES / LOAD DATA ==========================================
library(dplyr); library(lubridate); library(geosphere)
setwd("/Users/tamsin/Desktop/Thesis/R") #Set working directory

raw_197235 <- read.csv("/Users/tamsin/Desktop/Thesis/Data/Raw/Raw_GPS/197235-48-FastGPS.csv")
raw_204511 <- read.csv("/Users/tamsin/Desktop/Thesis/Data/Raw/Raw_GPS/204511-27-FastGPS.csv")
raw_215016 <- read.csv("/Users/tamsin/Desktop/Thesis/Data/Raw/Raw_GPS/215016-1-FastGPS.csv")
raw_238015 <- read.csv("/Users/tamsin/Desktop/Thesis/Data/Raw/Raw_GPS/238015-2-FastGPS.csv")
raw_238016 <- read.csv("/Users/tamsin/Desktop/Thesis/Data/Raw/Raw_GPS/238016-3-FastGPS.csv")
raw_238018 <- read.csv("/Users/tamsin/Desktop/Thesis/Data/Raw/Raw_GPS/238018-1-FastGPS.csv")
raw_238019 <- read.csv("/Users/tamsin/Desktop/Thesis/Data/Raw/Raw_GPS/238019-2-FastGPS.csv")
raw_238014 <- read.csv("/Users/tamsin/Desktop/Thesis/Data/Raw/238014/238014-1-FastGPS.csv")
## ===== REMOVE UNWANTED COLUMNS ===============================================
columns_to_keep <- c("Name", "Day", "Time", "Latitude", "Longitude", "Residual", "Time.Error") # Specify the columns to keep

raw_197235 <- raw_197235[, columns_to_keep] # Keep those columns, remove everything else
raw_204511 <- raw_204511[, columns_to_keep]
raw_215016 <- raw_215016[, columns_to_keep]
raw_238015 <- raw_238015[, columns_to_keep]
raw_238016 <- raw_238016[, columns_to_keep]
raw_238018 <- raw_238018[, columns_to_keep]
raw_238019 <- raw_238019[, columns_to_keep]
raw_238014 <- raw_238014[, columns_to_keep]

# Rename the columns
raw_197235 <- raw_197235 %>%
  rename(date = Day, id = Name, time = Time, lat = Latitude, lon = Longitude)
raw_204511 <- raw_204511 %>%
  rename(date = Day, id = Name, time = Time, lat = Latitude, lon = Longitude)
raw_215016 <- raw_215016 %>%
  rename(date = Day, id = Name, time = Time, lat = Latitude, lon = Longitude)
raw_238015 <- raw_238015 %>%
  rename(date = Day, id = Name, time = Time, lat = Latitude, lon = Longitude)
raw_238016 <- raw_238016 %>%
  rename(date = Day, id = Name, time = Time, lat = Latitude, lon = Longitude)
raw_238018 <- raw_238018 %>%
  rename(date = Day, id = Name, time = Time, lat = Latitude, lon = Longitude)
raw_238019 <- raw_238019 %>%
  rename(date = Day, id = Name, time = Time, lat = Latitude, lon = Longitude)
raw_238014 <- raw_238014 %>%
  rename(date = Day, id = Name, time = Time, lat = Latitude, lon = Longitude)

## ===== CREATE DATETIME COLUMNS ===============================================
# Most functions will require a datetime (class = POSIXct) column,
# create this by merging date and time

#Set date column to Class = Date
raw_197235$date <- as.Date(raw_197235$date, format = "%d-%b-%Y", tz = "UTC")
raw_204511$date <- as.Date(raw_204511$date, format = "%d-%b-%Y", tz = "UTC")
raw_215016$date <- as.Date(raw_215016$date, format = "%d-%b-%Y", tz = "UTC")
raw_238015$date <- as.Date(raw_238015$date, format = "%d-%b-%Y", tz = "UTC")
raw_238016$date <- as.Date(raw_238016$date, format = "%d-%b-%Y", tz = "UTC")
raw_238018$date <- as.Date(raw_238018$date, format = "%d-%b-%Y", tz = "UTC")
raw_238019$date <- as.Date(raw_238019$date, format = "%d-%b-%Y", tz = "UTC")
raw_238014$date <- as.Date(raw_238014$date, format = "%d-%b-%Y", tz = "UTC")

# Mutate date and time columns
raw_197235 <- raw_197235 %>%
  mutate(datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
raw_204511 <- raw_204511 %>%
  mutate(datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
raw_215016 <- raw_215016 %>%
  mutate(datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
raw_238015 <- raw_238015 %>%
  mutate(datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
raw_238016 <- raw_238016 %>%
  mutate(datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
raw_238018 <- raw_238018 %>%
  mutate(datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
raw_238019 <- raw_238019 %>%
  mutate(datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
raw_238014 <- raw_238014 %>%
  mutate(datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

## ===== FILTER DATA ===========================================================
# Subset data to remove any values after the tag was released from the animal
cutoff_197235 <- as.POSIXct("2021-03-11 05:15:00", tz = "UTC")
cutoff_204511 <- as.POSIXct("2022-04-03 04:00:00", tz = "UTC")
cutoff_215016 <- as.POSIXct("2022-05-18 15:00:00", tz = "UTC")
cutoff_238015 <- as.POSIXct("2023-02-25 15:00:00", tz = "UTC")
cutoff_238016 <- as.POSIXct("2023-02-12 23:00:00", tz = "UTC")
cutoff_238018 <- as.POSIXct("2023-03-03 18:00:00", tz = "UTC")
cutoff_238019 <- as.POSIXct("2023-04-09 08:00:00", tz = "UTC")
cutoff_238014 <- as.POSIXct("2023-03-03 18:00:00", tz = "UTC")

raw_197235 <- raw_197235 %>%
  filter(datetime <= cutoff_197235)
raw_204511 <- raw_204511 %>%
  filter(datetime <= cutoff_204511)
raw_215016 <- raw_215016 %>%
  filter(datetime <= cutoff_215016)
raw_238015 <- raw_238015 %>%
  filter(datetime <= cutoff_238015)
raw_238016 <- raw_238016 %>%
  filter(datetime <= cutoff_238016)
raw_238018 <- raw_238018 %>%
  filter(datetime <= cutoff_238018)
raw_238019 <- raw_238019 %>%
  filter(datetime <= cutoff_238019)
raw_238014 <- raw_238014 %>%
  filter(datetime <= cutoff_238014)

# Remove rows where Residual is greater than 30 and Time.Error is outside [-5, 5] range
raw_197235 <- raw_197235 %>%
  filter(Residual <= 30, Time.Error >= -5, Time.Error <= 5)
raw_204511 <- raw_204511 %>%
  filter(Residual <= 30, Time.Error >= -5, Time.Error <= 5)
raw_215016 <- raw_215016 %>%
  filter(Residual <= 30, Time.Error >= -5, Time.Error <= 5)
raw_238015 <- raw_238015 %>%
  filter(Residual <= 30, Time.Error >= -5, Time.Error <= 5)
raw_238016 <- raw_238016 %>%
  filter(Residual <= 30, Time.Error >= -5, Time.Error <= 5)
raw_238018 <- raw_238018 %>%
  filter(Residual <= 30, Time.Error >= -5, Time.Error <= 5)
raw_238019 <- raw_238019 %>%
  filter(Residual <= 30, Time.Error >= -5, Time.Error <= 5)
raw_238014 <- raw_238014 %>%
  filter(Residual <= 30, Time.Error >= -5, Time.Error <= 5)

# Remove the "Residual" and "Time.Error" columns from all data frames
raw_197235 <- raw_197235 %>%
  select(-Residual, -Time.Error)
raw_204511 <- raw_204511 %>%
  select(-Residual, -Time.Error)
raw_215016 <- raw_215016 %>%
  select(-Residual, -Time.Error)
raw_238015 <- raw_238015 %>%
  select(-Residual, -Time.Error)
raw_238016 <- raw_238016 %>%
  select(-Residual, -Time.Error)
raw_238018 <- raw_238018 %>%
  select(-Residual, -Time.Error)
raw_238019 <- raw_238019 %>%
  select(-Residual, -Time.Error)
raw_238014 <- raw_238014 %>%
  select(-Residual, -Time.Error)

# Speed filter to remove any values where speed is greater than 5ms-1
# Calculate the distance between points based on latitude and longitude
raw_197235 <- raw_197235 %>%
  mutate(distance = distVincentySphere(cbind(lon, lat), lag(cbind(lon, lat))))
raw_204511 <- raw_204511 %>%
  mutate(distance = distVincentySphere(cbind(lon, lat), lag(cbind(lon, lat))))
raw_215016 <- raw_215016 %>%
  mutate(distance = distVincentySphere(cbind(lon, lat), lag(cbind(lon, lat))))
raw_238015 <- raw_238015 %>%
  mutate(distance = distVincentySphere(cbind(lon, lat), lag(cbind(lon, lat))))
raw_238016 <- raw_238016 %>%
  mutate(distance = distVincentySphere(cbind(lon, lat), lag(cbind(lon, lat))))
raw_238018 <- raw_238018 %>%
  mutate(distance = distVincentySphere(cbind(lon, lat), lag(cbind(lon, lat))))
raw_238019 <- raw_238019 %>%
  mutate(distance = distVincentySphere(cbind(lon, lat), lag(cbind(lon, lat))))
raw_238014 <- raw_238014 %>%
  mutate(distance = distVincentySphere(cbind(lon, lat), lag(cbind(lon, lat))))

# Calculate the time difference in seconds
raw_197235 <- raw_197235 %>%
  mutate(time_diff = as.numeric(difftime(datetime, lag(datetime), units = "secs")))
raw_204511 <- raw_204511 %>%
  mutate(time_diff = as.numeric(difftime(datetime, lag(datetime), units = "secs")))
raw_215016 <- raw_215016 %>%
  mutate(time_diff = as.numeric(difftime(datetime, lag(datetime), units = "secs")))
raw_238015 <- raw_238015 %>%
  mutate(time_diff = as.numeric(difftime(datetime, lag(datetime), units = "secs")))
raw_238016 <- raw_238016 %>%
  mutate(time_diff = as.numeric(difftime(datetime, lag(datetime), units = "secs")))
raw_238018 <- raw_238018 %>%
  mutate(time_diff = as.numeric(difftime(datetime, lag(datetime), units = "secs")))
raw_238019 <- raw_238019 %>%
  mutate(time_diff = as.numeric(difftime(datetime, lag(datetime), units = "secs")))
raw_238014 <- raw_238014 %>%
  mutate(time_diff = as.numeric(difftime(datetime, lag(datetime), units = "secs")))

# Calculate speed 
raw_197235 <- raw_197235 %>%
  mutate(speed = distance / time_diff)
raw_204511 <- raw_204511 %>%
  mutate(speed = distance / time_diff)
raw_215016 <- raw_215016 %>%
  mutate(speed = distance / time_diff)
raw_238015 <- raw_238015 %>%
  mutate(speed = distance / time_diff)
raw_238016 <- raw_238016 %>%
  mutate(speed = distance / time_diff)
raw_238018 <- raw_238018 %>%
  mutate(speed = distance / time_diff)
raw_238019 <- raw_238019 %>%
  mutate(speed = distance / time_diff)
raw_238014 <- raw_238014 %>%
  mutate(speed = distance / time_diff)

# Remove rows where speed is greater than 5
# Initialize a single data frame to store removed rows
removed <- data.frame()
# Store values to be removed for evaluation
removed <- bind_rows(removed, raw_197235 %>%
                       filter(!is.na(speed) & speed >= 5))
removed <- bind_rows(removed, raw_204511 %>%
                       filter(!is.na(speed) & speed >= 5))
removed <- bind_rows(removed, raw_215016 %>%
                       filter(!is.na(speed) & speed >= 5))
removed <- bind_rows(removed, raw_238015 %>%
                       filter(!is.na(speed) & speed >= 5))
removed <- bind_rows(removed, raw_238016 %>%
                       filter(!is.na(speed) & speed >= 5))
removed <- bind_rows(removed, raw_238018 %>%
                       filter(!is.na(speed) & speed >= 5))
removed <- bind_rows(removed, raw_238019 %>%
                       filter(!is.na(speed) & speed >= 5))
removed <- bind_rows(removed, raw_238014 %>%
                       filter(!is.na(speed) & speed >= 5))

raw_197235 <- raw_197235 %>%
  filter(is.na(speed) | speed < 5)
raw_204511 <- raw_204511 %>%
  filter(is.na(speed) | speed < 5)
raw_215016 <- raw_215016 %>%
  filter(is.na(speed) | speed < 5)
raw_238015 <- raw_238015 %>%
  filter(is.na(speed) | speed < 5)
raw_238016 <- raw_238016 %>%
  filter(is.na(speed) | speed < 5)
raw_238018 <- raw_238018 %>%
  filter(is.na(speed) | speed < 5)
raw_238019 <- raw_238019 %>%
  filter(is.na(speed) | speed < 5)
raw_238014 <- raw_238014 %>%
  filter(is.na(speed) | speed < 5)

## ===== EXPORT DATA ===========================================================
output_dir <- "/Users/tamsin/Desktop/Thesis/Data/Horizontal/"
# Write each data frame to a CSV file
write.csv(raw_197235, file = paste0(output_dir, "df_197235.csv"), row.names = FALSE)
write.csv(raw_204511, file = paste0(output_dir, "df_204511.csv"), row.names = FALSE)
write.csv(raw_215016, file = paste0(output_dir, "df_215016.csv"), row.names = FALSE)
write.csv(raw_238015, file = paste0(output_dir, "df_238015.csv"), row.names = FALSE)
write.csv(raw_238016, file = paste0(output_dir, "df_238016.csv"), row.names = FALSE)
write.csv(raw_238018, file = paste0(output_dir, "df_238018.csv"), row.names = FALSE)
write.csv(raw_238019, file = paste0(output_dir, "df_238019.csv"), row.names = FALSE)
write.csv(raw_238014, file = paste0(output_dir, "df_238014.csv"), row.names = FALSE)

## ===== LOAD IN DATA ==========================================================
input_dir <- "/Users/tamsin/Desktop/Thesis/Data/Horizontal/"
df_197235 <- read.csv(file = paste0(input_dir, "df_197235.csv"))
df_204511 <- read.csv(file = paste0(input_dir, "df_204511.csv"))
df_238015 <- read.csv(file = paste0(input_dir, "df_238015.csv"))
df_238016 <- read.csv(file = paste0(input_dir, "df_238016.csv"))
df_238018 <- read.csv(file = paste0(input_dir, "df_238018.csv"))
df_238019 <- read.csv(file = paste0(input_dir, "df_238019.csv"))
df_238014 <- read.csv(file = paste0(input_dir, "df_238014.csv"))

## ===== REMOVE DATA OUT OF STUDY AREA =========================================
# Set datetime to POSIXct class
df_238019$datetime <- as.POSIXct(df_238019$datetime, tz = "UTC"); class(df_238019$datetime)
df_238018$datetime <- as.POSIXct(df_238018$datetime, tz = "UTC"); class(df_238018$datetime)
df_238016$datetime <- as.POSIXct(df_238016$datetime, tz = "UTC"); class(df_238016$datetime)
df_238015$datetime <- as.POSIXct(df_238015$datetime, tz = "UTC"); class(df_238015$datetime)
df_204511$datetime <- as.POSIXct(df_204511$datetime, tz = "UTC"); class(df_204511$datetime)
df_197235$datetime <- as.POSIXct(df_197235$datetime, tz = "UTC"); class(df_197235$datetime)
df_238014$datetime <- as.POSIXct(df_238014$datetime, tz = "UTC"); class(df_238014$datetime)

# Select cutoff based on visual analysis of when manta exited the study boundaries in QGIS
out_238019 <- as.POSIXct("2023-03-28 19:15:00", tz = "UTC")
out_238015 <- as.POSIXct("2023-02-02 06:48:00", tz = "UTC")
out_238018 <- as.POSIXct("2023-01-31 20:42:00", tz = "UTC")
out_204511 <- as.POSIXct("2022-02-07 08:07:00", tz = "UTC")
out_238014 <- as.POSIXct("2023-02-13 06:38:00", tz = "UTC")

df_238019 <- df_238019 %>%
  filter(datetime <= out_238019)
df_238018 <- df_238018 %>%
  filter(datetime <= out_238018)
df_238015 <- df_238015 %>%
  filter(datetime <= out_238015)
df_204511 <- df_204511 %>%
  filter(datetime <= out_204511)
df_238014 <- df_238014 %>%
  filter(datetime <= out_238014)

## ===== REMOVE VISUALLY IDENTIFIED POINTS =====================================
# This point was on land so must be removed
df_197235 <- df_197235[-c(4), ]

## ===== CREATE ONE BIG DATAFRAME ==============================================
all_data <- bind_rows(
  df_197235,
  df_204511,
  df_238015,
  df_238016,
  df_238018,
  df_238019,
  df_238014
)

# Remove extra columns 
all_data <- all_data %>%
  select(-distance, -time_diff, -speed)
df_197235 <- df_197235 %>%
  select(-distance, -time_diff, -speed)

## ===== EXPORT DATA ===========================================================
output_dir <- "/Volumes/Mantarays/Tamsin MSc_20240515/Data/GPSData_Cleaned/"

# Write each data frame to a CSV file
write.csv(df_204511, file = paste0(output_dir, "df_204511.csv"), row.names = FALSE)
write.csv(df_238015, file = paste0(output_dir, "df_238015.csv"), row.names = FALSE)
write.csv(df_238018, file = paste0(output_dir, "df_238018.csv"), row.names = FALSE)
write.csv(df_238019, file = paste0(output_dir, "df_238019.csv"), row.names = FALSE)
write.csv(df_197235, file = paste0(output_dir, "df_197235.csv"), row.names = FALSE)
write.csv(df_238014, file = paste0(output_dir, "df_238014.csv"), row.names = FALSE)
write.csv(all_data, file = paste0(output_dir, "gps_manta2.csv"), row.names = FALSE)


write.csv(df_197235, file = paste0(output_dir, "github.csv"), row.names = FALSE)



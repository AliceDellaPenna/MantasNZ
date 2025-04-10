## PREP ========================================================================
# Load in packages
library(dplyr); library(lubridate); library(geosphere); library(readr); library(stringr)
library(purrr); library(tidyr)

## CLEAN DATA (UNRECOVERED TAGS) ===============================================

# List all CSV files in the directory
file_list <- list.files(path = "/Users/tamsin/Files/Manuscript/Data/Vertical_Unrecovered_Raw/", pattern = "\\.csv$", full.names = TRUE)

# Loop through each file
for (file in file_list) {
  # Read the CSV file
  data <- read_csv(file)
  
  # Extract file name without extension
  file_name <- basename(file)
  file_name_no_ext <- tools::file_path_sans_ext(file_name)
  
  # Create datetime column
  data <- data %>%
    mutate(datetime = as.POSIXct(paste(Day, Time), format = "%d-%b-%Y %H:%M:%S", tz = "UTC"))
  
  # Remove duplicates in datetime column
  data <- data %>%
    distinct(datetime, .keep_all = TRUE)
  
  # Convert datetime to local time
  data$local.time <- as.POSIXct(data$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "Pacific/Auckland")
  
  # Keep only the desired columns
  data <- data %>%
    dplyr::select(Ptt, datetime, local.time, Depth, Temperature)
  
  # Convert Ptt column to character type
  data$Ptt <- as.character(data$Ptt)
  
  # Move files to main environment
  # Create dataframe name with prefix "df_" and first six digits of the original file name (The PTT number)
  df_name <- paste0("df_", substr(file_name_no_ext, 1, 6))
  # Assign each file with the correct name
  assign(df_name, data)
}

## CLEAN DATA (RECOVERED TAGS) =================================================

# List all CSV files in the directory
file_list <- list.files(path = "/Users/tamsin/Files/Manuscript/Data/Vertical_Recovered_Raw", pattern = "\\.csv$", full.names = TRUE)

# Create an empty list to store data frames
recovered_dfs <- list()

# Loop through each file and read it into a data frame
for (file in file_list) {
  recovered_dfs[[file]] <- read.csv(file)
}

# Loop through each file in the recovered_dfs list
for (file_name in names(recovered_dfs)) {
  # Extract the ID from the file name
  ptt <- gsub("-.*", "", basename(file_name))
  
  # Add the 'Ptt' column and fill it with the extracted ID
  recovered_dfs[[file_name]]$Ptt <- ptt
  
  # Convert 'Time' column to datetime
  recovered_dfs[[file_name]]$datetime <- 
    as.POSIXct(recovered_dfs[[file_name]]$Time, 
               format = "%H:%M:%S %d-%b-%Y", 
               tz = "UTC")
  
  # Remove duplicates in datetime column
  recovered_dfs[[file_name]] <- 
    recovered_dfs[[file_name]] %>%
    distinct(datetime, .keep_all = TRUE)
  
  # Add local time column
  recovered_dfs[[file_name]]$local.time <- 
    as.POSIXct(recovered_dfs[[file_name]]$datetime, 
               format = "%Y-%m-%d %H:%M:%S", 
               tz = "Pacific/Auckland")
  
  # Keep only the desired columns
  recovered_dfs[[file_name]] <- recovered_dfs[[file_name]] %>%
    dplyr::select(Ptt, datetime, local.time, Corrected.Depth, External.Temperature)
  
  # Rename columns
  recovered_dfs[[file_name]] <- 
    recovered_dfs[[file_name]] %>%
    rename(Temp = External.Temperature, Depth = Corrected.Depth)
  
  # Move files to main environment
  # Create dataframe name with prefix "df_" and first six digits of the original file name (The PTT number)
  df_name <- paste0("df_", substr(basename(file_name), 1, 6))
  # Assign each file with the correct name
  assign(df_name, recovered_dfs[[file_name]])
}

## SUBSET DATA =================================================================
# Cutoff data following final confirmed GPS position
df_197235 <- df_197235 %>% filter(datetime <= as.POSIXct("2021-03-06 09:48:34", tz = "UTC"))
df_204511 <- df_204511 %>% filter(datetime <= as.POSIXct("2022-02-07 08:06:24", tz = "UTC"))
df_215016 <- df_215016 %>% filter(datetime <= as.POSIXct("2022-03-01 13:10:19", tz = "UTC"))
df_238014 <- df_238014 %>% filter(datetime <= as.POSIXct("2023-02-13 05:40:41", tz = "UTC"))
df_238015 <- df_238015 %>% filter(datetime <= as.POSIXct("2023-02-02 06:47:44", tz = "UTC"))
df_238016 <- df_238016 %>% filter(datetime <= as.POSIXct("2023-02-11 23:10:00", tz = "UTC"))
df_238018 <- df_238018 %>% filter(datetime <= as.POSIXct("2023-01-31 20:41:53", tz = "UTC"))
df_238019 <- df_238019 %>% filter(datetime <= as.POSIXct("2023-03-28 19:14:46", tz = "UTC"))
df_252520 <- df_252520 %>% filter(datetime <= as.POSIXct("2024-02-02 07:12:05", tz = "UTC"))
df_252522 <- df_252522 %>% filter(datetime <= as.POSIXct("2024-03-15 19:52:18", tz = "UTC"))
df_252524 <- df_252524 %>% filter(datetime <= as.POSIXct("2024-03-14 04:04:34", tz = "UTC"))
df_252525 <- df_252525 %>% filter(datetime <= as.POSIXct("2024-02-29 03:07:28", tz = "UTC"))
df_252526 <- df_252526 %>% filter(datetime <= as.POSIXct("2024-03-12 08:01:10", tz = "UTC"))
df_252528 <- df_252528 %>% filter(datetime <= as.POSIXct("2024-03-12 04:34:05", tz = "UTC"))
df_252778 <- df_252778 %>% filter(datetime <= as.POSIXct("2024-02-18 07:45:16", tz = "UTC"))
df_252779 <- df_252779 %>% filter(datetime <= as.POSIXct("2024-02-13 07:41:27", tz = "UTC"))

# Cutoff data prior to deployment on animal
df_197235 <- df_197235 %>% filter(datetime >= as.POSIXct("2021-02-03 00:58:00", tz = "UTC"))
df_204511 <- df_204511 %>% filter(datetime >= as.POSIXct("2022-01-10 04:15:00", tz = "UTC"))
df_215016 <- df_215016 %>% filter(datetime >= as.POSIXct("2022-02-07 03:27:00", tz = "UTC"))
df_238014 <- df_238014 %>% filter(datetime >= as.POSIXct("2023-01-22 01:30:00", tz = "UTC"))
df_238015 <- df_238015 %>% filter(datetime >= as.POSIXct("2023-01-22 06:40:00", tz = "UTC"))
df_238016 <- df_238016 %>% filter(datetime >= as.POSIXct("2023-01-22 04:05:00", tz = "UTC"))
df_238018 <- df_238018 %>% filter(datetime >= as.POSIXct("2023-01-22 05:40:00", tz = "UTC"))
df_238019 <- df_238019 %>% filter(datetime >= as.POSIXct("2023-01-22 06:00:00", tz = "UTC"))
df_252524 <- df_252524 %>% filter(datetime >= as.POSIXct("2023-12-28 03:34:16", tz = "UTC"))
df_252528 <- df_252528 %>% filter(datetime >= as.POSIXct("2023-12-27 03:51:19", tz = "UTC"))

df_252520 <- df_252520 %>% filter(local.time >= as.POSIXct("2023-12-27 14:43:00", tz = "Pacific/Auckland"))
df_252522 <- df_252522 %>% filter(local.time >= as.POSIXct("2023-12-27 15:27:00", tz = "Pacific/Auckland"))
df_252525 <- df_252525 %>% filter(local.time >= as.POSIXct("2023-12-27 16:19:00", tz = "Pacific/Auckland"))
df_252526 <- df_252526 %>% filter(local.time >= as.POSIXct("2023-12-19 14:00:00", tz = "Pacific/Auckland"))
df_252778 <- df_252778 %>% filter(local.time >= as.POSIXct("2024-01-04 15:41:00", tz = "Pacific/Auckland"))
df_252779 <- df_252779 %>% filter(local.time >= as.POSIXct("2024-01-04 18:04:00", tz = "Pacific/Auckland"))


## SAVE FILES ==================================================================

# Define the folder path
folder_path <- "/Users/tamsin/Files/Manuscript/Data/Vertical_Cleaned"

# Save each filtered data frame as a CSV file in the folder
write.csv(df_197235, file.path(folder_path, "df_197235_filtered.csv"), row.names = FALSE)
write.csv(df_204511, file.path(folder_path, "df_204511_filtered.csv"), row.names = FALSE)
write.csv(df_215016, file.path(folder_path, "df_215016_filtered.csv"), row.names = FALSE)
write.csv(df_238014, file.path(folder_path, "df_238014_filtered.csv"), row.names = FALSE)
write.csv(df_238015, file.path(folder_path, "df_238015_filtered.csv"), row.names = FALSE)
write.csv(df_238016, file.path(folder_path, "df_238016_filtered.csv"), row.names = FALSE)
write.csv(df_238018, file.path(folder_path, "df_238018_filtered.csv"), row.names = FALSE)
write.csv(df_238019, file.path(folder_path, "df_238019_filtered.csv"), row.names = FALSE)
write.csv(df_252520, file.path(folder_path, "df_252520_filtered.csv"), row.names = FALSE)
write.csv(df_252522, file.path(folder_path, "df_252522_filtered.csv"), row.names = FALSE)
write.csv(df_252524, file.path(folder_path, "df_252524_filtered.csv"), row.names = FALSE)
write.csv(df_252525, file.path(folder_path, "df_252525_filtered.csv"), row.names = FALSE)
write.csv(df_252526, file.path(folder_path, "df_252526_filtered.csv"), row.names = FALSE)
write.csv(df_252528, file.path(folder_path, "df_252528_filtered.csv"), row.names = FALSE)
write.csv(df_252778, file.path(folder_path, "df_252778_filtered.csv"), row.names = FALSE)
write.csv(df_252779, file.path(folder_path, "df_252779_filtered.csv"), row.names = FALSE)

setwd("/Users/tamsin/Files/Manuscript/RDA_files") #Set working directory to save RDA file

save(df_197235, 
     df_204511,
     df_215016,
     df_238014,
     df_238015,
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
     df_252779, 
     file = "vertical01.RDA")













## PREP ========================================================================
# Load in packages
library(suncalc); library(lubridate); library(dplyr); library(tidyverse)

#Set working directory
setwd("RDA_files") 
# Load in the data 
load("vertical01.RDA")

## CLASSIFY DAY / NIGHT ========================================================
# Collect sunlight times for all dates using the mean location of data spread
timezone <- "Pacific/Auckland" # Specify the time zone (Pacific/Auckland)

# Create a sequence of dates with the specified time zone
first_date <-"2021-01-03"; first_date <- as.POSIXct(first_date, tz = "Pacific/Auckland")
last_date <- "2024-03-16"; last_date <- as.POSIXct(last_date, tz = "Pacific/Auckland")

date_sequence <- seq(from = as.POSIXct(first_date, tz = timezone),
                     to = as.POSIXct(last_date, tz = timezone),
                     by = "days")

# Create a data frame with the dates
date_df <- data.frame(Date = date_sequence)
date_df$lon <- 175.25; date_df$lat=-36.5 #Set mean geolocation as no location data available for vertical data
date_df$date <- as.Date(date_df$Date)
date_df <- subset(date_df, select = -Date)

# Get sunlight times for each date
suntimes <- getSunlightTimes(data = date_df, 
                             tz = "Pacific/Auckland",
                             keep = c("sunrise", "sunset"))
suntimes$local.date <- as.Date(suntimes$sunrise, tz= "Pacific/Auckland")

# List of your data frames
df_list <- list(df_197235, 
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
                df_252779)

# Create a vector to store the original names of the data frames
original_names <- c("df_197235", 
                    "df_204511",
                    "df_215016",
                    "df_238014", 
                    "df_238015", 
                    "df_238016", 
                    "df_238018", 
                    "df_238019", 
                    "df_252520", 
                    "df_252522",
                    "df_252524",
                    "df_252525", 
                    "df_252526",
                    "df_252528", 
                    "df_252778", 
                    "df_252779")

# Loop through each data frame
for (i in seq_along(df_list)) {
  # Preserve the original name of the data frame
  original_name <- original_names[i]
  
  # Create local.date column
  df_list[[i]]$local.date <- as.Date(df_list[[i]]$local.time)
  
  # Check if columns for merge exist
  if (!all(c("local.date") %in% names(df_list[[i]]))) {
    stop("Required columns for merge do not exist in data frame ", original_name)
  }
  
  # Merge with suntimes
  df_list[[i]] <- merge(df_list[[i]], suntimes, by.x = "local.date", by.y = "local.date", all.x = TRUE)
  
  # Apply the additional operation for all data frames
  df_list[[i]] <- df_list[[i]] %>%
    mutate(
      daytime = ifelse(local.time >= sunrise & local.time <= sunset, "Day", "Night")
    )
  
  # Remove extra columns for all data frames
  df_list[[i]] <- df_list[[i]][, !(names(df_list[[i]]) %in% c("date", "lat", "lon", "sunset", "sunrise", "local.date"))]
  
  # Move data frames to the global environment with original names
  assign(original_name, df_list[[i]], envir = globalenv())
}

## Get statistics =============================================================
# Combine all data frames in df_list into one data frame
combined_df <- bind_rows(df_list)

# Calculate mean, max, and standard deviation for the "Depth" column
depth_stats <- combined_df %>%
  summarize(
    mean_depth = mean(Depth, na.rm = TRUE),    # Mean of Depth
    max_depth = max(Depth, na.rm = TRUE),      # Max of Depth
    sd_depth = sd(Depth, na.rm = TRUE)         # Standard deviation of Depth
  )

# Print the statistics
print(depth_stats)

# Calculate statistics for "Temp" in df_197235
temp_stats_197235 <- df_197235 %>%
  summarize(
    mean_temp = mean(Temp, na.rm = TRUE),      # Mean of Temp
    min_temp = min(Temp, na.rm = TRUE),       # Minimum Temp
    max_temp = max(Temp, na.rm = TRUE),       # Maximum Temp
    sd_temp = sd(Temp, na.rm = TRUE)          # Standard deviation of Temp
  )

# Print the statistics for df_197235
print(temp_stats_197235)

# Calculate statistics for "Temp" in df_238016
temp_stats_238016 <- df_238016 %>%
  summarize(
    mean_temp = mean(Temp, na.rm = TRUE),      # Mean of Temp
    min_temp = min(Temp, na.rm = TRUE),       # Minimum Temp
    max_temp = max(Temp, na.rm = TRUE),       # Maximum Temp
    sd_temp = sd(Temp, na.rm = TRUE)          # Standard deviation of Temp
  )

# Print the statistics for df_238016
print(temp_stats_238016)

# Calculate statistics for "Temp" in df_252779
temp_stats_252779 <- df_252779 %>%
  summarize(
    mean_temp = mean(Temp, na.rm = TRUE),      # Mean of Temp
    min_temp = min(Temp, na.rm = TRUE),       # Minimum Temp
    max_temp = max(Temp, na.rm = TRUE),       # Maximum Temp
    sd_temp = sd(Temp, na.rm = TRUE)          # Standard deviation of Temp
  )

# Print the statistics for df_252779
print(temp_stats_252779)
## REGULARISE DATA =============================================================
# Regularise data for better comparisons in further analysis 

# Function to regularize temporal resolution
regularize_temporal_resolution <- function(data, time_interval = 300) { #Set time interval at coarsest resolution (5min in this dataset)
  # Create a sequence of time intervals
  regular_time_intervals <- seq(min(data$local.time), max(data$local.time), by = time_interval)
  
  # Create a data frame with regular time intervals
  regular_data <- data.frame(local.time = regular_time_intervals)
  
  # Merge the original data with the regularized time intervals
  result_data <- merge(regular_data, data, by = "local.time", all.x = TRUE)
  
  return(result_data)
}

# Loop through each data frame to regularize temporal resolution
for (i in seq_along(df_list)) {
  # Regularize temporal resolution
  df_list[[i]] <- regularize_temporal_resolution(df_list[[i]])
} # Now df_list contains time regularized data frames for further analysis

# Merge all data frames into one big data frame
regularised_df <- bind_rows(df_list)

# Move big_df to the main environment
assign("regularised_df", regularised_df, envir = .GlobalEnv)


## TIME AT DEPTH ===============================================================

# Separate data dependent on whether it was during the day or night
dive_day <- regularised_df %>%
  filter(daytime == "Day")
dive_night <- regularised_df %>%
  filter(daytime == "Night")


# Define custom breaks and labels for depth category/bin
tad_breaks <- c(-1, 5, 10, 15, 20, 25, 50, 100, 365)
tad_labels <- c("0-5", "5-10", "10-15", "15-20", "20-25", "25-50", "50-100", "100-365")


# Calculate the percentage of overall time spent within each depth category:

# Day
dive_day$Depth_Bin <- cut(dive_day$Depth, breaks = tad_breaks, labels = tad_labels)
# Calculate the percentage of time spent in each depth bin
day_stats <- dive_day %>%
  group_by(Depth_Bin) %>%
  summarise(Percent_Time = n() / nrow(dive_day))
# Remove rows with NA in the Percent_Time column
day_stats <- na.omit(day_stats)
# Change to a %
day_stats$Percentage <- day_stats$Percent_Time * 100

# Order the Depth_Bin levels and labels according to your specified order for day
day_stats$Depth_Bin <- fct_relevel(day_stats$Depth_Bin, tad_labels)
day_stats$Depth_Bin <- fct_rev(day_stats$Depth_Bin)  # Reverse the order

# Night
dive_night$Depth_Bin <- cut(dive_night$Depth, breaks = tad_breaks, labels = tad_labels)
# Calculate the percentage of time spent in each depth bin
night_stats <- dive_night %>%
  group_by(Depth_Bin) %>%
  summarise(Percent_Time = n() / nrow(dive_night))
# Remove rows with NA in the Percent_Time column
night_stats <- na.omit(night_stats)
# Change to a %
night_stats$Percentage <- night_stats$Percent_Time * 100

# Order the Depth_Bin levels and labels according to your specified order for night
night_stats$Depth_Bin <- fct_relevel(night_stats$Depth_Bin, tad_labels)
night_stats$Depth_Bin <- fct_rev(night_stats$Depth_Bin)  # Reverse the order

# Plot the data ----------------------------------------------------------------
# Plot the data as a back-to-back histogram plot 

tad.plot <- ggplot() +
  geom_col(data = day_stats, aes(x = Depth_Bin, y = -Percentage), fill = "white", color = "black", position = "identity", width = 0.9) +
  geom_col(data = night_stats, aes(x = Depth_Bin, y = Percentage), fill = "black", color = "black", position = "identity", width = 0.9) +
  geom_text(data = day_stats, 
            aes(x = Depth_Bin, y = -Percentage, label = sprintf("%.1f", Percentage), vjust = 0, hjust = 1.3),
            size = 3) +
  geom_text(data = night_stats, 
            aes(x = Depth_Bin, y = Percentage, label = sprintf("%.1f", Percentage), vjust = 0, hjust = -0.3),
            size = 3) +
  labs(x = "Depth (m)",  # Adjust x-axis label
       y = "Time at Depth (%)") +  # Adjust y-axis label
  theme_minimal() +
  theme(
    text = element_text(size = 10, color = "black", family = "Arial"),
    axis.text = element_text(size = 10, color = "black", family = "Arial"),
    axis.line = element_line(color = "black"),  # Add axis lines
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),  # Set background to white and remove border
    plot.background = element_rect(fill = "white", color = NA),   # Set plot background to white and remove border
    axis.ticks = element_line(color = "black"),  # Add axis ticks
    plot.margin = margin(1, 1, 1, 1, "cm"),  # Adjust plot margins
    plot.title = element_text(hjust = 0.5)  # Center the plot title
  ) +
  scale_y_continuous(labels = abs, limits = c(-100, 100), breaks = seq(-100, 100, by = 20)) +
  coord_flip() 

ggsave("figure_5.tiff", 
       plot = tad.plot, width = 8.3, height = 5.5, units = "in", dpi = 1000)


# Compare Day/Night ------------------------------------------------------------
# Extract percentages for the 0-5m depth category
day_0_5 <- day_stats %>% filter(Depth_Bin == "0-5") %>% pull(Percentage)
night_0_5 <- night_stats %>% filter(Depth_Bin == "0-5") %>% pull(Percentage)

# Conduct Mann-Whitney U test
mann_whitney_test <- wilcox.test(day_0_5, night_0_5, alternative = "two.sided", exact = FALSE)

# Display the results
print(mann_whitney_test)

## SAVE FILE ===================================================================
setwd("RDA_files") #Set working directory to save RDA file

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
     regularised_df,
     file = "vertical02.RDA")
















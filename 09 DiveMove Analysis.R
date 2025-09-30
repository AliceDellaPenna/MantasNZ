## PREP ========================================================================
# Load in packages
library(diveMove); library(suncalc); library(lubridate); library(dplyr); library(tidyverse)

#Set working directory
setwd("RDA_files") 
# Load in the data 
load("vertical02.RDA")


## REGULARISE DATA =============================================================
# Regularise data for better comparisons in further analysis 
# List of your data frames
df_list <- list(df_197235, df_238016, df_252779) # Create list of recovered tags
file_names <- c("df_197235", "df_238016", "df_252779")  # Extract original file names 


# Function to regularize temporal resolution
regularize_temporal_resolution <- function(data, time_interval = 10) { #Set time interval at coarsest resolution (5min in this dataset)
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

# Move df_list files to the global environment
list2env(setNames(df_list, file_names), envir = .GlobalEnv)

## ANALYSING THE DATA USING DIVEMOVE PACKAGE ===================================
# For each recovered tag: 

# df_197235 --------------------------------------------------------------------
# Create TDR 
tdr_197235 <- createTDR(
  df_197235$local.time,
  df_197235$Depth,
  dtime = 10, # Resolution (seconds)
  file = "")

dcalib_197235 <- calibrateDepth(tdr_197235, 
                                dive.thr = 10, # Threshold for minimum dive depth
                                zoc.method="offset",
                                offset=0)

# Calculate dive statistics using diveStats()
divestats_197235 <- diveStats(dcalib_197235)
summary(divestats_197235)

# df_238016 --------------------------------------------------------------------
# Create TDR 
tdr_238016 <- createTDR(
  df_238016$local.time,
  df_238016$Depth,
  dtime = 10, # Resolution (seconds)
  file = "")

dcalib_238016 <- calibrateDepth(tdr_238016, 
                                dive.thr = 10, # Threshold for minimum dive depth
                                zoc.method="offset",
                                offset=0)

# Calculate dive statistics using diveStats()
divestats_238016 <- diveStats(dcalib_238016)
summary(divestats_238016)


# df_252779 --------------------------------------------------------------------
# Create TDR 
tdr_252779 <- createTDR(
  df_252779$local.time,
  df_252779$Depth,
  dtime = 10, # Resolution (seconds)
  file = "")

dcalib_252779 <- calibrateDepth(tdr_252779, 
                                dive.thr = 10, # Threshold for minimum dive depth
                                zoc.method="offset",
                                offset=0)

# Calculate dive statistics using diveStats()
divestats_252779 <- diveStats(dcalib_252779)
summary(divestats_252779)


## EXTRACT ENVIRO DATA =========================================================

## EXTRACT TEMPERATURE =========================================================
# Extract mean & min temperature for each dive
# Using the datetimes at begdesc and begasc

# df_197235 --------------------------------------------------------------------
# Loop through each row
for (i in 1:nrow(divestats_197235)) {
  # Calculate mean and min
  mean_val <- mean(df_197235[which(df_197235$local.time == divestats_197235$begdesc[i]):which(df_197235$local.time == divestats_197235$begasc[i]), 5])
  min_val <- min(df_197235[which(df_197235$local.time == divestats_197235$begdesc[i]):which(df_197235$local.time == divestats_197235$begasc[i]), 5])
  
  # Assign mean and min values to corresponding rows
  divestats_197235$mean[i] <- mean_val
  divestats_197235$min[i] <- min_val
}

# df_238016 --------------------------------------------------------------------
# Loop through each row
for (i in 1:nrow(divestats_238016)) {
  # Calculate mean and min
  mean_val <- mean(df_238016[which(df_238016$local.time == divestats_238016$begdesc[i]):which(df_238016$local.time == divestats_238016$begasc[i]), 5])
  min_val <- min(df_238016[which(df_238016$local.time == divestats_238016$begdesc[i]):which(df_238016$local.time == divestats_238016$begasc[i]), 5])
  
  # Assign mean and min values to corresponding rows
  divestats_238016$mean[i] <- mean_val
  divestats_238016$min[i] <- min_val
}

# df_252779 --------------------------------------------------------------------
# Loop through each row
for (i in 1:nrow(divestats_252779)) {
  # Calculate mean and min
  mean_val <- mean(df_252779[which(df_252779$local.time == divestats_252779$begdesc[i]):which(df_252779$local.time == divestats_252779$begasc[i]), 5])
  min_val <- min(df_252779[which(df_252779$local.time == divestats_252779$begdesc[i]):which(df_252779$local.time == divestats_252779$begasc[i]), 5])
  
  # Assign mean and min values to corresponding rows
  divestats_252779$mean[i] <- mean_val
  divestats_252779$min[i] <- min_val
}

## TIME OF DAY =================================================================
# Collect sunlight times for all dates using the mean location of data spread
timezone <- "Pacific/Auckland" # Specify the time zone (Pacific/Auckland)

# Create a sequence of dates with the specified time zone
first_date <-"2021-02-03"; first_date <- as.POSIXct(first_date, tz = "Pacific/Auckland")
last_date <- "2024-02-29"; last_date <- as.POSIXct(last_date, tz = "Pacific/Auckland")

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


# Extract sunlight times
divestats_197235$local.date <- as.Date(divestats_197235$begdesc, tz = "Pacific/Auckland")
divestats_197235 <- merge(divestats_197235, suntimes, by.x = "local.date", by.y = "local.date", all.x = TRUE)

divestats_238016$local.date <- as.Date(divestats_238016$begdesc, tz = "Pacific/Auckland")
divestats_238016 <- merge(divestats_238016, suntimes, by.x = "local.date", by.y = "local.date", all.x = TRUE)

divestats_252779$local.date <- as.Date(divestats_252779$begdesc, tz = "Pacific/Auckland")
divestats_252779 <- merge(divestats_252779, suntimes, by.x = "local.date", by.y = "local.date", all.x = TRUE)

# Determine whether night or day (At start of dive)
divestats_197235$period <- ifelse(divestats_197235$begdesc >= divestats_197235$sunrise & 
                                    divestats_197235$begdesc <= divestats_197235$sunset,
                                  "day", "night")
divestats_238016$period <- ifelse(divestats_238016$begdesc >= divestats_238016$sunrise & 
                                    divestats_238016$begdesc <= divestats_238016$sunset,
                                  "day", "night")
divestats_252779$period <- ifelse(divestats_252779$begdesc >= divestats_252779$sunrise & 
                                    divestats_252779$begdesc <= divestats_252779$sunset,
                                  "day", "night")

## MOON ILLUMINAION ============================================================

# Use moonillumination function from suncalc package to extract fraction 
moonfraction_197235 <- getMoonIllumination(date = divestats_197235$local.date, keep = c("fraction"))
divestats_197235 <- cbind(divestats_197235, fraction = moonfraction_197235$fraction)

moonfraction_238016 <- getMoonIllumination(date = divestats_238016$local.date, keep = c("fraction"))
divestats_238016 <- cbind(divestats_238016, fraction = moonfraction_238016$fraction)

moonfraction_252779 <- getMoonIllumination(date = divestats_252779$local.date, keep = c("fraction"))
divestats_252779 <- cbind(divestats_252779, fraction = moonfraction_252779$fraction)


## SUN ALTITUDE ================================================================
# Create a new dataframe with necessary columns
divestats_197235$utc.date <- as.POSIXct(divestats_197235$begasc, tz = "UTC")
sun_197235 <- divestats_197235[, c("utc.date", "lat", "lon")]
names(sun_197235)[names(sun_197235) == "utc.date"] <- "date"
sun_197235 <- getSunlightPosition(data = sun_197235, keep = c("altitude"))
sun_197235$altitude <- sun_197235$altitude * (180 / pi)
divestats_197235 <- merge(divestats_197235, sun_197235, by.x = "utc.date", by.y = "date", all.x = TRUE)


divestats_238016$utc.date <- as.POSIXct(divestats_238016$begasc, tz = "UTC")
sun_238016 <- divestats_238016[, c("utc.date", "lat", "lon")]
names(sun_238016)[names(sun_238016) == "utc.date"] <- "date"
sun_238016 <- getSunlightPosition(data = sun_238016, keep = c("altitude"))
sun_238016$altitude <- sun_238016$altitude * (180 / pi)
divestats_238016 <- merge(divestats_238016, sun_238016, by.x = "utc.date", by.y = "date", all.x = TRUE)


divestats_252779$utc.date <- as.POSIXct(divestats_252779$begasc, tz = "UTC")
sun_252779 <- divestats_252779[, c("utc.date", "lat", "lon")]
names(sun_252779)[names(sun_252779) == "utc.date"] <- "date"
sun_252779 <- getSunlightPosition(data = sun_252779, keep = c("altitude"))
sun_252779$altitude <- sun_252779$altitude * (180 / pi)
divestats_252779 <- merge(divestats_252779, sun_252779, by.x = "utc.date", by.y = "date", all.x = TRUE)



setwd("RDA_files") #Set working directory to save RDA file
save(divestats_238016,divestats_197235,divestats_252779, file = "vertical2b.RDA")

## GAM =========================================================================
#
# AIM:
# To assess the influence of environmental variables on a range of diving metrics
#
## PREP ========================================================================
# Load in packages
library(mgcv); library(gratia);library(patchwork);library(lubridate)

load("vertical2b.RDA")

# Merge data frames
divestats_197235$id <- 197235
divestats_238016$id <- 238016
divestats_252779$id <- 252779
all_df <- bind_rows(divestats_197235, divestats_238016, divestats_252779)

# Subset data into night and day observations
night <- all_df %>% filter(period == "night")
day <- all_df %>% filter(period == "day")


### Max depth ------------------------------------------------------------------
# Log-transform response variable
day$logmaxdep <- log(day$maxdep)

# Run the gam
d_maxdepth <- 
  gam(logmaxdep ~ 
        s(altitude, k = 6) +
        s(min, k = 6),
      data = day,
      method = "REML",
      family = tw(link = "log"))
summary(d_maxdepth)

### Bottom Depth ---------------------------------------------------------------
# Log-transform response variable
day$logbottdep <- log(day$bottdep.mean)

# Run the gam
d_bottdep <- 
  gam(logbottdep ~ 
        s(altitude, k = 6)+
        s(min, k = -1),
      data = day,
      method = "REML",
      family = tw(link = "log"))
summary(d_bottdep)

### Dive time ------------------------------------------------------------------
# Run the gam
d_divetim <- 
  gam(divetim ~ 
        s(altitude, k = 6) +
        s(min, k = 6),
      data = day,
      method = "REML",
      family = tw(link = "log"))
summary(d_divetim)

### Post Dive Duration ---------------------------------------------------------
# Log-transform response variable
day$logpostdive <- log(day$postdive.dur)

# Run the gam
d_postdive.dur <- 
  gam(postdive.dur ~ 
        s(altitude, k = 6)+
        s(min, k = 6),
      data = day,
      method = "REML",
      family = tw(link = "log"))
summary(d_postdive.dur)


## GAM (NIGHT) =================================================================
### Max depth ------------------------------------------------------------------
# Log-transform response variable
night$logmaxdep <- log(night$maxdep)

# Run the gam
n_maxdepth <- 
  gam(logmaxdep ~ 
        s(fraction, k = 6) +
        s(min, k = 6),
      data = night,
      method = "REML",
      family = tw(link = "log"))
summary(n_maxdepth)

### Bottom Depth ---------------------------------------------------------------
# Log-transform response variable
night$logbottdep <- log(night$bottdep.mean)

# Run the gam
n_bottdepth <- 
  gam(logbottdep ~ 
        s(fraction, k = 6)+
        s(min, k = -1),
      data = night,
      method = "REML",
      family = tw(link = "log"))
summary(n_bottdepth)

### Dive time ------------------------------------------------------------------
# Run the gam
n_divetim <- 
  gam(divetim ~ 
        s(fraction, k = 6) +
        s(min, k = 6),
      data = night,
      method = "REML",
      family = tw(link = "log"))
summary(n_divetim)

### Post Dive Duration ---------------------------------------------------------
# Log-transform response variable
night$logpostdive <- log(night$postdive.dur)

# Run the gam
n_postdive.dur <- 
  gam(postdive.dur ~ 
        s(fraction, k = 6)+
        s(min, k = 6),
      data = night,
      method = "REML",
      family = tw(link = "log"))
summary(n_postdive.dur)


## PLOTTING GAM (DAY) ==========================================================
### Max depth ------------------------------------------------------------------
# evaluate the smooths
sm_d_maxdepth <- smooth_estimates(d_maxdepth) %>%
  add_confint()

#### Temperature -----------------------------------------------------------------------
a <-ggplot(sm_d_maxdepth, aes(y = est, x = min)) +
  geom_rug(data = day, mapping = aes(x = min), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1) +
  labs(y =  "Maximum Depth (Partial Effect)",
       x = NULL) +
  theme_minimal() +
  theme(panel.background = element_blank(),  # Remove panel background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"),  # Add axis line
        axis.ticks = element_line(colour = "black"),  # Set color of tick marks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of tick marks
        axis.ticks.margin = unit(0.5, "cm"),  # Set margin between tick marks and plot area
        axis.title = element_text(family = "Arial", size = 10, color = "black"),  # Set axis title properties
        axis.text = element_text(family = "Arial", size = 10, color = "black"))  +  # Set axis text properties
  coord_cartesian(ylim = c(-0.3, 0.8))  # Set y-axis limits

#### Altitude -----------------------------------------------------------------------
b <- ggplot(sm_d_maxdepth, aes(y = est, x = altitude)) +
  geom_rug(data = sm_d_maxdepth, mapping = aes(x = altitude), sides = "b") +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +
  geom_line(colour = "#a6cee3", size = 1) +
  labs(y = NULL, x = NULL) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.ticks.margin = unit(0.5, "cm"),
        axis.title = element_text(family = "Arial", size = 10, color = "black"),
        axis.text = element_text(family = "Arial", size = 10, color = "black")) +
  coord_cartesian(ylim = c(-0.05, 0.1))  



### Bott depth ------------------------------------------------------------------
# evaluate the smooths
sm_d_bottdep <- smooth_estimates(d_bottdep) %>%
  add_confint()

#### Temperature -----------------------------------------------------------------------
c <- ggplot(sm_d_bottdep, aes(y = est, x = min)) +
  geom_rug(data = day, mapping = aes(x = min), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1) +
  labs(y = "Mean Bottom Depth (Partial Effect)",
       x = NULL) +
  theme_minimal() +
  theme(panel.background = element_blank(),  # Remove panel background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"),  # Add axis line
        axis.ticks = element_line(colour = "black"),  # Set color of tick marks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of tick marks
        axis.ticks.margin = unit(0.5, "cm"),  # Set margin between tick marks and plot area
        axis.title = element_text(family = "Arial", size = 10, color = "black"),  # Set axis title properties
        axis.text = element_text(family = "Arial", size = 10, color = "black"))  +  # Set axis text properties
  coord_cartesian(ylim = c(-0.3, 0.6))  # Set y-axis limits
#### Altitude -----------------------------------------------------------------------
d <- ggplot(sm_d_bottdep, aes(y = est, x = altitude)) +
  geom_rug(data = day, mapping = aes(x = altitude), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1) +
  labs(y = NULL,
       x = NULL) +
  theme_minimal() +
  theme(panel.background = element_blank(),  # Remove panel background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"),  # Add axis line
        axis.ticks = element_line(colour = "black"),  # Set color of tick marks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of tick marks
        axis.ticks.margin = unit(0.5, "cm"),  # Set margin between tick marks and plot area
        axis.title = element_text(family = "Arial", size = 10, color = "black"),  # Set axis title properties
        axis.text = element_text(family = "Arial", size = 10, color = "black"))  +  # Set axis text properties
  coord_cartesian(ylim = c(-0.05, 0.1))  # Set y-axis limits

### Divetim ------------------------------------------------------------------
# evaluate the smooths
sm_d_divetim <- smooth_estimates(d_divetim) %>%
  add_confint()

#### Temperature -----------------------------------------------------------------------
e <- ggplot(sm_d_divetim, aes(y = est, x = min)) +
  geom_rug(data = day, mapping = aes(x = min), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1) +
  labs(y = "Dive Duration (Partial Effect)",
       x = "Temperature (°C)") +
  theme_minimal() +
  theme(panel.background = element_blank(),  # Remove panel background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"),  # Add axis line
        axis.ticks = element_line(colour = "black"),  # Set color of tick marks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of tick marks
        axis.ticks.margin = unit(0.5, "cm"),  # Set margin between tick marks and plot area
        axis.title = element_text(family = "Arial", size = 10, color = "black"),  # Set axis title properties
        axis.text = element_text(family = "Arial", size = 10, color = "black"))  +  # Set axis text properties
  coord_cartesian(ylim = c(-2, 3))  # Set y-axis limits
#### Altitude -----------------------------------------------------------------------
f <- ggplot(sm_d_divetim, aes(y = est, x = altitude)) +
  geom_rug(data = day, mapping = aes(x = altitude), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1) +
  labs(y = NULL,
       x = "Solar Altitude (°)") +
  theme_minimal() +
  theme(panel.background = element_blank(),  # Remove panel background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"),  # Add axis line
        axis.ticks = element_line(colour = "black"),  # Set color of tick marks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of tick marks
        axis.ticks.margin = unit(0.5, "cm"),  # Set margin between tick marks and plot area
        axis.title = element_text(family = "Arial", size = 10, color = "black"),  # Set axis title properties
        axis.text = element_text(family = "Arial", size = 10, color = "black"))  +  # Set axis text properties
  coord_cartesian(ylim = c(-0.5, 1))  # Set y-axis limits


#### Plot ----------------------------------------------------------------------
# Combine all plots into one figure
day <- ( a | b | c | d | e | f ) +
  plot_layout(ncol = 2, nrow = 3)  # Arrange plots in two columns
# Display the combined plot
day

ggsave("figure 7.tiff", 
       plot = day, width = 8.3, height = 10, units = "in", dpi = 1000)



## ===== GAM STATS_ (NIGHT) ==============================================
### Max depth ------------------------------------------------------------------
# evaluate the smooths
sm_n_maxdepth <- smooth_estimates(n_maxdepth) %>%
  add_confint()

#### Temperature -----------------------------------------------------------------------
a2 <- ggplot(sm_n_maxdepth, aes(y = est, x = min)) +
  geom_rug(data = night, mapping = aes(x = min), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1) +
  labs(y = "Maximum Depth (Partial Effect)",
       x = NULL) +
  theme_minimal() +
  theme(panel.background = element_blank(),  # Remove panel background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"),  # Add axis line
        axis.ticks = element_line(colour = "black"),  # Set color of tick marks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of tick marks
        axis.ticks.margin = unit(0.5, "cm"),  # Set margin between tick marks and plot area
        axis.title = element_text(family = "Arial", size = 10, color = "black"),  # Set axis title properties
        axis.text = element_text(family = "Arial", size = 10, color = "black"))  +  # Set axis text properties
  coord_cartesian(ylim = c(-0.2, 0.7))  # Set y-axis limits

#### Moon fraction -----------------------------------------------------------------------
b2 <- ggplot(sm_n_maxdepth, aes(y = est, x = fraction)) +
  geom_rug(data = night, mapping = aes(x = fraction), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1) +
  labs(y = NULL,
       x = NULL) +
  theme_minimal() +
  theme(panel.background = element_blank(),  # Remove panel background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"),  # Add axis line
        axis.ticks = element_line(colour = "black"),  # Set color of tick marks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of tick marks
        axis.ticks.margin = unit(0.5, "cm"),  # Set margin between tick marks and plot area
        axis.title = element_text(family = "Arial", size = 10, color = "black"),  # Set axis title properties
        axis.text = element_text(family = "Arial", size = 10, color = "black"))  +  # Set axis text properties
  coord_cartesian(ylim = c(-0.05, 0.05))  # Set y-axis limits


### Bott depth------------------------------------------------------------------
# evaluate the smooths
sm_n_bottdepth <- smooth_estimates(n_bottdepth) %>%
  add_confint()

#### Temperature -----------------------------------------------------------------------
c2 <- ggplot(sm_n_bottdepth, aes(y = est, x = min)) +
  geom_rug(data = night, mapping = aes(x = min), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1) +
  labs(y = "Mean Bottom Depth (Partial Effect)",
       x = NULL) +
  theme_minimal() +
  theme(panel.background = element_blank(),  # Remove panel background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"),  # Add axis line
        axis.ticks = element_line(colour = "black"),  # Set color of tick marks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of tick marks
        axis.ticks.margin = unit(0.5, "cm"),  # Set margin between tick marks and plot area
        axis.title = element_text(family = "Arial", size = 10, color = "black"),  # Set axis title properties
        axis.text = element_text(family = "Arial", size = 10, color = "black"))  +  # Set axis text properties
  coord_cartesian(ylim = c(-0.2, 0.6))  # Set y-axis limits

#### Moon fraction -----------------------------------------------------------------------
d2 <- ggplot(sm_n_bottdepth, aes(y = est, x = fraction)) +
  geom_rug(data = night, mapping = aes(x = fraction), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1) +
  labs(y = NULL,
       x = NULL) +
  theme_minimal() +
  theme(panel.background = element_blank(),  # Remove panel background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"),  # Add axis line
        axis.ticks = element_line(colour = "black"),  # Set color of tick marks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of tick marks
        axis.ticks.margin = unit(0.5, "cm"),  # Set margin between tick marks and plot area
        axis.title = element_text(family = "Arial", size = 10, color = "black"),  # Set axis title properties
        axis.text = element_text(family = "Arial", size = 10, color = "black"))  +  # Set axis text properties
  coord_cartesian(ylim = c(-0.06, 0.06))  # Set y-axis limits

### Dive time------------------------------------------------------------------
# evaluate the smooths
sm_n_divetim <- smooth_estimates(n_divetim) %>%
  add_confint()



#### Temperature -----------------------------------------------------------------------
e2 <- ggplot(sm_n_divetim, aes(y = est, x = min)) +
  geom_rug(data = night, mapping = aes(x = min), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1) +
  labs(y = "Dive Duration (Partial Effect)",
       x = "Temperature (°C)") +
  theme_minimal() +
  theme(panel.background = element_blank(),  # Remove panel background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"),  # Add axis line
        axis.ticks = element_line(colour = "black"),  # Set color of tick marks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of tick marks
        axis.ticks.margin = unit(0.5, "cm"),  # Set margin between tick marks and plot area
        axis.title = element_text(family = "Arial", size = 10, color = "black"),  # Set axis title properties
        axis.text = element_text(family = "Arial", size = 10, color = "black"))  +  # Set axis text properties
  coord_cartesian(ylim = c(-1.5, 3.5))  # Set y-axis limits

#### Moon fraction -----------------------------------------------------------------------
f2 <- ggplot(sm_n_divetim, aes(y = est, x = fraction)) +
  geom_rug(data = night, mapping = aes(x = fraction), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1) +
  labs(y = "Partial Effect",
       x = "Fraction of moon illuminated") +
  theme_minimal() +
  theme(panel.background = element_blank(),  # Remove panel background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"),  # Add axis line
        axis.ticks = element_line(colour = "black"),  # Set color of tick marks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of tick marks
        axis.ticks.margin = unit(0.5, "cm"),  # Set margin between tick marks and plot area
        axis.title = element_text(family = "Arial", size = 10, color = "black"),  # Set axis title properties
        axis.text = element_text(family = "Arial", size = 10, color = "black"))  +  # Set axis text properties
  coord_cartesian(ylim = c(-1, 1))  # Set y-axis limits

### Plot ----------------------------------------------------------------------
# Combine all plots into one figure
night <- ( a2 | b2 | c2 | d2 | e2 | f2 ) +
  plot_layout(ncol = 2, nrow = 3)  # Arrange plots in two columns

# Display the combined plot
night

ggsave("figure 8.tiff", 
       plot = night, width = 8.3, height = 10, units = "in", dpi = 1000)


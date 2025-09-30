## PREP ========================================================================
library(suncalc); library(dplyr); library(lubridate)
setwd("RDA_files")
load("horizontal04.RDA")

## CONVERT TO PACIFIC/AUCKLAND TIME  ===========================================
# Change datetime column to Pacific/Auckland time zone (NZST/NZDT)
manta_df$datetimenz <- with_tz(manta_df$datetime, tz = "Pacific/Auckland")

# Suncalc package requires the following: 
manta_df$datenz <- as.Date(manta_df$datetimenz, tz = "Pacific/Auckland")
manta_df <- manta_df %>% relocate(datenz, .after=datetimenz)

## GET SUNLIGHT TIMES ==========================================================
# Collect sunlight times for all dates using the mean location of data spread
first_date <- with_tz(as.Date(min(manta_df$datenz)), tz = "Pacific/Auckland")
last_date <- with_tz(as.Date(max(manta_df$datenz)), tz = "Pacific/Auckland")

timezone <- "Pacific/Auckland" # Specify the time zone (Pacific/Auckland)

# Create a sequence of dates with the specified time zone
date_sequence <- seq(from = as.POSIXct(first_date, tz = timezone),
                     to = as.POSIXct(last_date, tz = timezone),
                     by = "days")

# Create a data frame with the dates
date_df <- data.frame(Date = date_sequence)
date_df$lon <- 175.25; date_df$lat=-36.5 #Set mean geolocation
date_df$date <- as.Date(date_df$Date)
date_df <- subset(date_df, select = -Date)

# Get sunlight times for each date
suntimes <- getSunlightTimes(data = date_df, 
                             tz = "Pacific/Auckland",
                             keep = c("sunrise", "sunset"))
suntimes$datenz <- as.Date(suntimes$sunrise, tz= "Pacific/Auckland")

#Append the correct sunlight times:
manta_df <- merge(manta_df, suntimes, by.x = "datenz", by.y = "datenz", all.x = TRUE)

## CLASSIFY NIGHT/DAY ==========================================================
#Create new column labelling each data point as the day period
manta_df$period <- rep(" ", length.out = nrow(manta_df))

# Assign labels based on time ranges
manta_df$period[manta_df$datetime > manta_df$sunrise & manta_df$datetime < manta_df$sunset] <- "day"
manta_df$period[manta_df$period == " "] <- "night"


## CLEAN DATA TO EXPORT ========================================================
manta_df <- manta_df %>%
  dplyr::select(-sunrise, -sunset, -date.y, -lat.y, -lon.y) #Remove extra columns
manta_df <- rename(manta_df, 
                    lat = lat.x, lon = lon.x) # Rename
manta_df <- manta_df %>% relocate(period, .after=datetimenz)

save(manta_df, file = "horizontal05.RDA")


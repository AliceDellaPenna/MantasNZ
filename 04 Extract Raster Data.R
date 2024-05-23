## PREP ========================================================================
# Load required libraries
library(raster); library(ncdf4)

setwd("RDA_files") #Set working directory
load("horizontal03.RDA")

# Create a layername column to match to the rasterstack 
manta_df$date <- as.Date(manta_df$datetime) # Convert datetime to Date object
manta_df$layername <- format(manta_df$date, "%Y.%m.%d") # Convert date to the desired format in manta_df
manta_df$layername <- paste0("X", manta_df$layername) # Add the 'X' prefix

# Second layer name for sst
manta_df$layername2 <- format(manta_df$datetime, "X%Y.%m.%d.%H")
manta_df$layername2 <- as.character(manta_df$layername2)

## KD490_23 ====================================================================
# As KD490 was extracted for two different samples the data set has two different spatial extents
# Therefore cannot be stacked together - so do as two seperate extractions: 

# Set your working directory
setwd("kd490_23")

# List the files in the .nc folder 
kd.list_23 = list.files("kd490_23", 
                     pattern = '.nc$', all.files=TRUE,full.names = FALSE)

# Import all .nc files in the folder - lapply with raster so we import as raster layers 
# output should be a list item with each list item being a raster layer 
kdrasters_23 <- lapply(kd.list_23, raster)

# Stack the rasters into a list (this will convert from list --> raster brick)
# only if you have multiple layers 
kdrasters_23 <- stack(kdrasters_23)

# Rename the rasters to correspond with the correct date. 
# Extract dates from raster layer data
dates <- sapply(1:nlayers(kdrasters_23), function(i) {
  date <- as.character(kdrasters_23@layers[[i]]@z[[1]])
  return(date)
})

# Assign the extracted dates to the layer names
names(kdrasters_23) <- format(as.Date(dates, format = "%Y-%m-%d"), "%Y-%m-%d")
names(kdrasters_23) # Check the assigned names

# Extracting the raster value within the chlraster at longitude and latitude that matches the layername 
manta_df$kd490 <- raster::extract(kdrasters_23, cbind(manta_df$lon, manta_df$lat))[
  cbind(1:nrow(manta_df),match(manta_df$layername, names(kdrasters_23)))
]

## KD490_24=====================================================================
# Set your working directory
setwd("kd490_24")

# List the files in the .nc folder 
kd.list_24 = list.files("kd490_24", 
                        pattern = '.nc$', all.files=TRUE,full.names = FALSE)

# Import all .nc files in the folder - lapply with raster so we import as raster layers 
# output should be a list item with each list item being a raster layer 
kdrasters_24 <- lapply(kd.list_24, raster)

# Stack the rasters into a list (this will convert from list --> raster brick)
# only if you have multiple layers 
kdrasters_24 <- stack(kdrasters_24)

# Rename the rasters to correspond with the correct date. 
# Extract dates from raster layer data
dates <- sapply(1:nlayers(kdrasters_24), function(i) {
  date <- as.character(kdrasters_24@layers[[i]]@z[[1]])
  return(date)
})

# Assign the extracted dates to the layer names
names(kdrasters_24) <- format(as.Date(dates, format = "%Y-%m-%d"), "%Y-%m-%d")
names(kdrasters_24) # Check the assigned names

# Extracting the raster values within the kdrasters_24 at longitude and latitude that match the layername 
new_values <- raster::extract(kdrasters_24, cbind(manta_df$lon, manta_df$lat))[
  cbind(1:nrow(manta_df), match(manta_df$layername, names(kdrasters_24)))
]

# Only update the NaN values in manta_df$kd490 with the new values
manta_df$kd490[is.na(manta_df$kd490)] <- new_values[is.na(manta_df$kd490)]


## BATHY =======================================================================
# As bathymetry data is higher res for in the gulf, but we were unable to obtain this for the wider gulf region
# Prioritise the high res dataset, and fill in with lower res where it is not covered

# Read the TIF file as a raster layer
bathy_raster <- raster("Hauraki_Bathymetry_20m.tif")

# Check the raster layer format
names(bathy_raster)
head(bathy_raster)
# Change variables name 
names(bathy_raster)[1] <- 'bathy'
# whenever you want to check one of your layers you can plot it like this and make sure it makes sense or looks the way you want it to! 
plot(bathy_raster[[1]])

# Extract bathymetric values for each GPS point
manta_df$bathy <- raster::extract(bathy_raster, cbind(manta_df$lon, manta_df$lat))

### Prepare bathymetry data for out of gulf ------------------------------------
bathyoog <- raster("Gebco_Bathymetry.tif")

# Check the raster layer format
names(bathyoog)
# Change variables name 
names(bathyoog)[1] <- 'bathy'
# whenever you want to check one of your layers you can plot it like this and make sure it makes sense or looks the way you want it to! 
plot(bathyoog[[1]])

### Extract bathymetry data for out of gulf ------------------------------------
manta_df$bathyoog <- raster::extract(bathyoog, cbind(manta_df$lon, manta_df$lat))
# Identify rows with conditions (NA or 0 bathy values)
replace_indices <- is.na(manta_df$bathy) | manta_df$bathy == 0
# Replace mantatest$bathy values with mantatest$bathyoog values where conditions are met
manta_df$bathy[replace_indices] <- manta_df$bathyoog[replace_indices]
# Remove the mantatest$bathyoog column if no longer needed
manta_df$bathyoog <- NULL
# Set bathymetry values over 1 to 0 (Because the bathymetry raster is only to 20m)
manta_df$bathy[manta_df$bathy > 1] <- 0

## SST_23 ======================================================================
# Set your working directory
setwd("sst_23")

# List the files in the .nc folder
sst.list <- list.files("sst_23", pattern = '.nc$', full.names = TRUE)

# Initialize an empty list to store the individual raster layers
sstrasters <- list()

# Iterate through the list of file paths and read each file with the specified variable name as a raster layer
for (file_path in sst.list) {
  raster_data <- raster::raster(file_path, varname = "SST")
  sstrasters[[file_path]] <- raster_data
}

# Stack the rasters into a list (this will convert from list --> raster brick)
# only if you have multiple layers 
sstrasters <- stack(sstrasters)

# Rename the rasters to correspond with the correct date. 
# Extract dates from raster layer data
dates <- sapply(1:nlayers(sstrasters), function(i) {
  date <- as.character(sstrasters@layers[[i]]@z[[1]])
  return(date)
})

# Assign the extracted dates to the layer names
names(sstrasters) <- as.character(dates)
names(sstrasters) <- substr(names(sstrasters), 1, nchar(names(sstrasters)) - 6) # Remove extra characters
names(sstrasters)
plot(sstrasters[[107]]) # Plot layers here to check data

### Average data over six hours ------------------------------------------------
# SST is collected every hour - 
# aggregate data into six hour periods so that more locations return data points. 
###

# Define indices to aggregate data
hours <- as.numeric(substr(names(sstrasters), 13, 14)) # Extract hours from raster layer names
desired_break_hours <- c(6, 12, 18, 00) # Define the desired break hours
breaks <- c(0, which(hours %in% desired_break_hours), length(hours)) # Find the breaks in hours

# Create indices grouping layers into intervals based on breaks
indices <- rep(seq_along(breaks[-length(breaks)]), diff(breaks))

# Perform aggregation using stackApply with breaks
sstmean <- stackApply(sstrasters, indices, fun = mean, na.rm = TRUE)
sstmean_stack <- stack(sstmean)

# Rename the layers based on the last layer of each group
original_dates <- names(sstrasters)  # Extract the dates from the original sstrasters stack
layer_indices <- breaks[-length(breaks)] 
layer_names <- original_dates[layer_indices]

names(sstmean) <- layer_names
names(sstmean)

plot(sstmean[[821]]) # Plot layers here to check data

### Extracting the data --------------------------------------------------------
# Extract raster value within the sstrasterstack 
# at longitude and latitude that matches the layername 
manta_df$sst <- raster::extract(sstmean, cbind(manta_df$lon, manta_df$lat))[
  cbind(1:nrow(manta_df),match(manta_df$layername2, names(sstmean)))
]

## SST_24 ======================================================================
# Set your working directory
setwd("sst_24")

# List the files in the .nc folder
sst.list <- list.files("sst_24", pattern = '.nc$', full.names = TRUE)

# Initialize an empty list to store the individual raster layers
sstrasters <- list()

# Iterate through the list of file paths and read each file with the specified variable name as a raster layer
for (file_path in sst.list) {
  raster_data <- raster::raster(file_path, varname = "SST")
  sstrasters[[file_path]] <- raster_data
}

# Stack the rasters into a list (this will convert from list --> raster brick)
# only if you have multiple layers 
sstrasters <- stack(sstrasters)

# Rename the rasters to correspond with the correct date. 
# Extract dates from raster layer data
dates <- sapply(1:nlayers(sstrasters), function(i) {
  date <- as.character(sstrasters@layers[[i]]@z[[1]])
  return(date)
})

# Assign the extracted dates to the layer names
names(sstrasters) <- as.character(dates)
names(sstrasters) <- substr(names(sstrasters), 1, nchar(names(sstrasters)) - 6) # Remove extra characters
names(sstrasters)

plot(sstrasters[[540]]) # Plot layers here to check data

### Average data over six hours ------------------------------------------------
# SST is collected every hour - 
# aggregate data into six hour periods so that more locations return data points. 
###

# Define indices to aggregate data
hours <- as.numeric(substr(names(sstrasters), 13, 14)) # Extract hours from raster layer names
desired_break_hours <- c(6, 12, 18, 00) # Define the desired break hours
breaks <- c(0, which(hours %in% desired_break_hours), length(hours)) # Find the breaks in hours

# Create indices grouping layers into intervals based on breaks
indices <- rep(seq_along(breaks[-length(breaks)]), diff(breaks))

# Perform aggregation using stackApply with breaks
sstmean <- stackApply(sstrasters, indices, fun = mean, na.rm = TRUE)
sstmean_stack <- stack(sstmean)

# Rename the layers based on the last layer of each group
original_dates <- names(sstrasters)  # Extract the dates from the original sstrasters stack
layer_indices <- breaks[-length(breaks)] 
layer_names <- original_dates[layer_indices]

names(sstmean) <- layer_names
names(sstmean)

plot(sstmean[[90]]) # Plot layers here to check data

### Extracting the data --------------------------------------------------------
# Extracting the raster values within the sstmean_stack at longitude and latitude that match the layername 

# Extract SST values from sst_24
sst_24_values <- raster::extract(sstmean, cbind(manta_df$lon, manta_df$lat))[
  cbind(1:nrow(manta_df), match(manta_df$layername2, names(sstmean)))
]

# Update sst column only where sst_24 values are not NA
manta_df$sst <- ifelse(!is.na(sst_24_values), sst_24_values, manta_df$sst)

# Convert from Kelvin to Celsius
manta_df$sst <- manta_df$sst - 273.15

## SAVE FILE ===================================================================
setwd("RDA_files") #Set working directory to save RDA file
save(manta_df, file = "horizontal04.RDA")

library(raster); library(ncdf4)

## PREP ========================================================================
setwd("workingdir") #Set working directory
load("horizontal03.RDA")

# Create a layername column to match to the rasterstack 
manta_df$date <- as.Date(manta_df$datetime) # Convert datetime to Date object
manta_df$layername <- format(manta_df$date, "%Y.%m.%d") # Convert date to the desired format in manta_df
manta_df$layername <- paste0("X", manta_df$layername) # Add the 'X' prefix

# Second layer name for sst
manta_df$layername2 <- format(manta_df$datetime, "X%Y.%m.%d.%H")
manta_df$layername2 <- as.character(manta_df$layername2)

## KD490 =======================================================================
# List the files in the .nc folder 
kd.list = list.files("kd490", 
                     pattern = '.nc$', all.files=TRUE,full.names = FALSE)

# Import all .nc files in the folder - lapply with raster so we import as raster layers 
# output should be a list item with each list item being a raster layer 
kdrasters <- lapply(kd.list, raster)

# Stack the rasters into a list (this will convert from list --> raster brick)
# only if you have multiple layers 
kdrasters <- stack(kdrasters)

# Rename the rasters to correspond with the correct date. 
# Extract dates from raster layer data
dates <- sapply(1:nlayers(kdrasters), function(i) {
  date <- as.character(kdrasters@layers[[i]]@z[[1]])
  return(date)
})

# Assign the extracted dates to the layer names
names(kdrasters) <- format(as.Date(dates, format = "%Y-%m-%d"), "%Y-%m-%d")
names(kdrasters) # Check the assigned names

# Extracting the raster value within the chlraster at longitude and latitude that matches the layername 
manta_df$kd490 <- raster::extract(kdrasters, cbind(manta_df$lon, manta_df$lat))[
  cbind(1:nrow(manta_df),match(manta_df$layername, names(kdrasters)))
]



## BATHYMETRY ==================================================================
# First using the higher qulity Hauraki Gulf raster: 

# Define the file path
file_path <- "Hauraki_Bathymetry_20m.tif"
# Read the TIF file as a raster layer
bathy_raster <- raster(file_path)

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
bathyoog <- raster("GEBCO_Bathymetry_15_arc-second.tif")

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

## SST =========================================================================
# List the files in the .nc folder
sst.list <- list.files("SST", pattern = '.nc$', full.names = TRUE)

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
plot(sstmean[[838]]) # Plot layers here to check data

### Extracting the data --------------------------------------------------------
# Extract raster value within the sstrasterstack 
# at longitude and latitude that matches the layername 
manta_df$sst <- raster::extract(sstmean, cbind(manta_df$lon, manta_df$lat))[
  cbind(1:nrow(manta_df),match(manta_df$layername2, names(sstmean)))
]

# Convert from Kelvin to Celsius
manta_df$sst <- manta_df$sst - 273.15

## SAVE FILE ===================================================================
save(manta_df, file = "horizontal04.RDA")

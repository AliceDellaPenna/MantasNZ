## PREP ========================================================================
# Load in packages
library(aniMotum); library(ggplot2); library(dplyr); library(plotly); library(viridis)
library(rnaturalearth); library(rnaturalearthdata); library(rnaturalearthhires)
library(dplyr); library(pathroutr)
library(sf)

load("data")

## STATE SPACE MODELLING =======================================================
# Fitting the move persistence model to each tag individually
# This assumes that the movement of individuals is unique

ssm_252520 <- 
  fit_ssm(df_252520, # Filtered to just look at one tag
          model = "mp", # Move persistence state space model
          time.step = 6, # Six hour time step
          spdf = FALSE,
          map = list(rho_o = factor(NA)),
          control = ssm_control(verbose = 0))

ssm_252525 <- 
  fit_ssm(df_252525, # Filtered to just look at one tag
          model = "mp", # Move persistence state space model
          time.step = 6, # Six hour time step
          spdf = FALSE,
          map = list(rho_o = factor(NA)),
          control = ssm_control(verbose = 0))

ssm_252778 <- 
  fit_ssm(df_252778, # Filtered to just look at one tag
          model = "mp", # Move persistence state space model
          time.step = 6, # Six hour time step
          spdf = FALSE,
          map = list(rho_o = factor(NA)),
          control = ssm_control(verbose = 0))


## EXTRACTING THE DATA =========================================================
# Grab the data from each model to produce data frame
ssm_252520_df <- grab(ssm_252520, what = "predicted", as_sf = FALSE)
ssm_252525_df <- grab(ssm_252525, what = "predicted", as_sf = FALSE)
ssm_252778_df <- grab(ssm_252778, what = "predicted", as_sf = FALSE)

# Bind the dataframes together
combined_df <- bind_rows(
  ssm_252520_df ,
  ssm_252525_df,
  ssm_252778_df,
)


## REROUTING THE DATA ==========================================================
# For when wrapper function in aniMotum package doesn't work due to complexity of study area
# Code replicated from Harbour Seal Vignette to reroute data around land (London)

# Prep -------------------------------------------------------------------------
# Read in the NZ coastlines shapefile
nzcoast <- sf::read_sf("/Users/tamsin/Desktop/Thesis/QGIS/Shapefiles/NZ Coastlines and Islands/nz-coastlines-and-islands-polygons-4326.shp")
nzcoast <- sf::st_make_valid(nzcoast)

# Convert the data frame to an sf object
ssm_manta_sf <- st_as_sf(combined_df, coords = c("lon", "lat"), crs = st_crs(nzcoast))

# Transform ssm_manta_sf to NZTM (EPSG 2193)
ssm_manta_sf <- st_transform(ssm_manta_sf, crs = 2193)
# Transform nzcoast to NZTM (EPSG 2193)
nzcoast <- st_transform(nzcoast, crs = 2193)

# Plot and check the data ------------------------------------------------------
# Create separate line strings per id
lines <- ssm_manta_sf %>%
  filter(!sf::st_is_empty(.)) %>%
  group_by(id) %>%
  summarise(do_union = FALSE) %>%
  sf::st_cast('LINESTRING')

# Plot the data
ggplot() +
  ggspatial::annotation_spatial(nzcoast, fill = "cornsilk3", size = 0) +
  ggspatial::layer_spatial(lines, color = "darkgrey", size = 0.5) +
  ggspatial::layer_spatial(ssm_manta_sf, aes(color = as.factor(id)), size = 0.5) +
  theme_void()

# Limit polygon to area of interest --------------------------------------------
# Limit the barrier polygon to your region of interest
land_region <- sf::st_buffer(ssm_manta_sf, dist = 1000) %>%  # double check this cause used to be 10000
  sf::st_union() %>% 
  sf::st_convex_hull() %>% 
  sf::st_intersection(nzcoast) %>% 
  st_collection_extract('POLYGON') %>% 
  st_sf()

# Create Visibility graph
vis_graph <- prt_visgraph(land_region, buffer = 1000)
# Transform vis_graph to the correct CRS (EPSG 2193)
vis_graph <- st_transform(vis_graph, crs = 2193)

# Plot Visibility graph
vis_graph_sf <- sfnetworks::activate(vis_graph,"edges") %>% sf::st_as_sf()
ggplot() +
  ggspatial::annotation_spatial(land_region, fill = "cornsilk3", size = 0) +
  ggspatial::layer_spatial(vis_graph_sf, size = 0.5) +
  theme_void()

# Determine sections of track on land ------------------------------------------
track_pts <- ssm_manta_sf %>% dplyr::filter(!sf::st_is_empty(.))
segs_tbl <- get_barrier_segments(track_pts,land_region)
segs_tbl

# Determine rerouted track with shortest path ----------------------------------
segs_tbl <- segs_tbl %>% prt_shortpath(vis_graph)

ggplot() + 
  ggspatial::annotation_spatial(land_region, fill = "cornsilk3", size = 0) +
  ggspatial::layer_spatial(segs_tbl$geometry, color = "deepskyblue3") +
  theme_void()

# Visualise the rerouted tracks ------------------------------------------------
manta_rerouted <- prt_reroute(track_pts, land_region, vis_graph)
manta_rerouted <- prt_update_points(manta_rerouted, track_pts)

# Split the track_pts into separate line strings per id
lines <- manta_rerouted %>%
  filter(!st_is_empty(.)) %>%
  group_by(id) %>%
  summarise(do_union = FALSE) %>%
  st_cast('LINESTRING')

# Plot the data with separate line strings per id
ggplot() +
  ggspatial::annotation_spatial(land_region, fill = "cornsilk3", size = 0) +
  ggspatial::layer_spatial(lines, aes(color = as.factor(id)), size = 0.5) +
  ggspatial::layer_spatial(manta_rerouted, aes(color = as.factor(id)), size = 0.5) +
  theme_void()
ggplotly() #To visualise the data if you want to have a closer look

# Update original geometry with rerouted coordinates ---------------------------
# Transform the rerouted_sf back to EPSG 4326
rerouted_sf <- st_transform(manta_rerouted, crs = 4326)

# Plot the rerouted_sf data
ggplot() +
  ggspatial::annotation_spatial(nzcoast, fill = "cornsilk3", size = 0) +
  ggspatial::layer_spatial(rerouted_sf, aes(color = as.factor(id)), size = 0.5) +
  theme_void()

# Convert the sf object to a data frame
rerouted_df <- as.data.frame(rerouted_sf)
rerouted_df$lon <- st_coordinates(rerouted_sf)[, "X"]
rerouted_df$lat <- st_coordinates(rerouted_sf)[, "Y"]

# Remove extra columns 
names(rerouted_df)
columns_to_keep <- c("id","date", "lon", "lat") # Specify the columns to keep
rerouted_df <- rerouted_df[, columns_to_keep] # Keep those columns, remove everything else

# Rename the columns
rerouted_df <- rerouted_df %>%
  rename(datetime = date)


## MOVE PERSISTENCE ============================================================
# Running the model again to get it in the right format for inferring move persistence
# Just use the fitted model so that it does not alter the previously estimated locations

rerouted_df$datetime <- as.POSIXct(rerouted_df$datetime, tz = "UTC") # Set datetime class to POSIXct
rerouted_df$id <- as.character(rerouted_df$id)

# Run the move persistence model
# Infer move persistence
manta_mp <- fit_mpm(rerouted_df,
                    what = "fitted",
                    model = "mpm")
plot(manta_mp, 
     what = "fitted", 
     type = 2,
     normalise = FALSE)

# Extract the data for further analysis
manta_mp_df <- grab(manta_mp, what = "fitted", as_sf = FALSE, normalise = TRUE, group = FALSE)

# Join the two data frames so that we have coordinates and move persistence (g) together
manta_mp_df <- manta_mp_df %>%
  rename(datetime = date)
manta_df <- left_join(rerouted_df, manta_mp_df %>% dplyr::select(datetime, id, g), by = c("datetime", "id"))




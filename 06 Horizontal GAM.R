# AIM:
# To assess the influence of environmental variables on the probability of 
# an animal being in ARS state 
#
# Running separate GAM models for day versus night
#
## PREP ========================================================================
# Load in packages
library(mgcv); library(corrplot); library(fitdistrplus); library(readr); library(ggplot2); library(dplyr)
library(patchwork); library(gratia)

# Load in the data
setwd("RDA_files")
load("horizontal05.RDA")
manta_df$g <- 1 - manta_df$g # Transform g so it is 1-g so that we are looking at predictors of foraging rather than of travelling

names(manta_df)
manta_df$id[manta_df$id == "204511"] <- "9"
manta_df$id[manta_df$id == "238014"] <- "14"
manta_df$id[manta_df$id == "238016"] <- "16"
manta_df$id[manta_df$id == "238018"] <- "17"
manta_df$id[manta_df$id == "238019"] <- "18"
manta_df$id[manta_df$id == "252520"] <- "19"
manta_df$id[manta_df$id == "252522"] <- "20"
manta_df$id[manta_df$id == "252524"] <- "21"
manta_df$id[manta_df$id == "252525"] <- "22"
manta_df$id[manta_df$id == "252526"] <- "23"
manta_df$id[manta_df$id == "252528"] <- "24"
manta_df$id[manta_df$id == "252778"] <- "25"
manta_df$id <- as.character(manta_df$id); class(manta_df$id) #Check id is the right class


# Add year as a categorical variable
manta_df$year<- substr(manta_df$datetime, 1, 4)
manta_df$year <- as.character(manta_df$year); class(manta_df$year) #Check id is the right class (chr)

# Add sex as a categorical value
manta_df$sex <- ifelse(manta_df$id %in% c(14, 17, 19, 20, 21, 23, 24, 25 ), "Female",
                       ifelse(manta_df$id %in% c(9, 15, 16, 18, 22), "Male", NA))
manta_df$sex <- as.character(manta_df$sex); class(manta_df$sex) #Check sex is the right class (chr)

## FINAL GAM (DAY) =============================================================
# Filter the gam to just look at locations during the day
gam_d_df <- manta_df %>% filter(period == "day")

gam_d <- gam(g ~
              s(wind_direction, bs = 'cc', k = -1) +
              s(wind_speed, k= -1) +
              s(altitude, k = -1) + 
              s(kd490, k = -1) +
              s(bathy, k = -1) +
              s(sst, k = 6) +
              factor(tide_category) +
              factor(id) + 
              factor(sex) + 
              factor(year),
            data = gam_d_df,
            method = "REML",
            family = betar (link = "logit", eps=0.0000000001))

summary(gam_d)
plot(gam_d, all.terms = TRUE, pages = 1, scale = 0, shade = 1)


## ===== FINAL GAM (NIGHT) =====================================================
# Filter the gam to just look at locations during the night
gam_n_df <- manta_df %>% filter(period == "night")

gam_n <- gam(g ~
               s(wind_direction, bs = 'cc', k = -1) +
               s(wind_speed, k = -1) +
               s(fraction, k = 6) + 
               s(kd490, k = -1) +
               s(bathy, k = -1) +
               s(sst, k = -1) +
               factor(tide_category) +
               factor(id) + 
               factor(sex) +
               factor(year),
             data = gam_n_df,
             method = "REML",
             family = betar (link = "logit", eps=0.0000000001))

summary(gam_n)
plot(gam_n, all.terms = TRUE, pages = 1, scale = 0, shade = 1)


## ===== VISUALISING GAM (DAY) =================================================
# evaluate the smooths
sm_d <- smooth_estimates(gam_d) %>%
  add_confint()

# Extract parametric effect
param_effects_d <- parametric_effects(gam_d)
tide_category_effects_d <- subset(param_effects_d, term == "factor(tide_category)")
id_effects_d <- subset(param_effects_d, term == "factor(id)")
year_effects_d <- subset(param_effects_d, term == "factor(year)")
sex_effects_d <- subset(param_effects_d, term == "factor(sex)")

### BATHY ------------------------------------------------------------------------
# Plot smooth component estimates with confidence intervals using ggplot2
a <- ggplot(sm_d, aes(y = est, x = bathy)) +
  geom_rug(data = gam_d_df, mapping = aes(x = bathy), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", linewidth = 1.5) +
  labs(y = "Partial effect",
       x = "Bathymetry (m)") +
  scale_x_continuous(limits = c(-3000, 0)) +  # Set x-axis limits, remove negative sign, and add x labels every 50m
  theme_minimal() +
  theme(panel.background = element_blank(),  # Remove panel background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"),  # Add axis line
        axis.ticks = element_line(colour = "black"),  # Set color of tick marks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of tick marks
        axis.ticks.margin = unit(0.5, "cm"),  # Set margin between tick marks and plot area
        axis.title = element_text(family = "Arial", size = 10, color = "black"),  # Set axis title properties
        axis.text = element_text(family = "Arial", size = 10, color = "black")) + # Set axis text properties 
  coord_cartesian(ylim = c(-2.5, 1))  # Set y-axis limits

### KD490 ----------------------------------------------------------------------
# Plot smooth component estimates with confidence intervals using ggplot2
b <- ggplot(sm_d, aes(y = est, x = kd490)) +
  geom_rug(data = gam_d_df, mapping = aes(x = kd490), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1.5) +
  labs(y = NULL,
       x = "kd490 (nm)") +
  theme_minimal() +
  theme(panel.background = element_blank(),  # Remove panel background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"),  # Add axis line
        axis.ticks = element_line(colour = "black"),  # Set color of tick marks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of tick marks
        axis.ticks.margin = unit(0.5, "cm"),  # Set margin between tick marks and plot area
        axis.title = element_text(family = "Arial", size = 10, color = "black"),  # Set axis title properties
        axis.text = element_text(family = "Arial", size = 10, color = "black")) + # Set axis text properties
  scale_x_continuous(limits = c(0.02, 0.08)) +
  coord_cartesian(ylim = c(-0.8, 0.8))  # Set y-axis limits

### SST ------------------------------------------------------------------------
# Plot smooth component estimates with confidence intervals using ggplot2
# Your ggplot code
c <- ggplot(sm_d, aes(y = est, x = sst)) +
  geom_rug(data = gam_d_df, mapping = aes(x = sst), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1.5) +
  labs(y = "Partial effect",
       x = "Sea surface temperature (°C)") +
 scale_x_continuous(limits = c(18, 25)) +
  theme_minimal() +
  theme(panel.background = element_blank(),  # Remove panel background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"),  # Add axis line
        axis.ticks = element_line(colour = "black"),  # Set color of tick marks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of tick marks
        axis.ticks.margin = unit(0.5, "cm"),  # Set margin between tick marks and plot area
        axis.title = element_text(family = "Arial", size = 10, color = "black"),  # Set axis title properties
        axis.text = element_text(family = "Arial", size = 10, color = "black"))  + # Set axis text properties
  coord_cartesian(ylim = c(-2, 1))  # Set y-axis limits

### WindDir ----------------------------------------------------------------------
# Plot smooth component estimates with confidence intervals using ggplot2
# Your ggplot code
d <- ggplot(sm_d, aes(y = est, x = wind_direction)) +
  geom_rug(data = gam_d_df, mapping = aes(x = wind_direction), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1.5) +
  labs(y = NULL,
       x = "Wind direction (°)") +
  theme_minimal() +
  theme(panel.background = element_blank(),  # Remove panel background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"),  # Add axis line
        axis.ticks = element_line(colour = "black"),  # Set color of tick marks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of tick marks
        axis.ticks.margin = unit(0.5, "cm"),  # Set margin between tick marks and plot area
        axis.title = element_text(family = "Arial", size = 10, color = "black"),  # Set axis title properties
        axis.text = element_text(family = "Arial", size = 10, color = "black")) +  # Set axis text properties
  coord_cartesian(ylim = c(-0.3, 0.3))  # Set y-axis limits


### WindSpeed --------------------------------------------------------------------
# Plot smooth component estimates with confidence intervals using ggplot2
# Your ggplot code
e <- ggplot(sm_d, aes(y = est, x = wind_speed)) +
  geom_rug(data = gam_d_df, mapping = aes(x = wind_speed), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1.5) +
  labs(y = "Partial effect",
       x = expression("Wind speed (ms"^{-1} * ")")) +  # Include (ms^-1) in brackets
  scale_x_continuous(limits = c(0, 20)) +
  theme_minimal() +
  theme(panel.background = element_blank(),  # Remove panel background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"),  # Add axis line
        axis.ticks = element_line(colour = "black"),  # Set color of tick marks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of tick marks
        axis.ticks.margin = unit(0.5, "cm"),  # Set margin between tick marks and plot area
        axis.title = element_text(family = "Arial", size = 10, color = "black"),  # Set axis title properties
        axis.text = element_text(family = "Arial", size = 10, color = "black")) + # Set axis text properties
  coord_cartesian(ylim = c(-0.8, 0.8))  # Set y-axis limits

### Altitude -------------------------------------------------------------------
# Plot smooth component estimates with confidence intervals using ggplot2
# Your ggplot code
f <-ggplot(sm_d, aes(y = est, x = altitude)) +
  geom_rug(data = gam_d_df, mapping = aes(x = altitude), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1.5) +
  labs(y = NULL,
       x = "Solar altitude (°)") +
  theme_minimal() +
  theme(panel.background = element_blank(),  # Remove panel background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"),  # Add axis line
        axis.ticks = element_line(colour = "black"),  # Set color of tick marks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of tick marks
        axis.ticks.margin = unit(0.5, "cm"),  # Set margin between tick marks and plot area
        axis.title = element_text(family = "Arial", size = 10, color = "black"),  # Set axis title properties
        axis.text = element_text(family = "Arial", size = 10, color = "black"))  + # Set axis text properties
  coord_cartesian(ylim = c(-1, 0.5))  # Set y-axis limits

### Tide -----------------------------------------------------------------------
# Define custom labels
tide_labels <- c("falling" = "Falling", "rising" = "Rising", "slack" = "Slack")
# Plot the parametric effects with circles and error bars
g <- ggplot(tide_category_effects_d, aes(x = level, y = partial)) +
  geom_errorbar(aes(ymin = partial - se, ymax = partial + se), width = 0.2, size = 0.5) +  # Error bars
  geom_point(shape = 21, size = 3, fill = "black") +  # Circle for the partial effect
  labs(x = "Tide category", y = "Partial Effect") +
  scale_x_discrete(labels = tide_labels) +  # Set custom labels
  coord_cartesian(ylim = c(-1, 1)) +  # Adjust y-axis limits
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),  # Remove panel background
        axis.line = element_line(color = "black"),  # Add axis lines
        axis.text = element_text(size = 10, color = "black", family = "Arial"),  # Adjust axis text properties
        axis.title = element_text(size = 10, color = "black", family = "Arial"),  # Adjust axis title properties
        axis.ticks = element_line(color = "black", size = 0.5),  # Add axis ticks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of ticks
        axis.ticks.margin = unit(0.5, "cm"))  # Set margin between ticks and plot area

### ID -------------------------------------------------------------------------
# Plot the parametric effects with circles and error bars
# Reorder tag numbers
id_effects_d$level <- factor(id_effects_d$level, levels = c("9","14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25"))

# Create individual effects plot
h <- ggplot(id_effects_d, aes(x = level, y = partial)) +
  geom_errorbar(aes(ymin = partial - se, ymax = partial + se), width = 0.2, size = 0.5) +  # Error bars
  geom_point(fill = "black", shape = 21, size = 3) +  # Circle for the partial effect
  labs(x = "Manta #", y = NULL) +
  coord_cartesian(ylim = c(-3, 3)) +  # Adjust y-axis limits
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),  # Remove panel background
        axis.line = element_line(color = "black"),  # Add axis lines
        axis.text = element_text(size = 10, color = "black", family = "Arial"),  # Adjust axis text properties
        axis.title = element_text(size = 10, color = "black", family = "Arial"),   # Adjust axis title properties
        axis.ticks = element_line(color = "black", size = 0.5),  # Add axis ticks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of ticks
        axis.ticks.margin = unit(0.5, "cm")) +  # Set margin between ticks and plot area
  guides(fill = FALSE) +  # Remove legend
  scale_y_continuous(labels = function(x) sprintf("%.1f", x))  # Format y-axis labels to 1 decimal place

### SEX -------------------------------------------------------------------------
# Plot the parametric effects with circles and error bars
i <- ggplot(sex_effects_d, aes(x = level, y = partial)) +
  geom_errorbar(aes(ymin = partial - se, ymax = partial + se), width = 0.2, size = 0.5) +  # Error bars
  geom_point(fill = "black", shape = 21, size = 3) +  # Circle for the partial effect
  labs(x = "Sex", y = "Partial effect") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),  # Remove panel background
        axis.line = element_line(color = "black"),  # Add axis lines
        axis.text = element_text(size = 10, color = "black", family = "Arial"),  # Adjust axis text properties
        axis.title = element_text(size = 10, color = "black", family = "Arial"),   # Adjust axis title properties
        axis.ticks = element_line(color = "black", size = 0.5),  # Add axis ticks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of ticks
        axis.ticks.margin = unit(0.5, "cm")) +  # Set margin between ticks and plot area
  coord_cartesian(ylim = c(-1, 2)) +  # Adjust y-axis limits
  guides(fill = FALSE)  # Remove legend


### YEAR -------------------------------------------------------------------------
# Plot the parametric effects with circles and error bars
j <- ggplot(year_effects_d, aes(x = level, y = partial)) +
  geom_errorbar(aes(ymin = partial - se, ymax = partial + se), width = 0.2, size = 0.5) +  # Error bars
  geom_point(fill = "black", shape = 21, size = 3) +  # Circle for the partial effect
  labs(x = "Year", y = NULL) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),  # Remove panel background
        axis.line = element_line(color = "black"),  # Add axis lines
        axis.text = element_text(size = 10, color = "black", family = "Arial"),  # Adjust axis text properties
        axis.title = element_text(size = 10, color = "black", family = "Arial"),  # Adjust axis title properties
        axis.ticks = element_line(color = "black", size = 0.5),  # Add axis ticks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of ticks
        axis.ticks.margin = unit(0.5, "cm")) +  # Set margin between ticks and plot area
  coord_cartesian(ylim = c(-1, 1)) +  # Adjust y-axis limits
  guides(fill = FALSE) +  # Remove legend
  scale_y_continuous(labels = function(x) sprintf("%.1f", x))  # Format y-axis labels to 1 decimal place



### PLOTTING FIGURES -----------------------------------------------------------
# Combine all plots into one figure
combined_plot_d <- (a | b | c | d  | e | f | g | h| i | j ) +
  plot_layout(ncol = 2, nrow = 5)  # Arrange plots in two columns

# Display the combined plot
combined_plot_d

ggsave("figure_3.tiff", 
       plot = combined_plot_d, width = 8.3, height = 10, units = "in", dpi = 1000)

setwd("Figures/") #Set working directory to save RDA file
save.image(file = "Final3.RDA")

## ===== VISUALISING GAM (NIGHT) =================================================
# evaluate the smooths
sm_n <- smooth_estimates(gam_n) %>%
  add_confint()

# Extract parametric effect
param_effects_n<- parametric_effects(gam_n)

tide_category_effects_n <- subset(param_effects_n, term == "factor(tide_category)")
id_effects_n <- subset(param_effects_n, term == "factor(id)")
sex_effects_n <- subset(param_effects_n, term == "factor(sex)")
year_effects_n <- subset(param_effects_n, term == "factor(year)")

### BATHY ----------------------------------------------------------------------
a <- ggplot(sm_n, aes(y = est, x = bathy)) +
  geom_rug(data = gam_n_df, mapping = aes(x = bathy), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1.5) +
  labs(y = "Partial effect",
       x = "Bathymetry (m)") +
  scale_x_continuous(limits = c(-3000, 0)) +  # Set x-axis limits, remove negative sign, and add x labels every 50m
  coord_cartesian(ylim = c(-3, 1)) +
  theme_minimal() +
  theme(panel.background = element_blank(),  # Remove panel background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"),  # Add axis line
        axis.ticks = element_line(colour = "black"),  # Set color of tick marks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of tick marks
        axis.ticks.margin = unit(0.5, "cm"),  # Set margin between tick marks and plot area
        axis.title = element_text(family = "Arial", size = 10, color = "black"),  # Set axis title properties
        axis.text = element_text(family = "Arial", size = 10, color = "black"))  # Set axis text properties

### KD490 ----------------------------------------------------------------------
# Plot smooth component estimates with confidence intervals using ggplot2
b <- ggplot(sm_n, aes(y = est, x = kd490)) +
  geom_rug(data = gam_n_df, mapping = aes(x = kd490), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1.5) +
  labs(y = NULL,
       x = "kd490 (nm)") +
  theme_minimal() +
  theme(panel.background = element_blank(),  # Remove panel background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"),  # Add axis line
        axis.ticks = element_line(colour = "black"),  # Set color of tick marks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of tick marks
        axis.ticks.margin = unit(0.5, "cm"),  # Set margin between tick marks and plot area
        axis.title = element_text(family = "Arial", size = 10, color = "black"),  # Set axis title properties
        axis.text = element_text(family = "Arial", size = 10, color = "black"))  +
  scale_x_continuous(limits = c(0.02, 0.08)) +
  coord_cartesian(ylim = c(-1, 1.2))  # Set y-axis limits

### SST ------------------------------------------------------------------------
# Plot smooth component estimates with confidence intervals using ggplot2
# Your ggplot code

c <- ggplot(sm_n, aes(y = est, x = sst)) +
  geom_rug(data = gam_n_df, mapping = aes(x = sst), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1.5) +
  labs(y = "Partial effect",
       x = "Sea surface temperature (°C)") +
  scale_x_continuous(limits = c(18, 25)) +
  theme_minimal() +
  theme(panel.background = element_blank(),  # Remove panel background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"),  # Add axis line
        axis.ticks = element_line(colour = "black"),  # Set color of tick marks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of tick marks
        axis.ticks.margin = unit(0.5, "cm"),  # Set margin between tick marks and plot area
        axis.title = element_text(family = "Arial", size = 10, color = "black"),  # Set axis title properties
        axis.text = element_text(family = "Arial", size = 10, color = "black")) +  # Set axis text properties
  coord_cartesian(ylim = c(-2.5, 1)) +  # Set y-axis limits
  scale_y_continuous(labels = function(x) sprintf("%.1f", x))  # Format y-axis labels to 1 decimal place

### WindDir ----------------------------------------------------------------------
# Plot smooth component estimates with confidence intervals using ggplot2
# Your ggplot code
d <- ggplot(sm_n, aes(y = est, x = wind_direction)) +
  geom_rug(data = gam_n_df, mapping = aes(x = wind_direction), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1.5) +
  labs(y = NULL,
       x = "Wind direction (°)") +
  theme_minimal() +
  theme(panel.background = element_blank(),  # Remove panel background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"),  # Add axis line
        axis.ticks = element_line(colour = "black"),  # Set color of tick marks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of tick marks
        axis.ticks.margin = unit(0.5, "cm"),  # Set margin between tick marks and plot area
        axis.title = element_text(family = "Arial", size = 10, color = "black"),  # Set axis title properties
        axis.text = element_text(family = "Arial", size = 10, color = "black")) +  # Set axis text properties
  coord_cartesian(ylim = c(-0.2, 0.2))  # Set y-axis limits


### WindSpeed --------------------------------------------------------------------
# Plot smooth component estimates with confidence intervals using ggplot2
# Your ggplot code
e <- ggplot(sm_n, aes(y = est, x = wind_speed)) +
  geom_rug(data = gam_n_df, mapping = aes(x = wind_speed), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1.5) +
  labs(y = "Partial effect",
       x = expression("Wind speed (ms"^{-1} * ")")) +  # Include (ms^-1) in brackets
  scale_x_continuous(limits = c(0, 20)) +
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

### MOON FRACTION --------------------------------------------------------------
# Plot smooth component estimates with confidence intervals using ggplot2
# Your ggplot code
f <- ggplot(sm_n, aes(y = est, x = fraction)) +
  geom_rug(data = gam_n_df, mapping = aes(x = fraction), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1.5) +
  labs(y = NULL,
       x = "Fraction of moon illuminated") +
  coord_cartesian(ylim = c(-1, 1)) +  # Adjust y-axis limits
  theme_minimal() +
  theme(panel.background = element_blank(),  # Remove panel background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"),  # Add axis line
        axis.ticks = element_line(colour = "black"),  # Set color of tick marks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of tick marks
        axis.ticks.margin = unit(0.5, "cm"),  # Set margin between tick marks and plot area
        axis.title = element_text(family = "Arial", size = 10, color = "black"),  # Set axis title properties
        axis.text = element_text(family = "Arial", size = 10, color = "black")) +  # Set axis text properties
  scale_y_continuous(labels = function(x) sprintf("%.1f", x))  # Format y-axis labels to 1 decimal place


### TIDE -----------------------------------------------------------------------
# Define custom labels
tide_labels <- c("falling" = "Falling", "rising" = "Rising", "slack" = "Slack")
# Plot the parametric effects with circles and error bars
g <- ggplot(tide_category_effects_n, aes(x = level, y = partial)) +
  geom_errorbar(aes(ymin = partial - se, ymax = partial + se), width = 0.2, size = 0.5) +  # Error bars
  geom_point(shape = 21, size = 3, fill = "black") +  # Circle for the partial effect
  labs(x = "Tide category", y = "Partial Effect") +
  scale_x_discrete(labels = tide_labels) +  # Set custom labels
  coord_cartesian(ylim = c(-1, 1)) +  # Adjust y-axis limits
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),  # Remove panel background
        axis.line = element_line(color = "black"),  # Add axis lines
        axis.text = element_text(size = 10, color = "black", family = "Arial"),  # Adjust axis text properties
        axis.title = element_text(size = 10, color = "black", family = "Arial"),   # Adjust axis title properties
        axis.ticks = element_line(color = "black", size = 0.5),  # Add axis ticks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of ticks
        axis.ticks.margin = unit(0.5, "cm"))  # Set margin between ticks and plot area



### ID -------------------------------------------------------------------------
# Plot the parametric effects with circles and error bars
# Reorder tag numbers
id_effects_d$level <- factor(id_effects_d$level, levels = c("9", "14", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25"))
# Create individual effects plot
h <- ggplot(id_effects_d, aes(x = level, y = partial)) +
  geom_errorbar(aes(ymin = partial - se, ymax = partial + se), width = 0.2, size = 0.5) +  # Error bars
  geom_point(fill = "black", shape = 21, size = 3) +  # Circle for the partial effect
  labs(x = "Manta #", y = NULL) +
  coord_cartesian(ylim = c(-3, 3)) +  # Adjust y-axis limits
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),  # Remove panel background
        axis.line = element_line(color = "black"),  # Add axis lines
        axis.text = element_text(size = 10, color = "black", family = "Arial"),  # Adjust axis text properties
        axis.title = element_text(size = 10, color = "black", family = "Arial"),   # Adjust axis title properties
        axis.ticks = element_line(color = "black", size = 0.5),  # Add axis ticks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of ticks
        axis.ticks.margin = unit(0.5, "cm")) +  # Set margin between ticks and plot area
  guides(fill = FALSE) +  # Remove legend
  scale_y_continuous(labels = function(x) sprintf("%.1f", x))  # Format y-axis labels to 1 decimal place


### SEX -------------------------------------------------------------------------
# Plot the parametric effects with circles and error bars
i <- ggplot(sex_effects_n, aes(x = level, y = partial)) +
  geom_errorbar(aes(ymin = partial - se, ymax = partial + se), width = 0.2, size = 0.5) +  # Error bars
  geom_point(fill = "black", shape = 21, size = 3) +  # Circle for the partial effect
  labs(x = "Sex", y = "Partial effect") +
  coord_cartesian(ylim = c(-1, 1)) +  # Adjust y-axis limits
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),  # Remove panel background
        axis.line = element_line(color = "black"),  # Add axis lines
        axis.text = element_text(size = 10, color = "black", family = "Arial"),  # Adjust axis text properties
        axis.title = element_text(size = 10, color = "black", family = "Arial"),  # Adjust axis title properties
        axis.ticks = element_line(color = "black", size = 0.5),  # Add axis ticks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of ticks
        axis.ticks.margin = unit(0.5, "cm")) +  # Set margin between ticks and plot area
  guides(fill = FALSE)  + # Remove legend
  scale_y_continuous(labels = function(x) sprintf("%.1f", x))  # Format y-axis labels to 1 decimal place

### YEAR -------------------------------------------------------------------------
# Plot the parametric effects with circles and error bars
j <- ggplot(year_effects_n, aes(x = level, y = partial)) +
  geom_errorbar(aes(ymin = partial - se, ymax = partial + se), width = 0.2, size = 0.5) +  # Error bars
  geom_point(fill = "black", shape = 21, size = 3) +  # Circle for the partial effect
  labs(x = "Year ", y = NULL) +
  coord_cartesian(ylim = c(-1, 1)) +  # Adjust y-axis limits
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),  # Remove panel background
        axis.line = element_line(color = "black"),  # Add axis lines
        axis.text = element_text(size = 10, color = "black", family = "Arial"),  # Adjust axis text properties
        axis.title = element_text(size = 10, color = "black", family = "Arial"),   # Adjust axis title properties
        axis.ticks = element_line(color = "black", size = 0.5),  # Add axis ticks
        axis.ticks.length = unit(0.2, "cm"),  # Set length of ticks
        axis.ticks.margin = unit(0.5, "cm")) +  # Set margin between ticks and plot area
  guides(fill = FALSE) +  # Remove legend
  scale_y_continuous(labels = function(x) sprintf("%.1f", x))  # Format y-axis labels to 1 decimal place



### PLOTTING FIGURES -------------------------------------------------------------------------
# Combine all plots into one figure
combined_plot_n <- (a | b | c | d | e | f | g | h | i | j ) +
  plot_layout(ncol = 2, nrow = 5)  # Arrange plots in two columns

# Display the combined plot
combined_plot_n

ggsave("figure_4.tiff", 
       plot = combined_plot_n, width = 8.3, height = 10, units = "in", dpi = 1000)

setwd("Figures/") #Set working directory to save RDA file
save.image(file = "Final4.RDA")


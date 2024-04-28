# AIM:
# To assess the influence of environmental variables on the probability of 
# an animal being in ARS state 
#
# Running seperate GAM models for day versus night
## PREP ========================================================================
# Load in packages
library(mgcv); library(corrplot); library(fitdistrplus); library(readr); library(ggplot2); library(dplyr)
library(patchwork); library(gratia)

# Load in the data
load("horizontal05.RDA")
manta_df$g <- 1 - manta_df$g # Transform g so it is 1-g so that we are looking at predictors of foraging rather than of travelling
manta_df$id <- as.character(manta_df$id); class(manta_df$id) #Check id is the right class
names(manta_df)

## ===== FINAL GAM (DAY) =======================================================
gam_d_df <- manta_df %>% filter(period == "day")

gam_d <- gam(g ~
              s(wind_direction, bs = 'cc', k = 6) +
              s(wind_speed, k = 6) +
              s(sunaltitude, k = 6) + 
              s(kd490, k = 6) +
              s(bathy, k = 6) +
              s(sst, k = 6) +
              factor(tide_category) +
              factor(id),
            data = gam_d_df,
            method = "REML",
            family = betar (link = "logit", eps=0.0000000001))

summary(gam_d)
plot(gam_d, all.terms = TRUE, pages = 1, scale = 0, shade = 1)


## ===== FINAL GAM (NIGHT) =====================================================
gam_n_df <- manta_df %>% filter(period == "night")

gam_n <- gam(g ~
               s(wind_direction, bs = 'cc') +
               s(wind_speed) +
               s(fraction, k = 6) + 
               s(kd490, k = 6) +
               s(bathy, k = 6) +
               #s(sst, k = 6) +
               factor(tide_category) +
               factor(id),
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

### BATHY ------------------------------------------------------------------------
# Plot smooth component estimates with confidence intervals using ggplot2

a <- ggplot(sm_d, aes(y = est, x = bathy)) +
  geom_rug(data = gam_d_df, mapping = aes(x = bathy), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", linewidth = 1.5) +
  labs(y = "Partial effect",
       x = "Bathymetry (m)") +
  scale_x_continuous(limits = c(-310, -40), labels = function(x) abs(x), breaks = seq(-300, -50, by = 100)) +  # Set x-axis limits, remove negative sign, and add x labels every 50m
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

b <- ggplot(sm_d, aes(y = est, x = kd490)) +
  geom_rug(data = gam_d_df, mapping = aes(x = kd490), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#caa7cf", colour = "#caa7cf", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#caa7cf", size = 1.5) +
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
        axis.text = element_text(family = "Arial", size = 10, color = "black"))  # Set axis text properties


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
  scale_x_continuous(limits = c(18.5, 25)) +
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

### WindDir ----------------------------------------------------------------------
# Plot smooth component estimates with confidence intervals using ggplot2
# Your ggplot code
d <- ggplot(sm_d, aes(y = est, x = wind_direction)) +
  geom_rug(data = gam_d_df, mapping = aes(x = wind_direction), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#caa7cf", colour = "#caa7cf", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#caa7cf", size = 1.5) +
  labs(y = NULL,
       x = "Wind Direction (°)") +
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
  coord_cartesian(ylim = c(-0.6, 0.6))  # Set y-axis limits


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
  scale_x_continuous(limits = c(2, 14)) +
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



### ALTITUDE -------------------------------------------------------------------
# Plot smooth component estimates with confidence intervals using ggplot2
# Your ggplot code
f <- ggplot(sm_d, aes(y = est, x = sunaltitude)) +
  geom_rug(data = gam_d_df, mapping = aes(x = sunaltitude), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1.5) +
  labs(y = NULL,
       x = "Altitude (°)") +
  coord_cartesian(ylim = c(-2, 2)) +  # Adjust y-axis limits
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

### TIDE -----------------------------------------------------------------------
# Define custom labels
tide_labels <- c("falling" = "Falling", "rising" = "Rising", "slack" = "Slack")
# Plot the parametric effects with circles and error bars
g <- ggplot(tide_category_effects_d, aes(x = level, y = partial)) +
  geom_errorbar(aes(ymin = partial - se, ymax = partial + se), width = 0.2, size = 1) +  # Error bars
  geom_point(shape = 21, size = 5, fill = "black") +  # Circle for the partial effect
  labs(x = "Tide Category", y = "Partial Effect") +
  scale_x_discrete(labels = tide_labels) +  # Set custom labels
  coord_cartesian(ylim = c(-1, 1)) +  # Adjust y-axis limits
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),  # Remove panel background
        axis.line = element_line(color = "black"),  # Add axis lines
        axis.text = element_text(size = 10, color = "black", family = "Arial"),  # Adjust axis text properties
        axis.title = element_text(size = 10, color = "black", family = "Arial")) +  # Adjust axis title properties
  theme(axis.ticks.length = unit(-0.2, "cm"))  # Move ticks outside the plot

### ID -------------------------------------------------------------------------
# Plot the parametric effects with circles and error bars
h <- ggplot(id_effects_d, aes(x = level, y = partial)) +
  geom_errorbar(aes(ymin = partial - se, ymax = partial + se), width = 0.2, size = 1) +  # Error bars
  geom_point(fill = "black", shape = 21, size = 5) +  # Circle for the partial effect
  labs(x = "PTT #", y = "Partial effect") +
  coord_cartesian(ylim = c(-3, 3)) +  # Adjust y-axis limits
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),  # Remove panel background
        axis.line = element_line(color = "black"),  # Add axis lines
        axis.text = element_text(size = 10, color = "black", family = "Arial"),  # Adjust axis text properties
        axis.title = element_text(size = 10, color = "black", family = "Arial")) +  # Adjust axis title properties
  theme(axis.ticks.length = unit(-0.2, "cm")) +  # Move ticks outside the plot
  guides(fill = FALSE)  # Remove legend


### PLOTTING FIGURES -----------------------------------------------------------
# Combine all plots into one figure
combined_plot_d <- (a | b | c | d | e | f | g | h ) +
  plot_layout(ncol = 2, nrow = 5)  # Arrange plots in two columns

# Display the combined plot
combined_plot_d

## ===== VISUALISING GAM (NIGHT) =================================================
# evaluate the smooths
sm_n <- smooth_estimates(gam_n) %>%
  add_confint()

# Extract parametric effect
param_effects_n<- parametric_effects(gam_n)

tide_category_effects_n <- subset(param_effects_n, term == "factor(tide_category)")
id_effects_n <- subset(param_effects_n, term == "factor(id)")

### BATHY ----------------------------------------------------------------------
a <- ggplot(sm_n, aes(y = est, x = bathy)) +
  geom_rug(data = gam_n_df, mapping = aes(x = bathy), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1.5) +
  labs(y = "Partial effect",
       x = "Bathymetry (m)") +
  scale_x_continuous(limits = c(-310, -40), labels = function(x) abs(x), breaks = seq(-300, -50, by = 100)) +  # Set x-axis limits, remove negative sign, and add x labels every 50m
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
              alpha = 0.2, fill = "#caa7cf", colour = "#caa7cf", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#caa7cf", size = 1.5) +
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
  scale_x_continuous(limits = c(0.02, 0.1)) 

### SST ------------------------------------------------------------------------
# Plot smooth component estimates with confidence intervals using ggplot2
# Your ggplot code
#c <- ggplot(sm_n, aes(y = est, x = sst)) +
  geom_rug(data = gam_n_df, mapping = aes(x = sst), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1.5) +
  labs(y = "Partial effect",
       x = "Sea surface temperature (°C)") +
  scale_x_continuous(limits = c(18.5, 22)) +
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
  coord_cartesian(ylim = c(-1, 0.8))  # Set y-axis limits




### WindDir ----------------------------------------------------------------------
# Plot smooth component estimates with confidence intervals using ggplot2
# Your ggplot code
d <- ggplot(sm_n, aes(y = est, x = wind_direction)) +
  geom_rug(data = gam_n_df, mapping = aes(x = wind_direction), sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.2, fill = "#a6cee3", colour = "#a6cee3", linetype = "dashed") +  # Add dashed border
  geom_line(colour = "#a6cee3", size = 1.5) +
  labs(y = NULL,
       x = "Wind Direction (°)") +
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
  coord_cartesian(ylim = c(-1.6, 1.6))  # Set y-axis limits


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
  scale_x_continuous(limits = c(2, 12.5)) +
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
  coord_cartesian(ylim = c(-0.6, 0.6)) +  # Adjust y-axis limits
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


### TIDE -----------------------------------------------------------------------
# Define custom labels
tide_labels <- c("falling" = "Falling", "rising" = "Rising", "slack" = "Slack")
# Plot the parametric effects with circles and error bars
g <- ggplot(tide_category_effects_n, aes(x = level, y = partial)) +
  geom_errorbar(aes(ymin = partial - se, ymax = partial + se), width = 0.2, size = 1) +  # Error bars
  geom_point(shape = 21, size = 5, fill = "black") +  # Circle for the partial effect
  labs(x = "Tide Category", y = "Partial Effect") +
  scale_x_discrete(labels = tide_labels) +  # Set custom labels
  coord_cartesian(ylim = c(-1, 2)) +  # Adjust y-axis limits
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),  # Remove panel background
        axis.line = element_line(color = "black"),  # Add axis lines
        axis.text = element_text(size = 10, color = "black", family = "Arial"),  # Adjust axis text properties
        axis.title = element_text(size = 10, color = "black", family = "Arial")) +  # Adjust axis title properties
  theme(axis.ticks.length = unit(-0.2, "cm"))  # Move ticks outside the plot


### ID -------------------------------------------------------------------------
# Plot the parametric effects with circles and error bars
h <- ggplot(id_effects_n, aes(x = level, y = partial)) +
  geom_errorbar(aes(ymin = partial - se, ymax = partial + se), width = 0.2, size = 1) +  # Error bars
  geom_point(fill = "black", shape = 21, size = 5) +  # Circle for the partial effect
  labs(x = "PTT #", y = "Partial effect") +
  coord_cartesian(ylim = c(-3, 4)) +  # Adjust y-axis limits
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),  # Remove panel background
        axis.line = element_line(color = "black"),  # Add axis lines
        axis.text = element_text(size = 10, color = "black", family = "Arial"),  # Adjust axis text properties
        axis.title = element_text(size = 10, color = "black", family = "Arial")) +  # Adjust axis title properties
  theme(axis.ticks.length = unit(-0.2, "cm")) +  # Move ticks outside the plot
  guides(fill = FALSE)  # Remove legend


### PLOTTING FIGURES -------------------------------------------------------------------------
# Combine all plots into one figure
combined_plot_n <- (a | b  | d | e | f | g | h ) +
  plot_layout(ncol = 2, nrow = 5)  # Arrange plots in two columns

# Display the combined plot
combined_plot_n




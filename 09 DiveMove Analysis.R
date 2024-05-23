## PREP ========================================================================
# Load in packages
library(diveMove)

#Set working directory
setwd("RDA_files") 
# Load in the data 
load("vertical08.RDA")

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
  dtime = 1, # Resolution (seconds)
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
  dtime = 1, # Resolution (seconds)
  file = "")

dcalib_252779 <- calibrateDepth(tdr_252779, 
                                dive.thr = 10, # Threshold for minimum dive depth
                                zoc.method="offset",
                                offset=0)

# Calculate dive statistics using diveStats()
divestats_252779 <- diveStats(dcalib_252779)
summary(divestats_252779)


## SAVE FILE ===================================================================
setwd("RDA_files") #Set working directory to save RDA file

save(divestats_197235,
     divestats_238016,
     divestats_252779,
     file = "vertical09.RDA")

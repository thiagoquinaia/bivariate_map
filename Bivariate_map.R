# Load libraries
library(tidyverse)
library(sf)
library(terra)
library(rgeoboundaries)
library(climateR)
library(biscale)

#install.packages("remotes")
#remotes::install_github("wmgeolab/rgeoboundaries")
#remotes::install_github("mikejohnson51/climateR")

# Load United Kingdom boundary data at the country level (administrative level 0)
br0 <- geoboundaries(country = c("Brazil"))

# Load United Kingdom boundary data at the first administrative level (e.g., regions)
br1 <- geoboundaries(country = c("Brazil"), adm_lvl = "adm1")

# Plot the geometry of administrative level 1 boundaries with a gray border
plot(st_geometry(br1), border = "gray")

# Add the country boundary (administrative level 0) to the existing plot in bold
plot(st_geometry(br0), add = TRUE, size = 2)


# Retrieve TerraClimate data for the United Kingdom with specified variables (max and min temperature)
temp_terra = getTerraClim(AOI = br0, 
                          varname = c("tmax", "tmin"),
                          startDate = "2023-01-01",
                          endDate  = "2023-12-01")

# Calculate the monthly mean temperature by averaging maximum and minimum temperature
mean_temp_monthly <- (temp_terra$tmax + temp_terra$tmin) / 2

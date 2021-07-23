# Load packages and specify global options

# Tidy syntax
library(tidyverse)
library(magrittr)

# Spatial and census data
library(sf)
library(tigris)
library(tidycensus)

# Code and file management
library(units)
library(here)
library(markdown)
library(archive)

# Plotting
library(RColorBrewer)
library(ggnewscale)

# NHDPlus data
library(nhdR)
library(nhdplusTools)

# -------------------------

# Set global options

options(stringsAsFactors = FALSE) # get out of here factors
options(tigris_use_cache = TRUE) # cache tigris files
options(scipen = 999999) # display numbers without using scientific notation

# -------------------------

# Set analytic parameters

global_crs <- st_crs(4269)



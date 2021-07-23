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

# Support for various file types
library(archive)
library(foreign)

# Plotting
library(RColorBrewer)
library(ggnewscale)

# NHDPlus data
library(nhdR)
library(nhdplusTools)

# -------------------------

# Create directories

if (!dir.exists("here", "raw_data")) {
  dir.create("here", "raw_data")
}

# -------------------------

# Set global options

options(stringsAsFactors = FALSE) # get out of here factors
options(tigris_use_cache = TRUE) # cache tigris files
options(scipen = 999999) # display numbers without using scientific notation

# -------------------------

# Set analytic parameters

global_crs <- st_crs(4269) # specify CRS to use throughout analysis



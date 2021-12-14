# Stream restoration analysis functions
# for AGU poster code

# Lucy Andrews - December 2021

# - %not%in: returns the negation of %in%
# - get_ca_boundary: imports CA boundary into global environment as sf object
# - view_flat: quickly pulls up an sf object's attribute table
# - st_line_midpoints: returns the midpoint of a given sf line object
# - create_directory: checks if a directory exists and if not creates it
# - download_from_url: checks if a file exists in a directory and if not downloads it from a url
# - unzip_local: unzips a zipped file into the directory in which the zipped file is located
# - count_df_na: takes a dataframe and returns counts of NA values in each column
# - process_matches: returns matched pairs from a matchit object and an attribute dataframe

# -----------------------------

# %notin%: operator that returns the negation of %in%

`%notin%` <- Negate(`%in%`)

# -----------------------------

# get_ca_boundary(): function that imports CA boundary into global environment as sf object

library(tidyverse)
library(tigris)
library(sf)

get_ca_boundary <- function(crs) {
  states() %>%
    filter(NAME == "California") %>%
    rename(name = NAME) %>%
    dplyr::select(name) %>%
    st_transform(crs = crs)
}

# -----------------------------

# view_flat(): function that quickly pulls up an sf object's attribute table

library(sf)

view_flat <- function(sf_obj) {
  st_drop_geometry(sf_obj) %>%
    View(.)
}

# -----------------------------

# st_line_midpoints(): function that returns the midpoint of a given sf line object

st_line_midpoints <- function(sf_lines = NULL) {
  
  g <- st_geometry(sf_lines)
  
  g_mids <- lapply(g, function(x) {
    
    coords <- as.matrix(x)
    
    get_mids <- function (coords) {
      dist <- sqrt((diff(coords[, 1])^2 + (diff(coords[, 2]))^2))
      dist_mid <- sum(dist)/2
      dist_cum <- c(0, cumsum(dist))
      end_index <- which(dist_cum > dist_mid)[1]
      start_index <- end_index - 1
      start <- coords[start_index, ]
      end <- coords[end_index, ]
      dist_remaining <- dist_mid - dist_cum[start_index]
      mid <- start + (end - start) * (dist_remaining/dist[start_index])
      return(mid)
      
    }
    
    mids <- st_point(get_mids(coords))
    
  })
  
  geometry <- st_sfc(g_mids, crs = st_crs(sf_lines))
  
  geometry <- st_sf(geometry)
  
}

# -----------------------------

# get_upstream_length(): function that computes total stream length upstream of a segment

library(tidyverse)
library(nhdplusTools)

get_upstream_length <- function(comid, ut_network_df) {
  
  try(upstream_comids <- get_UT(ut_network_df, comid, distance = NULL))
  
  upstream_length_km <- ut_network_df %>%
    filter(COMID %in% upstream_comids) %>%
    summarize(total_length_km = sum(LENGTHKM)) %>%
    pull(total_length_km)
  
  return(upstream_length_km)
  
}

# -----------------------------

# create_directory(): function that checks if a directory exists and if not creates it

library(here)

create_directory <- function(path_with_directory) {
  
  if(!dir.exists(path_with_directory)) {
    
    dir.create(path_with_directory)
    
  }
  
}

# -----------------------------

# download_from_url(): function that checks if a file exists in a directory and if not downloads it from a url

download_from_url <- function(url, filename, directory) {
  
  if(!file.exists(here(directory, filename))) {
    
    download.file(url = url,
                  destfile = here(directory, filename))
    
  }
  
}

# -----------------------------

# unzip_local(): function that unzips a zipped file into the directory in which the zipped file is located

unzip_local <- function(zipped_file, directory) {
  
  unzip(here(directory, zipped_file),
        exdir = here(directory),
        overwrite = TRUE)
  
}

# -----------------------------

# count_df_na(): takes a dataframe and returns counts of NA values in each column

count_df_na <- function(df) {
  
  sapply(df, function(x) sum(is.na(x)))
  
}

# -----------------------------

# process_matches(): returns matched pairs from a matchit object and an attribute sf object

process_matches <- function(matchit_obj, study_points_to_match_sf) {
  
  study_points_to_match_sf <- study_points_to_match_sf %>%
    rowid_to_column(var = "row_id")
  
  return_matches <- study_points_to_match_sf %>%
    filter(obs_type == "restoration") %>%
    rename(match_id = row_id) %>%
    dplyr::select(obs_id, obs_type, geometry, match_id)
  
  match_indices <- matchit_obj$match.matrix %>%
    as.numeric() %>%
    as_tibble() %>%
    rowid_to_column(var = "match_id") %>%
    rename("row_id" = value)
  
  control_matches <- study_points_to_match_sf %>%
    dplyr::select(obs_id, obs_type, geometry, row_id) %>%
    inner_join(match_indices, by = "row_id") %>%
    dplyr::select(-row_id)
  
  return_matches <- return_matches %>%
    rbind(control_matches)
  
  return(return_matches)
  
}
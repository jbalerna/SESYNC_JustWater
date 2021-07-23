# Function that downloads a file from a web url and saves it to a specified directory
download_web_file <- function(desired_filename_as_string, 
                              web_address_as_string, 
                              folder_as_string) {
  
  # specify the file path
  path <- file.path(folder_as_string,
                    desired_filename_as_string)
  
  # check if the file exists already; if not, download and save the file
  if(!file.exists(path)){
    download.file(url = web_address_as_string,
                  destfile = path)
  }
}

# -------------------------

# An operator that returns objects that are not in a given object (list, vector, dataframe, etc.)
`%notin%` <- Negate(`%in%`)

# -------------------------

# Functions that loads state boundaries from Census Bureau as sf object using tigris package
get_ca_boundary <- function() {
  states() %>%
    filter(NAME == "California") %>%
    rename(name = NAME) %>%
    select(name) %>%
    st_transform(crs = global_crs) %>%
    st_make_valid()
}

get_mn_boundary <- function() {
  states() %>%
    filter(NAME == "Minnesota") %>%
    rename(name = NAME) %>%
    select(name) %>%
    st_transform(crs = global_crs) %>%
    st_make_valid()
}

# -------------------------

# Function that returns the midpoint of a line

st_line_midpoints <- function(sf_lines = NULL) {
  
  g <- st_geometry(sf_lines)
  
  g_mids <- lapply(g, function(x) {
    
    coords <- as.matrix(x)
    
    # this is just a copypaste of View(maptools::getMidpoints):
    get_mids <- function (coords) {
      dist <- sqrt((diff(coords[ , 1])^2 + (diff(coords[ , 2]))^2))
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
  
  geom <- st_sfc(g_mids, crs = st_crs(sf_lines))
  geom <- st_sf(geom)
}

# -------------------------
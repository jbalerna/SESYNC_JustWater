#Created by J. Balerna for SESYNC Summer Cyberinfrastructure Workshop
#July 2021

##Goals of script:
#select location from NRRSS - specific state or census tract?
#map number of restoration projects (points) AND census minority pop info (or median income?) as colors
#simple regression correlating the two

#install packages
install.packages("RSQLite")
install.packages("here")
install.packages("dplyr")
install.packages("sf")
install.packages("ggplot2")
install.packages("mapview")
install.packages("gstat")
install.packages("sp")
install.packages("spdep")
install.packages("spatialreg")



#open packages
library(RSQLite) # for bridging to SQL databases / RDB
library(here)
library(dplyr)
library(sf)
library(ggplot2)
library(mapview)
library(gstat)
library(sp)
library(spdep)
library(spatialreg)

## DATABASE CONNECTION

# select the driver
drv <- dbDriver("SQLite")

# give the database file path
db_nrrss <- paste(file.path(getwd()), 'input_data', 'NRRSS', 'nrrss-master', 'nrrss.sqlite', sep='/')

# connect to the database
con_nrrss <- dbConnect(drv, db_nrrss)

# list all the tables in the database
dbListTables(con_nrrss)

#load in a couple of the data frames
nrrss_record_table <- dbReadTable(con_nrrss, "nrrss_record_table")
geographic_table <- dbReadTable(con_nrrss, 'geographic_table')
proj_activities_table <- dbReadTable(con_nrrss, 'proj_activities_table')

head(nrrss_record_table)
head(geographic_table)
head(proj_activities_table)

#join lat long data to project info data (e.g., project cost)
proj_location_cost <-inner_join(geographic_table, proj_activities_table, by="nrrss_number")
head(proj_location_cost)

#change coordinates to one column in decimal form
proj_location_cost$lat <- proj_location_cost %>%
  mutate(lat_deg= as.numeric(lat_deg), lat_min = as.numeric(lat_min), lat_sec = as.numeric(lat_sec))%>%
  mutate(lat = ( lat_deg + (lat_min/60) + (lat_sec/3600)))

proj_location_cost$lon <- proj_location_cost %>%
  mutate(lon_deg= as.numeric(lon_deg), lon_min = as.numeric(lon_min), lon_sec = as.numeric(lon_sec))%>%
  mutate(lon = -1*( lon_deg + (lon_min/60) + (lon_sec/3600)))

head(proj_location_cost)
str(proj_location_cost)

proj_location_cost$lat <- as.numeric(unlist(proj_location_cost$lat))
proj_location_cost$lon <- as.numeric(unlist(proj_location_cost$lon))


#Change proj_location_cost from a data frame to spatial object
proj_location_cost_sf <- st_as_sf(proj_location_cost,
                 coords = c("lat","lon"),
                 crs = 32618)
head(proj_location_cost_sf)
plot(proj_location_cost_sf["proj_cost"])

mapview(proj_location_cost_sf["proj_cost"],
        map.types = 'OpenStreetMap')

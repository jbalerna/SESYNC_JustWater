#Created by J. Balerna for SESYNC Summer Cyberinfrastructure Workshop
#July 2021

##Goals of script:
#select location from NRRSS - specific state or census tract?
#map number of restoration projects (points) AND census minority pop info (or median income?) as colors
#simple regression correlating the two
#this continues with add-census-data.R script file!

#install packages
install.packages("RSQLite")
install.packages("here")
install.packages("dplyr")
install.packages("sf")
install.packages("ggplot2")
install.packages("ggspatial")
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
library(ggspatial)
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
proj_location_cost <-inner_join(geographic_table, 
                                proj_activities_table, 
                                by="nrrss_number")
head(proj_location_cost)

#change coordinates to one column in decimal form
proj_location_cost <- proj_location_cost %>%
  mutate(lat_deg= as.numeric(lat_deg), 
         lat_min = as.numeric(lat_min), 
         lat_sec = as.numeric(lat_sec))%>%
  mutate(lat = ( lat_deg + (lat_min/60) + (lat_sec/3600)))

proj_location_cost <- proj_location_cost %>%
  mutate(lon_deg= as.numeric(lon_deg), 
         lon_min = as.numeric(lon_min), 
         lon_sec = as.numeric(lon_sec))%>%
  mutate(lon = -1*( lon_deg + (lon_min/60) + (lon_sec/3600)))

proj_location_cost<- proj_location_cost %>%
  select(lon, lat, nrrss_number, proj_cost)

#Change proj_location_cost from a data frame to spatial object

proj_location_cost_sf <- st_as_sf(proj_location_cost,
                                  coords = c("lon","lat"), 
                                  crs=4269)

#check coordinate system
st_crs(proj_location_cost_sf)

#Other coordinate systems:
#32618 - "WGS_1984"
#4269 - "North American Datum 1983"

#Start mapping

ggplot(proj_location_cost_sf) +
  geom_sf() 

#colored by cost
ggplot(proj_location_cost_sf, aes(fill=proj_cost)) +
  geom_sf()
#this data is too skewed for it to really show any variation


# make it with a pretty tile - I can't get this to work
ggplot(proj_location_cost_sf) +
  annotation_map_tile(type="osm", zoomin=0)+
  geom_sf()


mapview(proj_location_cost_sf,
        map.types = 'OpenStreetMap')

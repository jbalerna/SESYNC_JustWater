library(RSQLite) # for bridging to SQL databases / RDB
library(ggspatial)
library(dplyr)
library(sf)
library(ggplot2)
library(mapview)
library(gstat)
library(sp)
library(spdep)
library(spatialreg)

drv <- dbDriver("SQLite")
db_nrrss <- paste(file.path(getwd()), 'input_data', 'NRRSS', 'nrrss-master', 'nrrss.sqlite', sep='/')
con_nrrss <- dbConnect(drv, db_nrrss)

geographic_table <- dbReadTable(con_nrrss, 'geographic_table')
proj_activities_table <- dbReadTable(con_nrrss, 'proj_activities_table')

proj_location_cost <-inner_join(geographic_table, 
                                proj_activities_table, 
                                by="nrrss_number")

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

proj_location_cost_sf <- st_as_sf(proj_location_cost,
                                  coords = c("lon","lat"), 
                                  crs=4269)


ggplot(proj_location_cost_sf) +
  geom_sf() 

#colored by cost
ggplot(proj_location_cost_sf, aes(fill=proj_cost)) +
  geom_sf()


# make it with a pretty tile
ggplot(proj_location_cost_sf) +
  annotation_map_tile(type='osm', zoomin=0)+
  geom_sf()


mapview(proj_location_cost_sf["proj_cost"],
        map.types = 'OpenStreetMap')

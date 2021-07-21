#Created by J. Balerna for SESYNC Summer Cyberinfrastructure Workshop
#July 2021

##Goals of script:
#select location from NRRSS - specific state or continental US? which is easier?
#map number of restoration projects (points) AND census minority pop info as colors
#simple regression correlating the two

#install packages
install.packages("RSQLite")
install.packages("here")

#open packages
library(RSQLite) # for bridging to SQL databases / RDB
library(here)

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

head(geographic_table)
head(proj_activities_table)

#not sure what this does yet
inner_join(geographic_table, proj_activities_table, by="nrrss_number")



install.packages("dplyr")
library(dplyr)
geographic_table %>%
  mutate(lat_deg= as.numeric(lat_deg), lat_min = as.numeric(lat_min), lat_sec = as.numeric(lat_sec))%>%
  mutate(lat = ( lat_deg + (lat_min/60) + (lat_sec/3600)))

geographic_table %>%
  mutate(lon_deg= as.numeric(lon_deg), lon_min = as.numeric(lon_min), lon_sec = as.numeric(lon_sec))%>%
  mutate(lon = -1*( lon_deg + (lon_min/60) + (lon_sec/3600)))


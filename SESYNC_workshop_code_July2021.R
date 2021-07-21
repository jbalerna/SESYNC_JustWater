##Created by J. Balerna for SESYNC Summer Cyberinfrastructure Workshop
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
db_nrrss <- here("input_data", "NRRSS", "nrrss-master", "nrrss.sqlite")

# connect to the database
con_nrrss <- dbConnect(drv, db_nrrss)

# list all the tables in the database
dbListTables(con_nrrss)

#load in a couple of the data frames
nrrss_record_table <- dbReadTable(con_nrrss, "nrrss_record_table")
geographic_table <- dbReadTable(con_nrrss, 'geographic_table')



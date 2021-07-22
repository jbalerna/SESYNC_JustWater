library(tidycensus)
census_api_key(readLines('~/census_key.txt')) # store api key in file and put here

#view in just 2005 
v2005 <- load_variables(2005, "acs1", cache = TRUE)

md <- get_acs(geography = "county", 
              variables = c(medincome = "B19013_001"), 
              state = "MD", #state code for maryland
              year = 2009, 
              geometry=T)


md_sf<- st_as_sf(md) #has same coordinate system as nrrss data

ggplot(md_sf, aes(fill = estimate)) +
  geom_sf() 

md_nrrss<-st_filter(proj_location_cost_sf, md_sf) # proj_location_cost_sf makde in All-nrrss-map.R


ggplot(md_sf) +
  geom_sf() +
  geom_sf(data = md_nrrss)

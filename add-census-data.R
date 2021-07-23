install.packages("tidycensus")

library(tidycensus)
census_api_key("f7a31bca866d690a768302cd1cea2e32346f2006", install=TRUE)

#view in just 2005 - helps you see which codes you need to find income, pop size, etc.
v2005 <- load_variables(2005, "acs1", cache = TRUE)
v2005

md <- get_acs(geography = "county", 
              variables = c(medincome = "B19013_001"), 
              state = "MD", #state code for maryland
              year = 2009, 
              geometry=T)

#Both md_sf and md_sf_test work in ggplot
#but I think the md_sf_test has a more defined crs 
#so I used that for leaflet mapping
md_sf<- st_as_sf(md) #has same coordinate system as nrrss data

md_sf_test <- st_sf(md, 
                     crs=4269)

# proj_location_cost_sf made in All-nrrss-map.R - this is defined in workshop code Rscript
md_nrrss<-st_filter(proj_location_cost_sf, md_sf_test) 


#double check crs
st_crs(md_sf)
st_crs(md_sf_test)
st_crs(md_nrrss)

#Check visualizations

ggplot(md_sf_test, aes(fill = estimate)) +
  geom_sf() 


#Add some additional points for reference
sesync <- st_sfc(
  st_point(c(-76.503394, 38.976546)),
  crs = st_crs(md_sf))

oriole_park <- st_sfc(
  st_point(c(-76.6216, 39.2839)),
  crs = st_crs(md_sf))


#plot plot plot!
library(leaflet)
library(htmlwidgets)

pal <- colorNumeric("PuBuGn", md_sf_test$estimate)

Map_census_restor-sites_MD <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = md_sf_test,
              fillColor = ~pal(estimate),
              fillOpacity = 0.75,
              weight=1) %>%
  addLegend(pal = pal, values = md_sf_test$estimate, 
            title = "Median Income",
            position = "bottomleft") %>%
  addCircleMarkers(data=md_nrrss,
                   color="red",
                   fillOpacity = 0.7,
                   radius = 0.2)

#share map
saveWidget(Map_census_restor-sites_MD, file="Map_census_restor-sites_MD.html")


ggplot(md_sf_test, 
       aes(fill = estimate/10000)) +
  geom_sf()  +
  labs(fill="Median Income/10000") +
  geom_sf(data = md_nrrss, color = 'red',
          fill = NA, size = 1) +
  geom_sf(data = sesync, color = 'limegreen',
          fill = NA, size = 3) +
  geom_sf(data = oriole_park, color = 'limegreen',
        fill = NA, size = 3) +
  annotate(
    "text", label = "SESYNC",
    x = -76.6, y = 38.95, size = 3.5, colour = "white", font=2) +
  annotate(
    "text", label = "Baltimore City",
    x = -76.6216, y = 39.25, size = 3.5, colour = "white", font=2)




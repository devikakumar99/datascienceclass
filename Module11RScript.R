getwd()
setwd("~/Desktop/RESEARCH/DataSci")

#MODULE 11.1 INTRODUCTION TO GIS

rm(list=ls(all=TRUE))

#load all the necessary packages

# install these normally if you don't have them
library(rio) 
library(tidyverse)
library(googlesheets4) 
library(labelled) 
library(data.table)
library(varhandle)
library(ggrepel)
library(geosphere) 
library(rgeos)
library(viridis)
library(mapview) 
library(rnaturalearth)
library(rnaturalearthdata)
library(devtools)
library(remotes)
library(raster)
library(sp)
library(sf)
library(Imap)
library(rnaturalearthhires)# devtools::install_github("ropensci/rnaturalearthhires") 
library(ggsflabel) # devtools::install_github("yutannihilation/ggsflabel")

#MODULE 11.2 Loading and Transforming Shapefiles using st_read from the sf pkg

world_borders <- st_read("/Users/devikakumar/Desktop/RESEARCH/DataSci")
#make sure that the file is projected in the right WGS84 format
borders <- st_transform(world_borders, "+proj=longlat +ellps=WGS84 +datum=WGS84")
#now that its in the correct format, we can remove the world_borders
rm(world_borders)

#MODULE 11.3 Prep the Natural Resources Data
orig_nr = import("/Users/devikakumar/Desktop/RESEARCH/DataSci/v11almost_newer_natural_resources.dta")
dim(orig_nr)

# create fake variable to summarize over
orig_nr$lat_long = orig_nr$latitude + orig_nr$longitude
# collapse
nr = orig_nr %>%
 group_by(country, resource, latitude, longitude, continent, region_wb) %>% 
 summarize(lat_long = mean(lat_long, na.rm=TRUE)) %>% 
 dplyr::select(-c(lat_long)) # drop the fake variable

#OR you can use distinct which is a much easier way to do this
nr <- orig_nr %>%
  distinct(country, resource, latitude, longitude, continent, region_wb)

# let's drop the NAs just in case
nr <- na.omit(nr, select=c("latitude","longitude", 
                           "resource", "country",
                           "continent", "region_wb"))

#now you transform df into the sf format
#taking the latitude and longitude into a new variable, geometry
#make sure to put the longitude first because it corresponds to x and y that way
#crs asks what kind of coordinate system and we're using WGS84
nr_sf <- st_as_sf(nr, coords = c("longitude", "latitude"), crs = 4326,
                  agr = "constant")

#rename resource to Resource
library(data.table)
setnames(nr_sf, "resource" , "Resource")

#for our purposes, only using the gold, oil, and diamond resources only
final = transform(nr_sf,
                  Resource = factor(replace(as.character(Resource),
                  list = !Resource %in% c("oil","gold", "diamond"), values = "other")))

#put it back into sf format
final = st_sf(final)

table(final$Resource)

#MODULE 11.4 MAPPING THE POINT DATA

#get the world map from the natural earth package: rnaturalearth
world <- ne_countries(scale = "small", returnclass = "sf")

#for some reason this is overloading my R program so i'm NOT going to run this
#now we can make a basic world map that interacts with ggplot
world_basic = ggplot() + 
  geom_sf(data = world) + 
  geom_sf(data = final)
print(world_basic)

ggsave(world_basic, filename = "world_map.png", width = 6.5, height = 6)

# World map (with legend, theme change, and shapes for resources)
world_all = ggplot() +
  geom_sf(data = world) +
  geom_sf(data = final, aes(shape=Resource)) + theme_void() +
  scale_shape_manual(values= c("gold" = 11,
                               "diamond" = 18, "oil" = 10, "other" = 20)) +
  theme(legend.position = "right") 

print(world_all)

#SKIP to Africa map
africa <- ne_countries(continent = 'africa', 
                       scale = "large",
                       returnclass = "sf")

# subset to only get African natural resource data
africa_data = subset(final, continent=="africa")

# make the map
africa_map = ggplot() +
  geom_sf(data = africa) +
  geom_sf(data = africa_data, aes(shape=Resource)) +
  theme_void() +
  scale_shape_manual(values= c("gold" = 11,
                               "diamond" = 5, "oil" = 10, "other" = 20)) +
  theme(legend.position = "right") 

print(africa_map)

# save the map
ggsave(africa_map, filename = "africa_map.png", width = 6.5, height = 6)

####################################################
#COLOMBIA DO THE SAME THING, BUT WITH COLORS
#since it didn't seem to be working, won't be asked on exam
#but here is the code anyways
colombia <- ne_countries(country = 'colombia', scale = "large", returnclass = "sf")

# perform the transformation
final2 = transform(nr_sf,
                   Resource = factor(replace(as.character(Resource), 
                   list = !Resource %in% c("oil","gold", "emerald"),
                                                   values = "other")))
# put everything back in sf form (the thing I forgot in the video)
final2=st_sf(final2)

#subset colombia
colombia_data <- subset(final2, country=="colombia")

#now map it with colors
colombia_map = ggplot() + geom_sf(data = colombia) + 
  geom_sf(data = colombia_data, aes(color=Resource)) + 
  theme_void() + theme(legend.position = "right") + 
  scale_color_manual(values= c("gold" = "gold",
                               "emerald" = "dark green",
                               "oil" = "black",
                               "other" = "red"))
print(colombia_map)

## Interactive point mapping with mapview
red <- colorRampPalette(c("red"))
gray <- colorRampPalette(c("gray")) 
mapview(colombia, col.regions = gray(100)) +
  mapview(colombia_data, col.regions = red(100))

#remove unnecessary objects
rm(asia,nr,nr_sf,africa,colombia,colombia_data,final2,world_basic, red,gray)

####################################################
#map for South Asia SUBSETTING

# getting a map of Asia from Natural Earth package
asia <- ne_countries(continent = 'asia', scale = "small", returnclass = "sf")

#which countries are included?
table(asia$admin)

#subset only south asian countries
south_asia <- subset(asia, admin=="Afghanistan" |
                       admin=="Pakistan" | admin=="India" | 
                       admin=="Nepal" | admin=="Bangladesh" | 
                       admin=="Myanmar" | admin=="Bhutan" | 
                       admin=="Myanmar" | admin=="Thailand" |
                       admin=="Laos" | admin=="Malaysia" | 
                       admin=="Vietnam" | admin=="Sri Lanka" |
                       admin=="Cambodia")

#now get the natural resources data
south_asia_nr <- subset(final, country=="afghanistan" | country=="pakistan" |
                          country=="india" | country=="nepal" | 
                          country=="bangladesh" | country=="myanmar (burma)" | 
                          country=="bhutan" | country=="myanmar" | 
                          country=="thailand" | country=="laos" | 
                          country=="malaysia" | country=="vietnam" | 
                          country=="sri lanka" | country=="cambodia")

#create the visualization 
south_asia_basic = ggplot() + 
  geom_sf(data = south_asia) + 
  geom_sf(data = south_asia_nr) + 
  geom_sf_label_repel(data = south_asia,
                      aes(label = admin),nudge_x = -1, nudge_y = -1, seed = 10 ) + 
  ggtitle("Natural Resource Locations in South Asia (Progress to Date)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) # center the title 

print(south_asia_basic)

#save the plot
ggsave(south_asia_basic, filename = "south_asia_map.png", width = 6.5, height = 6)


#MODULE 11.5 Mapping Polygon Data via Merging and Subsetting

#see the country names
table(orig_nr$country)

#put it back into Title form with the uppercase
orig_nr$country = str_to_title(orig_nr$country)
table(orig_nr$country)

#fix country names as needed
orig_nr$country[orig_nr$country == "Democratic Republic Of Congo"] = "Democratic Republic of Congo"
orig_nr$country[orig_nr$country == "Republic Of Congo"] = "Republic of Congo"
orig_nr$country[orig_nr$country == "Myanmar (Burma)"] = "Myanmar"
orig_nr$country[orig_nr$country == "Swaziland (Eswatini)"] = "Swaziland"

#get the country code
library(countrycode)
orig_nr$ISO3 = countrycode(sourcevar = orig_nr$country,
                           origin = "country.name",
                           destination = "iso3c",
                           warn = TRUE)
#fix Kazahkstan
orig_nr$ISO3[orig_nr$country == "Kazahkstan"] = "KAZ"

#creating a smaller dataset with what we want
orig_nr_small <- na.omit(subset(orig_nr, select=c("latitude","longitude",
                                                  "resource", "country",
                                                  "continent", "log_wb_val", "ISO3")))

#now merge the data (generally when you're mapping you want to do this)
merged_data = left_join(borders, orig_nr_small, by=c("ISO3"))

# subset the natural resources data by south america (to speed up mapping)
# merged data with our natural resources
sa_nr <- subset(merged_data, country=="Chile" | country=="Peru" | country=="Argentina" |
                  country=="Bolivia" | country=="Brazil" | country=="Suriname" | country=="Guyana"| 
                  country=="Ecuador" |country=="Venezuela" | country=="Paraguay" | 
                  country=="Uruguay" |country=="Colombia" )

#collapse this data, so we only have 13 observations with sa_nr
sa_nr_collapsed = sa_nr %>%
  group_by(country) %>%
  summarize(log_wb_val = sum(log_wb_val, na.rm=TRUE))

#rename the Log Value variable for the legend (the natural resource variable)
setnames(sa_nr_collapsed, "log_wb_val", "Log Value")

#get south america data from rnaturalearth package
south_america <- ne_countries(continent = 'south america', scale = "medium",
                              returnclass = "sf")

#now create the map
sa_map = ggplot() +
  geom_sf(data = south_america) +
  geom_sf(data = sa_nr_collapsed, aes(fill=`Log Value`)) + 
  scale_fill_viridis(option = "viridis") +
  ggtitle("Natural Resource Wealth in South America (World Prices), 1994-2014") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_void()
print(sa_map)

ggsave(filename = "south_america_map.png", width = 6.5, height = 6)




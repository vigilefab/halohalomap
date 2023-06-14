#----- PRELIMINARIES -----#

# change system language to english
Sys.setlocale(category = "LC_ALL", locale = "English")
Sys.setenv(LANG = "en")

# deactivate scientific notation
options(scipen=999)


# setup libraries
library(osmdata)
library(leaflet)
library(rshiny)
library(readxl)
library(lubridate)
library(dplyr)
library(sf)
library(htmlwidgets)
library(htmltools)
library(magrittr)
library(tidygeocoder)

# set wd
setwd("C:/Users/user/Dropbox/Halo-Halo/Halo-Halo IG & Website/Sonstiges/Pinboard Map/git/halohalomap")

# clear workspace
rm(list = ls())








#-------------------------------------------------
#----- WHERE ARE WE SCATTERED ACROSS EUROPE -----#
#-------------------------------------------------

# import mailing mailing list
maillist <- read_xlsx(path = "../../Mailinglist.xlsx", col_types = "text")


# clean date
split <- strsplit(x = maillist$Timestamp, split = c(" ")) 
split <- do.call(rbind.data.frame, split)
maillist$date <- as.Date(split[[1]], "%m/%d/%Y")

# clean cities
maillist <- maillist[!is.na(maillist$`Ort / Stadt (optional)`), ]
maillist$city <- paste0(maillist$`Ort / Stadt (optional)`, ", ", maillist$Land)

# using getbb() function to geocode locations
for(i in 1:nrow(maillist)){
  coordinates = getbb(maillist$city[i])
  maillist$long[i] = (coordinates[1,1] + coordinates[1,2])/2
  maillist$lat[i] = (coordinates[2,1] + coordinates[2,2])/2
}


# add jitter to coordinates so that identical coordinates differ slightly
 maillist$lat_jit <- jitter(maillist$lat, factor = 12)
 maillist$lon_jit <- jitter(maillist$long, factor = 3)


# convert 
 maillist2 <- maillist %>% sf::st_as_sf(coords = c("lon_jit", "lat_jit"), crs = 4326) 

 
# geb bb of germany to set default zoom 
 de_bb <-  getbb("Germany")
 de_long <- 10.4541
 de_lat <- 51.1642
 
  
# Create map with pin-clustering with leaflet  
map_clust <- leaflet(maillist) %>% 
  addProviderTiles("CartoDB.Positron",
                   options = providerTileOptions(minZoom = 2, maxZoom = 9)) %>% 
  addMarkers(label = ~ `Ort / Stadt (optional)`,
             clusterOptions = markerClusterOptions(maxClusterRadius = 15)) %>% 
  setView(de_long, de_lat, zoom = 5)
map_clust


# 
# # Create map with pins (no clustering) - for IG
# 
# greenLeafIcon <- makeIcon(
#   iconUrl = "https://halo-halo.de/wp-content/uploads/2022/06/Halo-Halo-Logo-noword.png",
#   iconWidth = 38, iconHeight = 25,
#   iconAnchorX = 22, iconAnchorY = 20,
#   shadowWidth = 50, shadowHeight = 64,
#   shadowAnchorX = 4, shadowAnchorY = 62
# )
# 
# # determine colors
# pal <- colorFactor(c("#f3bb50", "#326ebd", "#d15956"), domain = c("Österreich", "Deutschland", "Schweiz"))
# 
# map <- leaflet(maillist2) %>% addProviderTiles("CartoDB.Positron", 
#                                               options = providerTileOptions(minZoom = 2, maxZoom = 10)) %>% 
#   addCircleMarkers(label = ~ `Ort / Stadt (optional)`,
#     color = ~pal(Land),
#     stroke = FALSE, fillOpacity = 0.5
#   ) %>% 
#   setView(de_long, de_lat, zoom = 5)
# #  addMarkers(label = ~ `Ort / Stadt (optional)`) #, icon = greenLeafIcon)
# 
# map


# Export as HTML file
saveWidget(map_clust, 'map_cluster.html', selfcontained = FALSE)
# saveWidget(map, 'map.html', selfcontained = FALSE)



table(maillist$Land)

























#-------------------------------
#----- HALO-HALO KITAKITS -----#
#-------------------------------

# import mailing mailing list
kitakits <- read_xlsx(path = "../../Kitakits.xlsx")
kitakits$Date <- format(kitakits$Date, "%d %B %Y")

# create content of pop-up
w <- 170
br <- "<br/>"
kitakits <- kitakits %>%
  mutate(preview = paste0("<center><img src = '", Image, "', width = ", w, "px></center>", #br,
                          "<b><a href='", Link, "'>", Event, "</a></b>", br,
                          Place, ", ", Country, br,
                          Date)
         )

# clean cities
kitakits <- kitakits %>%
  mutate(location = if_else(is.na(Adresse),
                            false = paste0(Adresse, ", ", Country),
                            true = paste0(Place, ", ", Country)))

# using ggmap::geocodes() function to geocode locations
# source: https://stackoverflow.com/questions/44527893/how-can-i-get-latitude-and-longitude-from-an-address-with-r
kitakits <- kitakits %>%
  geocode(location, method = 'osm', lat = latitude , long = longitude)

# for some reason, the wiener stammtisch location doest get coords. add manually
kitakits[kitakits$Event == "Monatliche Wiener Stammtisch", "latitude"] <- 48.197310787725286
kitakits[kitakits$Event == "Monatliche Wiener Stammtisch", "longitude"] <- 16.365477428835185


# convert
kitakits2 <- kitakits %>% sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


# geb bb of germany to set default zoom
de_bb <-  getbb("Germany")
de_long <- 10.4541
de_lat <- 51.1642

# determine colors
pal2 <- colorFactor(c("#f3bb50", "#326ebd", "#d15956"), domain = c("Austria", "Germany", "Switzerland"))
pal3 <- colorFactor(c("#326ebd", "#f3bb50", "#d15956"), domain = c("Austria", "Germany", "Switzerland"))


# Create labels as list
labs <- as.list(kitakits$preview)

# Create map with pin-clustering with leaflet
kitakits_clust <- leaflet(kitakits) %>%
  addMarkers(~longitude, ~latitude, popup = lapply(labs, HTML),
#             clusterOptions = markerClusterOptions(), # maxClusterRadius = 15)
             ) %>%
#  addPopups(~long, ~lat, popup=kitakits$preview,
#            options = popupOptions(maxWidth = w,
#                                   closeButton = TRUE
#            )) %>%
  addProviderTiles("CartoDB.Positron",
                   options = providerTileOptions(minZoom = 2, maxZoom = 25)) %>%
  setView(de_long, de_lat, zoom = 7)
kitakits_clust




kitakits_map <- leaflet(kitakits) %>%
  addProviderTiles("CartoDB.Positron",
                   options = providerTileOptions(minZoom = 2, maxZoom = 25)) %>%
  addCircleMarkers(~longitude, ~latitude, popup = lapply(labs, HTML),
                   color = ~pal3(Country),
                   stroke = FALSE, fillOpacity = 0.75,
                   clusterOptions = markerClusterOptions(maxClusterRadius = 15)) %>%
  setView(de_long, de_lat, zoom = 5)
kitakits_map

# IDEA: CHANGE COLOR OF MARKERS: https://github.com/pointhi/leaflet-color-markers


# Export as HTML file
saveWidget(kitakits_clust, 'kitakits_clust.html', selfcontained = FALSE)
saveWidget(kitakits_map, 'kitakits_map.html', selfcontained = FALSE)









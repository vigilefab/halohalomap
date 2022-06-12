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

  
# set wd
setwd("C:/Users/user/Dropbox/Halo-Halo/Halo-Halo IG & Website/Sonstiges/Pinboard Map")

# clear workspace
rm(list = ls())









#----- START MAIN CODE -----#

# import mailing mailing list
maillist <- read_xlsx(path = "20220611_Mailinglist.xlsx", col_types = "text")


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
 maillist$lat_jit <- jitter(maillist$lat, factor = 2)
 maillist$lon_jit <- jitter(maillist$long, factor = 2)


# convert 
 maillist2 <- maillist %>% sf::st_as_sf(coords = c("long", "lat"), crs = 4326) 
 
  
# Create map with pin-clustering with leaflet  
map_clust <- leaflet(maillist) %>% addProviderTiles("CartoDB.Positron", 
                                       options = providerTileOptions(minZoom = 2, maxZoom = 9)) %>% 
  addMarkers(label = ~ `Ort / Stadt (optional)`,
  clusterOptions = markerClusterOptions(maxClusterRadius = 10))
map_clust

# Create map with pins (no clustering) - for IG

greenLeafIcon <- makeIcon(
  iconUrl = "https://halo-halo.de/wp-content/uploads/2022/06/Halo-Halo-Logo-noword.png",
  iconWidth = 38, iconHeight = 25,
  iconAnchorX = 22, iconAnchorY = 20,
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)

# determine colors
pal <- colorFactor(c("#f3bb50", "#326ebd", "#d15956"), domain = c("Österreich", "Deutschland", "Schweiz"))

map <- leaflet(maillist2) %>% addProviderTiles("CartoDB.Positron", 
                                                    options = providerTileOptions(minZoom = 2, maxZoom = 9)) %>% 
  addCircleMarkers(label = ~ `Ort / Stadt (optional)`,
    color = ~pal(Land),
    stroke = FALSE, fillOpacity = 0.5
  )
#  addMarkers(label = ~ `Ort / Stadt (optional)`) #, icon = greenLeafIcon)

map


# Export as HTML file
saveWidget(map_clust, 'map_cluster.html', selfcontained = FALSE)
saveWidget(map, 'map.html', selfcontained = FALSE)



table(maillist$Land)










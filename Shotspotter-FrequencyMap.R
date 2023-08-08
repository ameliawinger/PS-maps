setwd("~/Desktop/Shotspotter/FrequencyMap")
library(tidyverse)
library(rvest)
library(dplyr)
library(lubridate)
library(tidygeocoder)
library(ggplot2)
library(stringr)
library(readr)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(plotly)
library(geojsonio)
library(rgdal)

###STEP ONE: IMPORTING AND CLEANING DATA
#Import raw data
rawdata <- read_csv("rawdata.csv")

#Create column combining latitude and longitude
rawdata <- rawdata %>% 
  mutate(LatLong = paste(Lat, CleanLong))

#Create a pivot table showing how many alerts were issued at each 
pivot <- rawdata %>% 
  group_by(LatLong) %>% 
  summarize(Freq = n())

#Create columns for just the latitude and longitude (important for map)
pivot <- pivot %>% 
  mutate(Lat = as.numeric(gsub( " .*$", "", LatLong)),
         Long = as.numeric(gsub(".* ", "", LatLong)))

#Arrange pivot table from lowest frequency to highest (will be important for layering map dots)
pivot <- arrange(pivot, Freq)

#Create frequency categories (for color-coding on map)
pivot <- pivot %>% 
  mutate(Category = ifelse(pivot$Freq >= 10, "High",
                           ifelse(pivot$Freq >= 4, "Medium",
                                  ifelse(pivot$Freq >= 2, "Low", "Very Low"))))

###STEP TWO: CREATING THE MAP
#Create map color palette
pal <- colorFactor(palette = c("#003a6d", "#1192e8", "#82cfff", "#e5f6ff"),
                   levels = c("High", "Medium", "Low", "Very Low"))

#Create map labels
labs <- lapply(seq(nrow(pivot)), function(i) {
  paste0("<b>Number of Shotspotter Alerts: </b>", pivot[i, "Freq"], '<br />')})

#Create map
m <- leaflet(pivot) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter,
                   options = providerTileOptions(minZoom = 11, maxZoom = 19)) %>% 
  setView(lng = -79.99045863436507, lat = 40.439970708384806, zoom = 12) %>% 
  addCircleMarkers(lng = pivot$Long, lat = pivot$Lat, 
                   label = lapply(labs, htmltools::HTML),
                   stroke = FALSE, fillOpacity = 1, color = ~pal(Category),
                   radius = 4) %>% 
  addLegend("bottomright", 
            colors = c("#e5f6ff", "#82cfff", "#1192e8", "#003a6d"),
            labels = c("1 Alert", "2-3 Alerts", "4-9 Alerts", "10+ Alerts"),
            opacity = .9)
m

#save map as html file
saveWidget(m, file="shotspotter_map.html")






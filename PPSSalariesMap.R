setwd("~/Desktop/PPSSalariesMap")
packages <- c("tidyverse", "rvest", "dplyr", "lubridate", "tidygeocoder", "ggplot2", "stringr", "readr", "leaflet", "htmltools", "htmlwidgets", "geojsonio",
              "rgdal", "leafem", "raster", "sf", "lattice", "xml2", "leaflet.extras", "devtools")
lapply(packages, require, character.only = TRUE) 


##Read in geojson
zipcodes <- geojsonio::geojson_read("updated.json", what = "sp")
alleg <- geojsonio::geojson_read("alleg.json", what="sp")
##Read in data
data <- read_csv("update.csv")

##Calculate natural breaks for color palette
#getJenksBreaks(data$Count, 6)
##Create color palette
bins <- c(1, 8, 19, 32, 51, 89)
pal <- colorBin(c("#FAEAB7", "#F6C066", "#D88D38",
                  "#BF6D2E", "#8F4A1F"), domain = data$Count, bins = bins)

##Create labels
labs <- lapply(seq(nrow(data)), function(i) {
  ifelse(data[i,"Count"] == 1, 
         paste0("<strong>ZIP Code: ",data[i, "ZIP"], '</strong><br />',
                                      "This ZIP code is home to <strong>", data[i,"Count"], " PPS teacher</strong>."),
         paste0("<strong>ZIP Code: ",data[i, "ZIP"], '</strong><br />',
                "This ZIP code is home to <strong>", data[i,"Count"], " PPS teachers</strong>.")) })

##Create map
m <- leaflet(zipcodes)
m <- m %>% 
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
  setView(lng = -79.9959, lat = 40.4406, zoom = 9) %>%
  addPolygons(fillColor = ~pal(data$Count), weight = .35, opacity = 1, 
              color = "dimgray", fillOpacity = 0.95,
              label = lapply(labs, htmltools::HTML)) %>% 
  addLegend(pal=pal, values=data$Count, title= "Number of PPS Teachers as Residents", 
            position="bottomright", opacity=.9)
###OPTIONAL: Add a line to represent the county border
#m <- m %>% 
  #addPolygons(data=alleg, weight=1, color="dimgray", fillOpacity=0, opacity=1)
m

saveWidget(m, file="teachers_map.html")






















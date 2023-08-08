setwd("~/Desktop/turnoutMap")
packages <- c("tidyverse", "rvest", "dplyr", "lubridate", "tidygeocoder", "ggplot2", "stringr", "readr", "leaflet", "htmltools", "htmlwidgets", "geojsonio",
              "rgdal", "leafem", "raster", "sf", "lattice", "xml2", "leaflet.extras", "devtools")
lapply(packages, require, character.only = TRUE)    

####Import geojson of all counties
alleg_boundaries <- geojsonio::geojson_read("votemap.json", what = "sp")
##Subset county data for just Pittsburgh precincts
pitt_boundaries <- subset(alleg_boundaries, alleg_boundaries$NAME == "PITTSBURGH")

###Import charlie's voting data
votedata <- read_csv("charliedata.csv")

###Creating invisible markers for searching
######Start by putting names+coordinates of all polygons in a data frame
polygon_coordinates <- data.frame()
for(i in 1:402) {
  polygon_coordinates <- rbind(polygon_coordinates, pitt_boundaries@polygons[[i]]@labpt)
}
polygon_coordinates <- mutate(polygon_coordinates, polyname = votedata$Formatted_Name)
##Rename column names
polygon_coordinates <- polygon_coordinates %>% 
  rename(polylat = X40.4911026097577,
         polylong = X.80.0073791576877)


#Create color bins for choropleth
bins <- c(0, .38, .47, .54, .62, 1)
pal <- colorBin(c("#FEEDDE", "#FEC894", "#FD9E54",
                  "#EC641A", "#A63604"), domain = votedata$VoterTurnout, bins = bins)

###Create labels for choropleth
labs <- lapply(seq(nrow(votedata)), function(i) {
  paste0("<strong>",votedata[i, "Formatted_Name"], '</strong><br />',
         "Total Votes Cast: ",votedata[i, "BallotsCast"], '<br />',
         "Turnout: ", votedata[i, "VoterTurnout_Formatted"], '<br />') })

###CREATE CHOROPLETH MAP
m <- leaflet(pitt_boundaries)
m <- m %>% 
  addProviderTiles(providers$Esri.WorldGrayCanvas,
                   options = providerTileOptions(minZoom = 11, maxZoom = 16)) %>% 
  setView(lng = -79.9959, lat = 40.4406, zoom = 12) %>%
  addPolygons(fillColor = ~pal(votedata$VoterTurnout), weight = 1, opacity = 1, 
              color = "white", fillOpacity = 0.95,
              highlightOptions = highlightOptions(
                weight = 3,
                color = "#a9a9a9",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = lapply(labs, htmltools::HTML)) %>% 
  addMarkers(data = polygon_coordinates, lng = polygon_coordinates$polylong, 
             lat = polygon_coordinates$polylat, label = polygon_coordinates$polyname, group = "polygon_coordinates",
             icon = makeIcon(
               iconUrl = "https://leafletjs.com/examples/custom-icons/leaf-green.png",
               iconWidth = 0.01, iconHeight = 0.01)) %>% 
  leaflet.extras::addSearchFeatures(targetGroups= "polygon_coordinates",
                                    options = searchFeaturesOptions(
                                      zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
                                      autoCollapse = TRUE, hideMarkerOnCollapse = TRUE)) %>% 
  addResetMapButton() %>%
  addLegend(
    pal = pal, values = votedata$BallotsCast, title = "Turnout", opacity = 0.8, position = "bottomright",
    labFormat = labelFormat(between = "% &ndash; ", suffix = "%", transform = function(x) 100*x))

##Show choropleth
m

saveWidget(m, file="turnoutMap.html")

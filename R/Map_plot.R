#' @title Map_Plot
#' @description Plots filtered data onto an Open Street Map of Paris
#' @param start = a POSXct, end = a POSXct, duration_min = an int (default is 0), duration_max = an int (default=7200), districts = a vector (default=c("All")), cat_sites = idem, sites = idem, countries = idem, devices = idem
#' @import tidyr dplyr lubridate sp leaflet sf geojson
#' @return a dataframe
#' @export


Map_plot <- function(start, end, duration_min=0, duration_max=7200, districts=c("All"), cat_sites=c("All"), sites=c("All"), countries=c("All"), devices=c("All")) {

#Define the map data
Data_Map_plot <- Data_Map_Filter(start, end, duration_min, duration_max, districts, cat_sites, sites, countries, devices)
Data_Map_gps_catsite <- mapping_site_gps_catsite %>%  filter (Site %in% unique(Data_Map_plot$site))

#Define palets
pal_Ardt <- colorNumeric("Blues", NULL )

pal_Site <- colorFactor(c("navy", "red","purple","green","white","yellow"),
                        mapping_site_gps_catsite$category_site)

#Define description
description <- paste("Nom:", Data_Map_gps_catsite$Site, "<br>",
                     "Adresse:", Data_Map_gps_catsite$Adresse_Postale, "<br>",
                     "Arrondissement:", Data_Map_gps_catsite$Ardt,"<br>")

#Defining Map Variables
Lng=as.numeric(Data_Map_gps_catsite$y)
Lat=as.numeric(Data_Map_gps_catsite$x)

map <- leaflet(arrondissements_geojson, options = leafletOptions(minZoom = 11, maxZoom = 15)) %>%

  addTiles() %>%

  setView(lng =2.345228, lat = 48.862246, zoom = 11) %>%

  fitBounds(max(Lng), max(Lat), min(Lng), min(Lat)) %>%

  addPolygons(color = pal_Ardt, stroke = FALSE,
              smoothFactor = 0, fillOpacity = 0.2,group="Arrondissement") %>%

  addPolylines (group="Arrondissement",stroke = TRUE, weight = 3, opacity = 0.5) %>%

  addCircleMarkers(stroke = FALSE,

                   lng= ~Lng,
                   lat= ~Lat ,
                   radius = 4,

                   color = pal_Site(Data_Map_gps_catsite$category_site),
                   fillColor = pal_Site(Data_Map_gps_catsite$category_site),
                   fillOpacity = 4,

                   popup = description,
                   group = "Wifi Site") %>%

  addCircles(lng= ~Lng,
             lat= ~Lat ,
             radius = 100, group = "Wifi Coverage Zone (100m Radius)", stroke = TRUE, color = "black",
             opacity = 0.5, fill = TRUE, fillColor = "transparent", fillOpacity = 4, popup = description) %>%

  addCircleMarkers(stroke = FALSE,
                   lng= ~Lng,
                   lat= ~Lat ,
                   radius = 4,

                   color = pal_Site(Data_Map_gps_catsite$category_site),
                   fillColor = pal_Site(Data_Map_gps_catsite$category_site),
                   fillOpacity = 4,

                   popup = description,
                   group = "Wifi Clustering",
                   clusterOptions = markerClusterOptions(
                     showCoverageOnHover = FALSE, zoomToBoundsOnClick = TRUE,
                     spiderfyOnMaxZoom = TRUE, removeOutsideVisibleBounds = TRUE,
                     spiderLegPolylineOptions = list(weight = 1.5, color = "#222", opacity =
                                                       0.5), freezeAtZoom = FALSE)) %>%

  addLegend(pal = pal_Site, values = Data_Map_gps_catsite$category_site, position = "bottomright", title = 'Legend') %>%

  addLayersControl(
    baseGroups = c("Wifi Site", "Wifi Coverage Zone (100m Radius)","Wifi Clustering"),
      overlayGroups = c("Arrondissement")
    )

  return(map)
}

library(tidyr)
library(dplyr)
library(lubridate)
library(sp)
library(leaflet)
library(sf)
library(geojson)

start<-lubridate::ymd("2016-09-27")
end<-lubridate::ymd("2016-09-30")
duration_min<- 10
duration_max<- 5000
districts<-c("20","11")
cat_sites<-c("All")
sites<-c("All")
countries<-c("All")
devices<-c("smartphone","tablet")

Map_plot(start, end, duration_min, duration_max, districts, cat_sites, sites, countries, devices)



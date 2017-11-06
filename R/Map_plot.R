#' Plots a map
#'
#' @param a_set = a dataframe
#' @import tidyr dplyr lubridate sp leaflet sf geojson
#' @return a leaflet
#' @export
#'
#' @examples
#' \dontrun{
#' start<-lubridate::ymd("2016-09-27")
#' end<-lubridate::ymd("2016-09-30")
#' duration_min<- 0
#' duration_max<- 7200
#' districts<-c("20","11","18")
#' cat_sites<-c("Bibliothèque")
#' sites<-c("All")
#' countries<-c("All")
#' devices<-c("smartphone","table")
#' a_set<-Viz1_Filter(start, end, duration_min, duration_max, districts, cat_sites, sites, countries, devices)
#'
#' Map_plot(a_set)
#'
#' }
#'


Map_plot <- function(a_set) {

#Define the map data
Data_Map_plot <- a_set
Data_Map_gps_catsite <- mapping_site_gps_catsite %>%  filter (Site %in% unique(Data_Map_plot$site))

#Define palets
pal_Ardt <- colorNumeric("Blues", NULL )

pal_Site <- colorFactor(c("navy", "red","purple","green","orange","yellow"),
                        mapping_site_gps_catsite$category_site)

#Define description
description <- paste("Nom:", Data_Map_gps_catsite$Site, "<br>",
                     "Adresse:", Data_Map_gps_catsite$Adresse_Postale, "<br>",
                     "Arrondissement:", Data_Map_gps_catsite$Ardt,"<br>")

#Defining Map Variables
Lng=as.numeric(Data_Map_gps_catsite$y)
Lat=as.numeric(Data_Map_gps_catsite$x)

map <- leaflet(arrondissements_geojson, options = leafletOptions(minZoom = 11, maxZoom = 14)) %>%

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

  addCircles(lng= ~Lng,
             lat= ~Lat ,
             radius = 100, group = "Wifi Coverage Zone", stroke = TRUE, color = "black",
             opacity = 0.5, fill = TRUE, fillColor = "transparent", fillOpacity = 4, popup = description) %>%



  addLegend(pal = pal_Site, values = Data_Map_gps_catsite$category_site, position = "bottomleft",opacity = 5, labelFormat(transform = identity)) %>%

  addLayersControl(options = layersControlOptions(collapsed = FALSE),
    baseGroups = c("Wifi Site", "Wifi Clustering"),
      overlayGroups = c("Arrondissement","Wifi Coverage Zone")
    ) %>%

  hideGroup("Wifi Coverage Zone")


  return(map)
}

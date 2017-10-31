library(dplyr)
library(leaflet)
library(sp)
library(sf)
library(tidyr)
library(geojson)


Data_Map_2 <- mapping_site_gps_catsite %>%  filter (Site %in% Data_Map$site)

pal_Ardt <- colorNumeric("Blues", NULL)
pal_Site <- colorFactor(c("navy", "red","purple","yellow","white","green"),
                        mapping_site_gps_catsite$category_site)


map <- leaflet(arrondissements_geojson[arrondissements_geojson$Ardt==16]) %>%

  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%

  setView(lng =2.345228, lat = 48.862246, zoom = 12) %>%

  addPolygons(color = pal_Ardt, stroke = FALSE,
              smoothFactor = 0, fillOpacity = 0.3, group="Arrondissement") %>%

  addPolylines () %>%
  addCircleMarkers(stroke = FALSE,

                   lng=as.numeric(mapping_site_gps_catsite$y),
                   lat=as.numeric(mapping_site_gps_catsite$x),
                   radius = 5,

                   color = pal_Site(mapping_site_gps_catsite$category_site),
                   fillColor = pal_Site(mapping_site_gps_catsite$category_site),
                   fillOpacity = 5,
                   clusterOptions = markerClusterOptions(
                     showCoverageOnHover = FALSE, zoomToBoundsOnClick = TRUE,
                     spiderfyOnMaxZoom = TRUE, removeOutsideVisibleBounds = TRUE,
                     spiderLegPolylineOptions = list(weight = 1.5, color = "#222", opacity =
                                                       0.5), freezeAtZoom = FALSE),

                   popup =
                     paste("Nom:", mapping_site_gps_catsite$Site, "<br>",
                           "Adresse:", mapping_site_gps_catsite$Adresse_Postale, "<br>",
                           "Arrondissement:", mapping_site_gps_catsite$Ardt,"<br>") )

map

no_site_par_arrondissement<- mapping_site_gps_catsite %>% group_by(Ardt) %>% summarise ( count = n() )

lat = arrondissements_geojson[arrondissements_geojson$object==16,"geom_x_y1"]
long = arrondissements_geojson[arrondissements_geojson$object==16,"geom_x_y2"]

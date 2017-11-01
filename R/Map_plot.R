#' @title Map_Plot
#' @description Plots filtered data onto an Open Street Map of Paris
#' @param start = a POSXct, end = a POSXct, duration_min = an int (default is 0), duration_max = an int (default=7200), districts = a vector (default=c("All")), cat_sites = idem, sites = idem, countries = idem, devices = idem
#' @import tidyr dplyr lubridate sp leaflet sf geojson
#' @return a dataframe
#' @export


Map_plot <- function(start, end, duration_min=0, duration_max=7200, districts=c("All"), cat_sites=c("All"), sites=c("All"), countries=c("All"), devices=c("All")) {

pal_Ardt <- colorNumeric("Blues", NULL)
pal_Site <- colorFactor(c("navy", "red","purple","yellow","white","green"),
                          mapping_site_gps_catsite$category_site)

Data_Map_plot <- Data_Map_Filter(start, end, duration_min, duration_max, districts, cat_sites, sites, countries, devices)
Data_Map_gps_catsite <- mapping_site_gps_catsite %>%  filter (Site %in% unique(Data_Map_plot$site))

map <- leaflet(arrondissements_geojson) %>%
  addTiles() %>%
  setView(lng =2.345228, lat = 48.862246, zoom = 12) %>%
  addPolygons(color = pal_Ardt, stroke = FALSE,
              smoothFactor = 0, fillOpacity = 0.3) %>%
  addPolylines () %>%
  addCircleMarkers(stroke = FALSE,

                   lng=as.numeric(Data_Map_gps_catsite$y),
                   lat=as.numeric(Data_Map_gps_catsite$x),
                   radius = 5,

                   color = pal_Site(mapping_site_gps_catsite$category_site),
                   fillColor = pal_Site(mapping_site_gps_catsite$category_site),
                   fillOpacity = 5,
                   clusterOptions = markerClusterOptions(
                     showCoverageOnHover = FALSE, zoomToBoundsOnClick = TRUE,
                     spiderfyOnMaxZoom = TRUE, removeOutsideVisibleBounds = TRUE,
                     spiderLegPolylineOptions = list(weight = 1.5, color = "#222", opacity =
                                                       0.5), freezeAtZoom = FALSE),
                   popup = paste("Nom:", Data_Map_gps_catsite$Site, "<br>",
                           "Adresse:", Data_Map_gps_catsite$Adresse_Postale, "<br>",
                           "Arrondissement:", Data_Map_gps_catsite$Ardt,"<br>") )


return(map)
}
wifiparis::Map_plot(start, end, duration_min, duration_max, districts, cat_sites, sites, countries, devices)



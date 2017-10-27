devtools::use_data_raw()
devtools::use_data(wifi_connexion_data)
devtools::use_data(mapping_site_gps_catsite)
devtools::use_data(mapping_langues)
devtools::use_data(mapping_devices)
devtools::use_data(arrondissements_geojson)

devtools:: use_package("dplyr")
devtools:: use_package("tidyr")

devtools:: use_package("leaflet")
devtools::use_package("sp")
devtools::use_package("sf")
devtools:: use_package("geojson")

# Il faut écrire un .R pour les database



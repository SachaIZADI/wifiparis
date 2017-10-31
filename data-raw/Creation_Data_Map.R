library(dplyr)

data(mapping_devices)
data(mapping_langues)
data(mapping_site_gps_catsite)
data(wifi_connexion_data)

#Full DataBase
Data_Map <-wifi_connexion_data
Data_Map <-Data_Map %>% left_join(mapping_site_gps_catsite, by=c("site"="Site"))
Data_Map <- Data_Map %>% left_join(mapping_devices, by=c("os","browser","device"))
Data_Map <- Data_Map %>% left_join(mapping_langues, by=c("langue"="langage"))

#Removing unecessary columns
Data_Map <- subset(Data_Map, select= c("start_time", "duration", "Country", "category_device", "site", "Ardt","category_site"))

Data_Map <- Data_Map[c("start_time", "duration",
                       "Country", "category_device",
                       "site", "Ardt","category_site" )]


Data_Map<-Data_Map %>% unique()
devtools::use_data(Data_Map)



library(dplyr)

data(mapping_devices)
data(mapping_langues)
data(mapping_site_gps_catsite)
data(wifi_connexion_data)

Data_Viz_1<-wifi_connexion_data

Data_Viz_1$stop_time<-NULL
Data_Viz_1$input_octets<-NULL
Data_Viz_1$output_octets<-NULL
Data_Viz_1 <- Data_Viz_1 %>% left_join(mapping_devices, by=c("os","browser","device"))
Data_Viz_1$os<-NULL
Data_Viz_1$browser<-NULL
Data_Viz_1$device<-NULL
Data_Viz_1 <- Data_Viz_1 %>% left_join(mapping_langues, by=c("langue"="langage"))
Data_Viz_1$langue<-NULL
Data_Viz_1$langagebis <-NULL
Data_Viz_1<-Data_Viz_1 %>% left_join(mapping_site_gps_catsite, by=c("site"="Site"))
Data_Viz_1$Adresse_Postale<-NULL
Data_Viz_1$x<-NULL
Data_Viz_1$y<-NULL

Data_Viz_1 <- Data_Viz_1[c("start_time", "duration", "Country", "category_device", "site", "Ardt","category_site" )]
Data_Viz_1<-Data_Viz_1 %>% unique()

devtools::use_data(Data_Viz_1)

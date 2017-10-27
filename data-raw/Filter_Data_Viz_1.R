library(lubridate)

Viz1_Filter <-function(start, end, duration_max, districts, cat_sites, sites, countries, devices){
  ##### /!\  besoin de loader Data_Viz_1 dans la fct ?
  Data_Viz_1_Filter<-Data_Viz_1 %>% filter(date(start_time)>=start & date(start_time)<=end)
  Data_Viz_1_Filter <- Data_Viz_1_Filter %>% filter(duration<duration_max)
  if (districts != "All"){
    Data_Viz_1_Filter <- Data_Viz_1_Filter %>% filter(Ardt %in% districts)
  }
  if (cat_sites != "All"){
    Data_Viz_1_Filter <- Data_Viz_1_Filter %>% filter(category_site %in% cat_sites)
  }
  if (sites != "All"){
    Data_Viz_1_Filter <- Data_Viz_1_Filter %>% filter(site %in% sites)
  }
  if (countries != "All"){
    Data_Viz_1_Filter <- Data_Viz_1_Filter %>% filter(Country %in% countries)
  }
  if (devices != "All"){
    Data_Viz_1_Filter <- Data_Viz_1_Filter %>% filter(category_device %in% devices)
  }
  return(Data_Viz_1_Filter)
}

##Il faut traiter les cas districts == all

start<-ymd("2016-09-10")
end<-ymd("2016-09-11")
duration_max<- 5000
districts=c(1,2,5,8)
cat_sites=c("Mairie","Bibliothèque")
sites=c("Discothèque des Halles","Bibliothèque Port Royal","Bibliothèque Mouffetard")
countries=c("SPAIN","FRANCE")
devices=c("smartphone","tablet")



Viz1_Filter(start, end, duration_max, districts, cat_sites, sites, countries, devices) %>% View()
#Viz1_Filter <-function(start, end, duration_max, districts, cat_sites, sites, countries, devices){






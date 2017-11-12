library(dplyr)

#

start<-lubridate::ymd("2016-09-27")
end<-lubridate::ymd("2016-09-30")
duration_min<- 10
duration_max<- 5000
districts<-c(1,2,5,8,"All")
cat_sites<-c("Mairie","Bibliothèque")
sites<-c("All")
countries<-c("All")
devices<-c("smartphone","tablet")
analysis_axis<-"category_device"
a_set <- Viz1_Filter(start, end, duration_min, duration_max, districts, cat_sites, sites, countries, devices) %>% number_connexions(start,end,analysis_axis)

Viz1_plot_V2(a_set, start, end, analysis_axis, FALSE)



start<-lubridate::ymd("2016-09-27")
end<-lubridate::ymd("2016-09-30")
duration_min<- 10
duration_max<- 5000
districts<-c(1,2,5,8,"All")
cat_sites<-c("Mairie","Bibliothèque")
sites<-c("All")
 countries<-c("All")
 devices<-c("smartphone","tablet")
 analysis_axis<-"category_device"
 hierarchy_view<-FALSE
 a_set <- Viz1_Filter(start, end, duration_min, duration_max, districts, cat_sites, sites, countries, devices) %>%
 number_connexions(start,end,analysis_axis)

 Viz3_plot(a_set, analysis_axis, hierarchy_view)




 start<-lubridate::ymd("2016-09-27")
 end<-lubridate::ymd("2016-09-30")
 duration_min<- 0
 duration_max<- 7200
 districts<-c("20","11","18")
 cat_sites<-c("Bibliothèque")
 sites<-c("All")
 countries<-c("All")
 devices<-c("smartphone","table")
 a_set<-Viz1_Filter(start, end, duration_min, duration_max, districts, cat_sites, sites, countries, devices)

 Map_plot(a_set)




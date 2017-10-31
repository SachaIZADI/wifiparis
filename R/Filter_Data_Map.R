#' @title Filter_Data_Map
#' @description Filter the wifi_connexion_data DB joined with the mapping DB, according to the filters parametrized in the Shiny App
#' @param start = a POSIXct, end = a POSIXct, duration_min = an int (default is 0), duration_max = an int (default is 7200), districts, = a vector (default is c("All")) , cat_sites = idem, sites = idem, countries = idem, devices = idem
#' @import tidyr dplyr lubridate
#' @return a dataframe
#' @export

Data_Map_Filter <-function(start, end, duration_min=0, duration_max=7200,
                           districts=c("All"), cat_sites=c("All"), sites=c("All"),
                           countries=c("All"), devices=c("All"))
  {
  Data_Map_Filter<-Data_Map %>% filter(date(start_time)>=start & date(start_time)<=end)
  Data_Map_Filter <- Data_Map_Filter %>% filter(duration<duration_max & duration_min< duration)
  if (!"All" %in% districts){
    Data_Map_Filter <- Data_Map_Filter %>% filter(Ardt %in% districts)
  }
  if (!"All" %in% cat_sites){
    Data_Map_Filter <- Data_Map_Filter %>% filter(category_site %in% cat_sites)
  }
  if (!"All" %in% sites){
    Data_Map_Filter <- Data_Map_Filter %>% filter(site %in% sites)
  }
  if (!"All" %in% countries){
    Data_Map_Filter <- Data_Map_Filter %>% filter(Country %in% countries)
  }
  if (!"All" %in% devices){
    Data_Map_Filter <- Data_Map_Filter %>% filter(category_device %in% devices)
  }
  return(Data_Map_Filter)
}

#' Filter the wifi_connexion_data DB joined with the mapping DB, according to the filters parametrized in the Shiny App
#'
#' @param start = a POSIXct, end = a POSIXct, duration_min = an int (default is 0), duration_max = an int (default is 7200), districts = a vector (default is c("All")) , cat_sites = idem, sites = idem, countries = idem, devices = idem
#' @import tidyr dplyr lubridate
#' @return a dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' start<-lubridate::ymd("2016-09-10")
#' end<-lubridate::ymd("2016-09-12")
#' duration_min<- 10
#' duration_max<- 5000
#' districts<-c(1,2,5,8,"All")
#' cat_sites<-c("Mairie","Bibliothèque")
#' sites<-c("Discothèque des Halles","Bibliothèque Port Royal","Bibliothèque Mouffetard")
#' countries<-c("SPAIN","FRANCE")
#' devices<-c("smartphone","tablet")
#' Viz1_Filter(start, end, duration_max, districts, cat_sites, sites, countries, devices) %>% View()
#' }
#'

Viz1_Filter <-function(start, end, duration_min=0, duration_max=7200, districts=c("All"), cat_sites=c("All"), sites=c("All"), countries=c("All"), devices=c("All")){

  ## data("Data_Viz_1")
  ######## Si jamais il y a un pb, décommenter la ligne du haut. A-t-on vraiment besoin de loader la database ici à chaque fois, car c'est super long si on doit la loader ...

  Data_Viz_1_Filter<-Data_Viz_1 %>% filter(date(start_time)>=start & date(start_time)<=end)
  Data_Viz_1_Filter <- Data_Viz_1_Filter %>% filter(duration<duration_max & duration_min< duration)
  if (!"All" %in% districts){
    Data_Viz_1_Filter <- Data_Viz_1_Filter %>% filter(Ardt %in% districts)
  }
  if (!"All" %in% cat_sites){
    Data_Viz_1_Filter <- Data_Viz_1_Filter %>% filter(category_site %in% cat_sites)
  }
  if (!"All" %in% sites){
    Data_Viz_1_Filter <- Data_Viz_1_Filter %>% filter(site %in% sites)
  }
  if (!"All" %in% countries){
    Data_Viz_1_Filter <- Data_Viz_1_Filter %>% filter(Country %in% countries)
  }
  if (!"All" %in% devices){
    Data_Viz_1_Filter <- Data_Viz_1_Filter %>% filter(category_device %in% devices)
  }
  return(Data_Viz_1_Filter)
}


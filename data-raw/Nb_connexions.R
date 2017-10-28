#' Returns the number of connexions of a set vs. time
#'
#' @param a_set = a subset of the wifi_connexion_data set where at least start_time and stop_time are present
#' @import tidyr dplyr lubridate plotly
#' @return a dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' simultaneous_connexions(wifi_connexion_data)
#' }
#'
#'

number_connexions <- function(set, start, end){
  ### In case the time scope of the analysis is < 1 week >>> the nb of connexions is calculated by day
  if(as.integer(end-start)>7){
    nb_connex <- set %>%
      mutate(date=date(start_time)) %>%
      group_by(date) %>%
      mutate(nb_connexions=n())
  }else{ ### In the other case >>> the nb of connexions is calculated by hour
    nb_connex <- set %>%
      mutate(date=ymd_hms(paste(substr(as.character(start_time),1,nchar(as.character(start_time))-5),"00:00",sep=''))) %>%
      group_by(date) %>%
      mutate(nb_connexions=n())
  }
  return(nb_connex)
}

###### réécrire la doc




library(lubridate)
library(dplyr)

start<-ymd("2016-09-10")
end<-ymd("2016-09-17")
duration_max<- 5000
districts<-c(1,2,5,8,"All")
districts<-c(1,2,5,8)
cat_sites<-c("Mairie","Bibliothèque")
sites<-c("All")
countries<-c("All")
devices<-c("smartphone","tablet")


Viz1_Filter(start, end, duration_max, districts, cat_sites, sites, countries, devices) %>%
  number_connexions(start,end) %>%
  View()



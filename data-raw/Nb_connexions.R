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

number_connexions <- function(start, end, set){
  ### In case the time scope of the analysis is < 1 week >>> the nb of connexions is calculated by day
  if(as.integer(end-start)>7){
    nb_connexions <- set %>%
      mutate(date=date(start_time)) %>%
      group_by(date) %>%
      mutate(nb_connexions=n())
  }else{ ### In the other case >>> the nb of connexions is calculated by hour
    nb_connexions <- set %>%
      mutate(moment=ymd_hms(paste(substr(as.character(start_time),1,nchar(as.character(start_time))-5),"00:00",sep=''))) %>%
      group_by(moment) %>%
      mutate(nb_connexions=n())
  }
}

###### Tester cette fonction + réécrire la doc






start_time<-ymd_hms("2016-02-10 14:45:39")

start<-ymd("2016-09-10")
end<-ymd("2016-09-17")
Viz1_Filter(start, end, 7200, c("All"), c("All"), c("All"), c("All"), c("All")) %>%
  mutate(moment=ymd_hms(paste(substr(as.character(start_time),1,nchar(as.character(start_time))-5),"00:00",sep=''))) %>%
  group_by(moment) %>%
  mutate(nb_connexions=n()) %>% View()





wifi_connexion_data %>% filter(site=="Bibliothèque Yourcenar" |site=="Parc Monceau 1 (Entré)") %>%  number_connexions() %>% View()


p<-ggplot(data = wifi_connexion_data %>% filter(site=="Promenade Des Champs Elysees" |site=="Bibliothèque Yourcenar") %>%  number_connexions(),
       aes(x =date , y = nb_connexions, color=site)) +
  #geom_line(size = 0.3) +
  geom_smooth(method = 'loess',formula = y ~ x,span = 0.1, size = 0.4,se=FALSE)+
  #geom_smooth(method = 'loess',formula = y ~ x,span = 0.1, size = 0.4)+
  ggtitle("Number of daily connexions")+
  labs(y="Number of connexions",x = "Date")+
  theme_fivethirtyeight(base_family = "helvetica",base_size=10) +
  theme(legend.title=element_blank())
ggplotly(p)



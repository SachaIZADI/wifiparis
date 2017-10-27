#' Returns the number of connexions of a set vs. time
#'
#' @param a_set = a subset of the wifi_connexion_data set where at least start_time and stop_time are present
#' @import tidyr dplyr assertthat lubridate plotly
#' @return a dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' simultaneous_connexions(wifi_connexion_data)
#' }
#'
#'

number_connexions <- function(a_set){
  assert_that("start_time" %in% colnames(a_set))
  
  a_set %>% 
    mutate(date=date(start_time)) %>% 
    group_by(date,site) %>% 
    summarize(nb_connexions=n()) %>% 
    unique()
}


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



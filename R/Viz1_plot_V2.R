#' Plots the number of wifi connexions with respects to a given parametrization
#'
#' @param a_set = a dataset, start = a POSXct, end = a POSXct, duration_min = an int (default is 0), duration_max = an int (default=7200), districts = a vector (default=c("All")), cat_sites = idem, sites = idem, countries = idem, devices = idem, analysis_axis = a string of char to chose in ("None","Country","category_device","site","Ardt","category_site" - default="None"), complete_data = Boolean (default=FALSE)
#' @import tidyr dplyr lubridate ggplot2 ggthemes plotly
#' @return a dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' start<-lubridate::ymd("2016-09-27")
#' end<-lubridate::ymd("2016-09-30")
#' duration_min<- 10
#' duration_max<- 5000
#' districts<-c(1,2,5,8,"All")
#' cat_sites<-c("Mairie","Bibliothèque")
#' sites<-c("All")
#' countries<-c("All")
#' devices<-c("smartphone","tablet")
#' analysis_axis<-"category_device"
#' a_set <- Viz1_Filter(start, end, duration_min, duration_max, districts, cat_sites, sites, countries, devices) %>%
#' number_connexions(start,end,analysis_axis)
#'
#' Viz1_plot_V2(a_set, start, end, analysis_axis, TRUE)
#' }
#'
#'

Viz1_plot_V2 <-function(a_set, start, end, analysis_axis="None", complete_data=FALSE){
  data_Viz1_plot <- a_set

  if (analysis_axis == "None"){
    data_Viz1_plot<-data_Viz1_plot %>% select(date, nb_connexions) %>% unique()
    p<-ggplot(data = data_Viz1_plot,
              aes(x = date , y = nb_connexions))
  } else if(analysis_axis == "Country"){
    data_Viz1_plot<-data_Viz1_plot %>% select(date, nb_connexions, Country) %>% unique()
    p<-ggplot(data = data_Viz1_plot,
              aes(x = date , y = nb_connexions, color=Country))
  } else if(analysis_axis == "category_device"){
    data_Viz1_plot<-data_Viz1_plot %>% select(date, nb_connexions, category_device) %>% unique()
    p<-ggplot(data = data_Viz1_plot,
              aes(x = date , y = nb_connexions, color=category_device))
  } else if(analysis_axis == "site"){
    data_Viz1_plot<-data_Viz1_plot %>% select(date, nb_connexions, site) %>% unique()
    p<-ggplot(data = data_Viz1_plot,
              aes(x = date , y = nb_connexions, color=site))
  } else if(analysis_axis == "Ardt"){
    data_Viz1_plot<-data_Viz1_plot %>% select(date, nb_connexions, Ardt) %>% unique()
    data_Viz1_plot$Ardt<-as.factor(as.character(data_Viz1_plot$Ardt))
    p<-ggplot(data = data_Viz1_plot,
              aes(x = date , y = nb_connexions, color=Ardt)) ### PB ARDT
  } else if(analysis_axis == "category_site"){
    data_Viz1_plot<-data_Viz1_plot %>% select(date, nb_connexions, category_site) %>% unique()
    p<-ggplot(data = data_Viz1_plot,
              aes(x = date , y = nb_connexions, color=category_site))
  }

  #### Gérer le smoothering qui marche pas et passer complete_data à TRUE si besoin >>> FOIRE
  count<-data_Viz1_plot %>% ungroup() %>% select(date) %>% unique() %>% summarise(count=n())
  if (count$count[1]<=20){
    complete_data<-TRUE
  }

  if(complete_data){
    p<-p+geom_line(size = 0.3)
  } else {
    p<-p+geom_smooth(method = 'loess',formula = y ~ x,span = 0.20, size = 0.4,se=FALSE)
  }

  if(as.integer(end-start)>7){
    p<-p+ggtitle("Number of daily connexions")

  } else {
    p<-p+ggtitle("Number of hourly connexions")
  }

  p<-p +
    labs(y="Number of connexions",x = "Date")+
    theme_fivethirtyeight(base_family = "helvetica",base_size=10) +
    theme(legend.title=element_blank()) +
    #expand_limits(x=ymd_hms("2016-09-27 00:00:00"),y = 0) ####################
  scale_y_continuous(limits = c(0, as.integer(1.05*max(data_Viz1_plot$nb_connexions))), expand = c(0.01, 0))

  return(ggplotly(p))
}


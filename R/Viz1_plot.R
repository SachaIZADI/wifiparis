#' Plots the number of wifi connexions with respects to a given parametrization
#'
#' @param start = a POSXct, end = a POSXct, duration_max = an int (default=7200), districts = a vector (default=c("All")), cat_sites = idem, sites = idem, countries = idem, devices = idem, analysis_axis = a string of char to chose in ("None","Country","category_device","site","Ardt","category_site" - default="None"), complete_data = Boolean (default=FALSE)
#' @import tidyr dplyr lubridate ggplot2 ggthemes plotly
#' @return a dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' start<-ymd("2016-09-27")
#' end<-ymd("2016-09-30")
#' duration_max<- 5000
#' districts<-c(1,2,5,8,"All")
#' cat_sites<-c("Mairie","Bibliothèque")
#' sites<-c("All")
#' countries<-c("All")
#' devices<-c("smartphone","tablet")
#' analysis_axis<-"category_device"
#'
#' Viz1_plot(start, end, duration_max, districts, cat_sites, sites, countries, devices, analysis_axis, TRUE)
#' }
#'
#'


Viz1_plot <-function(start, end, duration_max=7200, districts=c("All"), cat_sites=c("All"), sites=c("All"), countries=c("All"), devices=c("All"), analysis_axis="None", complete_data=FALSE){
  data_Viz1_plot <- Viz1_Filter(start, end, duration_max, districts, cat_sites, sites, countries, devices) %>%
    number_connexions(start,end,analysis_axis)

  if (analysis_axis == "None"){
    p<-ggplot(data = data_Viz1_plot,
              aes(x = date , y = nb_connexions))
  } else if(analysis_axis == "Country"){
    p<-ggplot(data = data_Viz1_plot,
              aes(x = date , y = nb_connexions, color=Country))
  } else if(analysis_axis == "category_device"){
    p<-ggplot(data = data_Viz1_plot,
              aes(x = date , y = nb_connexions, color=category_device))
  } else if(analysis_axis == "site"){
    p<-ggplot(data = data_Viz1_plot,
              aes(x = date , y = nb_connexions, color=site))
  } else if(analysis_axis == "Ardt"){
    p<-ggplot(data = data_Viz1_plot,
              aes(x = date , y = nb_connexions, color=Ardt))
  } else if(analysis_axis == "category_site"){
    p<-ggplot(data = data_Viz1_plot,
              aes(x = date , y = nb_connexions, color=category_site))
  }

  #### Gérer le smoothering qui marche pas et passer complete_data à TRUE si besoin
  count<-data_Viz1_plot %>% ungroup() %>% select(date) %>% unique() %>% summarise(count=n())
  if (count$count[1]<=25){
    complete_data<-TRUE
  } else{
    p<-p+geom_smooth(method = 'loess',formula = y ~ x,span = 0.20, size = 0.4,se=FALSE)
  }

  if(complete_data){
    p<-p+geom_line(size = 0.3)
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

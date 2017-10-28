Viz1_plot <-function(start, end, duration_max=7200, districts=c("All"), cat_sites=c("All"), sites=c("All"), countries=c("All"), devices=c("All"), analysis_axis="None"){
  data_Viz1_plot <- Viz1_Filter(start, end, duration_max, districts, cat_sites, sites, countries, devices) %>%
    number_connexions(start,end)

  if (analysis_axis != "None"){

    if(!complete_data){
      p<-ggplot(data = data_Viz1_plot,
                aes(x = date , y = nb_connexions, color=analysis_axis)) +
        geom_smooth(method = 'loess',formula = y ~ x,span = 0.1, size = 0.4,se=FALSE)

    } else {
      p<-ggplot(data = data_Viz1_plot,
                aes(x = date , y = nb_connexions, color=analysis_axis)) +
        geom_line(size = 0.3) +
        geom_smooth(method = 'loess',formula = y ~ x,span = 0.1, size = 0.4,se=FALSE)
    }

  } else { ### /!\ si analysis_axis == "None" >>> il faut voir ce qu'il faut tracer
    if(!complete_data){
      p<-ggplot(data = data_Viz1_plot,
                aes(x = date , y = nb_connexions)) +
        geom_smooth(method = 'loess',formula = y ~ x,span = 0.1, size = 0.4,se=FALSE)

    } else {
      p<-ggplot(data = data_Viz1_plot,
                aes(x = date , y = nb_connexions, color=analysis_axis)) +
        geom_line(size = 0.3) +
        geom_smooth(method = 'loess',formula = y ~ x,span = 0.1, size = 0.4,se=FALSE)
    }
  }

  if(as.integer(end-start)>7){
    p<-p+ggtitle("Number of daily connexions")

  } else {
    p<-p+ggtitle("Number of hourly connexions")
  }

  p<-p +
    labs(y="Number of connexions",x = "Date")+
    theme_fivethirtyeight(base_family = "helvetica",base_size=10) +
    theme(legend.title=element_blank())

  return(ggplotly(p))
}


###### Ecrire des fonctions de test et gérer la viz dans le cas où c'est analysis_axis == None





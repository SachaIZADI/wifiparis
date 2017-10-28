Viz1_plot <-function(start, end, duration_max=7200, districts=c("All"), cat_sites=c("All"), sites=c("All"), countries=c("All"), devices=c("All"), analysis_axis="None", complete_data=FALSE){
  data_Viz1_plot <- Viz1_Filter(start, end, duration_max, districts, cat_sites, sites, countries, devices) %>%
    number_connexions(start,end,analysis_axis)

  if (analysis_axis != "None"){
    analysis_axis<-as.name(analysis_axis) ### A tester

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
                aes(x = date , y = nb_connexions)) +
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


start<-ymd("2016-09-01")
end<-ymd("2016-09-30")
duration_max<- 5000
districts<-c(1,2,5,8,"All")
cat_sites<-c("Mairie","Bibliothèque")
sites<-c("All")
countries<-c("All")
devices<-c("smartphone","tablet")
#analysis_axis<-"None"
analysis_axis<-"category_device"

Viz1_plot(start, end, duration_max, districts, cat_sites, sites, countries, devices, analysis_axis, FALSE)



#Il y a un pb avec le axis of analysis et avec le smooth (a besoin de plusieurs points)

### >>> Fonctionne avec analysis_axis="None" et complete_data=FALSE si beaucoup de données
### >>> Il faut au moins 21 points (le mieux : 1 mois ou 24h pour avoir le smoother qui fonctionne bien)

## lazy_eval : https://www.r-bloggers.com/data-frame-columns-as-arguments-to-dplyr-functions/












start<-ymd("2016-02-10")
end<-ymd("2016-09-20")
duration_max<- 5000
districts<-c(1,2,5,8,"All")
districts<-c(1,2,5,8)
cat_sites<-c("Mairie","Bibliothèque")
sites<-c("All")
countries<-c("All")
devices<-c("All")

data_Viz1_plot <- Viz1_Filter(start, end, duration_max, districts, cat_sites, sites, countries, devices) %>%
  number_connexions(start,end)

p<-ggplot(data = data_Viz1_plot,
          aes(x = date , y = nb_connexions, color=category_device)) +
  geom_line(size = 0.3) #+
#geom_smooth(method = 'loess',formula = y ~ x,span = 0.1, size = 0.4,se=FALSE)

p<-p+ggtitle("Number of daily connexions")

p<-p +
  labs(y="Number of connexions",x = "Date")+
  theme_fivethirtyeight(base_family = "helvetica",base_size=10) +
  theme(legend.title=element_blank())

ggplotly(p)

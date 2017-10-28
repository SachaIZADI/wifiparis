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
  count<-data_Viz1_plot %>% select(date) %>% unique() %>% summarise(count=n())
  if (count$count[1]<=23){
    complete_data<-TRUE
  } else{
    p<-p+geom_smooth(method = 'loess',formula = y ~ x,span = 0.1, size = 0.4,se=FALSE)
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
    theme(legend.title=element_blank())

  return(ggplotly(p))
}


###### Ecrire des fonctions de test et gérer la viz dans le cas où c'est analysis_axis == None


start<-ymd("2016-04-01")
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


#Adding missing grouping variables: `category_device`
#Passe le complete_data à TRUE systématiquement











start<-ymd("2016-02-10")
end<-ymd("2016-09-20")
duration_max<- 5000
districts<-c(1,2,5,8,"All")
districts<-c(1,2,5,8)
cat_sites<-c("Mairie","Bibliothèque")
sites<-c("All")
countries<-c("All")
devices<-c("All")

analysis_axis<-"category_device"

data_Viz1_plot <- Viz1_Filter(start, end, duration_max, districts, cat_sites, sites, countries, devices) %>%
  number_connexions(start,end,analysis_axis)

##########################################################
##########################################################
##########################################################
# analysis_axis<-as.name(analysis_axis) ### FOIRAGE COMPLET
##########################################################
##########################################################
##########################################################


p<-ggplot(data = data_Viz1_plot,
          aes(x = date , y = nb_connexions, color=as_name(analysis_axis))) +
  geom_line(size = 0.3) +
  ggtitle("Number of daily connexions") +
  labs(y="Number of connexions",x = "Date") +
  theme_fivethirtyeight(base_family = "helvetica",base_size=10) +
  theme(legend.title=element_blank())

ggplotly(p)

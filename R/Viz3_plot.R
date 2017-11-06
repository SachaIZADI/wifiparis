#' Plots a treemap with size = to average number of connexions & color = connexion duration with respect to a given analysis axis
#'
#' @param a_set = a dataframe, analysis_axis = a defined string of characters (default is "None"), hierarchy_view = a boolean
#' @import dplyr treemap d3treeR
#' @return a treemap object
#' @export
#'
#' @examples
#' \dontrun{
#'
#' A MODIFIER
#'
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
#' hierarchy_view<-FALSE
#' a_set <- Viz1_Filter(start, end, duration_min, duration_max, districts, cat_sites, sites, countries, devices) %>%
#' number_connexions(start,end,analysis_axis)
#'
#' Viz3_plot(a_set, analysis_axis, hierarchy_view)
#'
#' }
#'

# filtering the data using Viz1_Filter function and calculating nb of connexion using
# number of connexion function.

Viz3_plot <- function(a_set, analysis_axis = "None", hierarchy_view){

  data_Viz3_plot <- a_set

  # Removing hours (in case of date scope < 1 week)
  data_Viz3_plot$date <- as.Date(data_Viz3_plot$date)


  #Creating a database adapted to the choosen treemap.

  if (analysis_axis == "None") {

    data = data_Viz3_plot %>%  mutate( None = "None") %>% select(date, None, duration, nb_connexions) %>% group_by(date,None) %>%
      summarise(nb_connexions = mean(as.numeric(nb_connexions)), duration = mean(as.numeric(duration))) %>% group_by(None) %>%
      summarise(nb_connexions = mean(as.numeric(nb_connexions)), duration = mean(as.numeric(duration)))

    p<-d3tree3(treemap(data, index= "None",  vSize= "nb_connexions"),
               rootname = paste("Average nb of connexions  : " , as.character(data$nb_connexions[1])) )


  }


  if(analysis_axis == "Country"){

    if (hierarchy_view == FALSE) {
      data = data_Viz3_plot %>% select(date, Country , duration, nb_connexions) %>% group_by(date,Country) %>%
        summarise(nb_connexions = mean(as.numeric(nb_connexions)), duration = mean(as.numeric(duration))) %>% group_by(Country) %>%
        summarise(nb_connexions = mean(as.numeric(nb_connexions)), duration = mean(as.numeric(duration)))

      if (length(data$nb_connexions)==1){

        p<-d3tree3(treemap(data, index= "Country",  vSize= "nb_connexions"),
                   rootname = paste("Average nb of connexions  : " , as.character(data$nb_connexions[1])))
      }

      else {

        p<-d3tree3(treemap(data, index= "Country",  vSize= "nb_connexions", vColor = "duration", type = "value"),
                   rootname = "Average nb of connexions")

      }


    }

    else {

      data = data_Viz3_plot %>% select(date, Country , duration, nb_connexions) %>% group_by(date,Country) %>%
        summarise(nb_connexions = mean(as.numeric(nb_connexions)), duration = mean(as.numeric(duration))) %>% group_by(Country) %>%
        summarise(nb_connexions = mean(as.numeric(nb_connexions)), duration = mean(as.numeric(duration))) %>% full_join(mapping_country_region, by = "Country")

      if (length(data$nb_connexions)==1){

        p<-d3tree3(treemap(data, index= c("Region","Country"),  vSize= "nb_connexions"),
                   rootname = paste("Average nb of connexions  : " , as.character(data$nb_connexions[1])))
      }

      else {

        p<-d3tree3(treemap(data, index= c("Region","Country"),  vSize= "nb_connexions", vColor = "duration", type = "value"),
                   rootname = "Average nb of connexions")

      }


    }


  } else if(analysis_axis == "category_device"){


    data = data_Viz3_plot %>% select(date, category_device , duration, nb_connexions) %>% group_by(date,category_device) %>%
      summarise(nb_connexions = mean(as.numeric(nb_connexions)), duration = mean(as.numeric(duration))) %>% group_by(category_device) %>%
      summarise(nb_connexions = mean(as.numeric(nb_connexions)), duration = mean(as.numeric(duration)))

    if (length(data$nb_connexions)==1){

      p<-d3tree3(treemap(data, index= "category_device",  vSize= "nb_connexions"),
                 rootname = paste("Average nb of connexions  : " , as.character(data$nb_connexions[1])))
    }

    else {

      p<-d3tree3(treemap(data, index= "category_device",  vSize= "nb_connexions", vColor = "duration", type = "value"),
                 rootname = "Average nb of connexions")

    }





  } else if(analysis_axis == "site"){

    data = data_Viz3_plot %>% select(date, site , duration, nb_connexions) %>% group_by(date,site) %>%
      summarise(nb_connexions = mean(as.numeric(nb_connexions)), duration = mean(as.numeric(duration))) %>% group_by(site) %>%
      summarise(nb_connexions = mean(as.numeric(nb_connexions)), duration = mean(as.numeric(duration)))

    if (length(data$nb_connexions)==1){


      p<-d3tree3(treemap(data, index= "site",  vSize= "nb_connexions"),
                 rootname = paste("Average nb of connexions  : " , as.character(data$nb_connexions[1])))
    }

    else {

      p<-d3tree3(treemap(data, index= "site",  vSize= "nb_connexions", vColor = "duration", type = "value"),
                 rootname = "Average nb of connexions")

    }





  } else if(analysis_axis == "Ardt"){

    if (hierarchy_view==FALSE){

      data = data_Viz3_plot %>% select(date, Ardt , duration, nb_connexions) %>% group_by(date,Ardt) %>%
        summarise(nb_connexions = mean(as.numeric(nb_connexions)), duration = mean(as.numeric(duration))) %>% group_by(Ardt) %>%
        summarise(nb_connexions = mean(as.numeric(nb_connexions)), duration = mean(as.numeric(duration)))



      if (length(data$nb_connexions)==1){


        p<-d3tree3(treemap(data, index= "Ardt",  vSize= "nb_connexions"),
                   rootname = paste("Average nb of connexions  : " , as.character(data$nb_connexions[1])))
      }

      else {

        p<-d3tree3(treemap(data, index= "Ardt",  vSize= "nb_connexions", vColor = "duration", type = "value"),
                   rootname = "Average nb of connexions")

      }


    }

    else {

      data =  data_Viz3_plot %>% select(date, Ardt , site, duration, nb_connexions) %>% group_by(date,Ardt,site) %>%
        summarise(nb_connexions = mean(as.numeric(nb_connexions)), duration = mean(as.numeric(duration))) %>% group_by(Ardt,site) %>%
        summarise(nb_connexions = mean(as.numeric(nb_connexions)), duration = mean(as.numeric(duration)))




      if (length(data$nb_connexions)==1){


        p<-d3tree3(treemap(data, index= c("Ardt", "site"),  vSize= "nb_connexions"),
                   rootname = paste("Average nb of connexions  : " , as.character(data$nb_connexions[1])))
      }

      else {

        p<-d3tree3(treemap(data, index= c("Ardt", "site"),  vSize= "nb_connexions", vColor = "duration", type = "value"),
                   rootname = "Average nb of connexions")

      }

    }





  } else if(analysis_axis == "category_site"){

    if (hierarchy_view==FALSE) {

      data = data_Viz3_plot %>% select(date, category_site , duration, nb_connexions) %>% group_by(date,category_site) %>%
        summarise(nb_connexions = mean(as.numeric(nb_connexions)), duration = mean(as.numeric(duration))) %>% group_by(category_site) %>%
        summarise(nb_connexions = mean(as.numeric(nb_connexions)), duration = mean(as.numeric(duration)))

      if (length(data$nb_connexions)==1){


        p<-d3tree3(treemap(data, index= "category_site",  vSize= "nb_connexions"),
                   rootname = paste("Average nb of connexions  : " , as.character(data$nb_connexions[1])))
      }

      else {

        p <- d3tree3(treemap(data, index= "category_site",  vSize= "nb_connexions", vColor = "duration", type = "value"),
                     rootname = "Average nb of connexions")

      }


    }

    else {

      data = data_Viz3_plot %>% select(date, category_site , site, duration, nb_connexions) %>% group_by(date,category_site,site) %>%
        summarise(nb_connexions = mean(as.numeric(nb_connexions)), duration = mean(as.numeric(duration))) %>% group_by(category_site,site) %>%
        summarise(nb_connexions = mean(as.numeric(nb_connexions)), duration = mean(as.numeric(duration)))

      if (length(data$nb_connexions)==1){


        p<-d3tree3(treemap(data, index= c("category_site", "site") ,  vSize= "nb_connexions"),
                   rootname = paste("Average nb of connexions  : " , as.character(data$nb_connexions[1])))
      }

      else {

        p<-d3tree3(treemap(data, index= c("category_site", "site") ,  vSize= "nb_connexions", vColor = "duration", type = "value"),
                   rootname = "Average nb of connexions")

      }


    }



  }

  return(p)


}

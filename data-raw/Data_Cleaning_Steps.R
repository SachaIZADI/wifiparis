###### The aim of this file is to give as complete an overview as possible of what we did in terms of data cleaning before being able to exploit the data set ########







#################################################################################
################### First cleaning steps of the original DB : ###################
#################################################################################

## Completing the dataset
wifi_connexion_data$stop_time <- wifi_connexion_data$start_time + wifi_connexion_data$duration

## connexions are limited to 2hours, the other connexions are irrelevant to our study
wifi_connexion_data <- wifi_connexion_data %>% filter(duration<=7203)
wifi_connexion_data$duration[wifi_connexion_data$duration>7200]<-7200

## "NULL"-sites are irrelevant to our study
wifi_connexion_data <- wifi_connexion_data %>% filter(site!="NULL")

## Bibliothèque de prêt has no record on the internet
wifi_connexion_data<-wifi_connexion_data %>% filter(site!="Bibliothèque de prÃƒÆ’Ã‚Âªt")
wifi_connexion_data<-wifi_connexion_data %>% filter(site!="Bibliothèque de prêt")

## We extract the arrondissement number rather than working with the full zipcode
mapping_site_gps_catsite$Code_postal <- mapping_site_gps_catsite$Code_postal%%75100
mapping_site_gps_catsite$Code_postal <- mapping_site_gps_catsite$Code_postal%%75000
mapping_site_gps_catsite<-mapping_site_gps_catsite %>% rename("Ardt"="Code_postal")

## We clean the database to keep only relevant information
arrondissements_geojson$c_ar<-NULL
arrondissements_geojson$c_arinsee<-NULL
arrondissements_geojson$l_ar<-NULL
arrondissements_geojson %>% View()








#################################################################################
################# Creating a database for mapping the sites : ###################
#################################################################################


######## Creating a database for mapping the sites :

site_data <- mapping_site_gps %>% mutate(category_site=NA)
site_data$category_site<-NA

######## Creating a word cloud to understand the data :

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# Charger les données comme un corpus
docs <- Corpus(VectorSource(site_data$Site))
inspect(docs)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

##Completing the category ## grepl(search, in)

#if site contains Square/Parc/jardin/promenade/Berge --> Espaces verts : Square,Parc,Jardin,Promenade,Berge

site_data$category_site[grepl("Square", site_data$Site)] <- "Espace ouvert/vert"
site_data$category_site[grepl("SQUARE", site_data$Site)] <- "Espace ouvert/vert"
site_data$category_site[grepl("square", site_data$Site)] <- "Espace ouvert/vert"
site_data$category_site[grepl("parc", site_data$Site)] <- "Espace ouvert/vert"
site_data$category_site[grepl("Parc", site_data$Site)] <- "Espace ouvert/vert"
site_data$category_site[grepl("jardin", site_data$Site)] <- "Espace ouvert/vert"
site_data$category_site[grepl("Jardin", site_data$Site)] <- "Espace ouvert/vert"
site_data$category_site[grepl("Promenade", site_data$Site)] <- "Espace ouvert/vert"
site_data$category_site[grepl("Berges de Seine", site_data$Site)] <- "Espace ouvert/vert"
site_data$category_site[grepl("Berge de Seine", site_data$Site)] <- "Espace ouvert/vert"
## Special cases  :
site_data$category_site[site_data$Site== "Allee Vivaldi"] <- "Espace ouvert/vert"
site_data$category_site[site_data$Site== "TOUR ST JACQUES"] <- "Espace ouvert/vert"
site_data$category_site[site_data$Site== "Ecole du Breuil"] <- "Espace ouvert/vert"
site_data$category_site[site_data$Site== "Place de la république coté Ouest"] <- "Espace ouvert/vert"
site_data$category_site[site_data$Site== "Place de la république coté Est"] <- "Espace ouvert/vert"
site_data$category_site[site_data$Site== "Esplanade Des Invalides"] <- "Espace ouvert/vert"



#if site contains bibliothèqe/médiathèque/ discothèque --> Bibliothèque = biliothèque, médiathèque, discothèque

site_data$category_site[grepl("Bibliothèque", site_data$Site)] <- "Bibliothèque"
site_data$category_site[grepl("bibliothèque", site_data$Site)] <- "Bibliothèque"
site_data$category_site[grepl("Bibliotheque", site_data$Site)] <- "Bibliothèque"
site_data$category_site[grepl("bibliothèque", site_data$Site)] <- "Bibliothèque"
site_data$category_site[grepl("Médiathèque", site_data$Site)] <- "Bibliothèque"
site_data$category_site[grepl("MEDIATHEQUE", site_data$Site)] <- "Bibliothèque"
site_data$category_site[grepl("Mediatheque", site_data$Site)] <- "Bibliothèque"


## Special cases  :
site_data$category_site[site_data$Site== "Archives de Paris"] <- "Bibliothèque"
site_data$category_site[site_data$Site== "Discothèque des Halles"] <- "Bibliothèque"


#if site contains Mairie/ Hotel --> Mairie = Mairie and Hotel de Ville

site_data$category_site[grepl("Mairie", site_data$Site)] <- "Mairie"
site_data$category_site[grepl("mairie", site_data$Site)] <- "Mairie"
site_data$category_site[grepl("Hotel", site_data$Site)] <- "Mairie"


## Special cases  :
site_data$category_site[site_data$Site== "Maison Communale du 15 ̬me"] <- "Mairie"
site_data$category_site[site_data$Site== "Maison Communale du 15"] <- "Mairie"
site_data$category_site[site_data$Site== "DSTI"] <- "Mairie"


#if site contains Maison des Associations/Entreprises/Etudiantes/Emploi --> Maison d'Accompagnement

site_data$category_site[grepl("Maison des Associations", site_data$Site)] <- "Maison d'accompagnement"
## Special cases  :
site_data$category_site[site_data$Site== "Maison Du Developpement Economique Du 20eme"] <- "Maison d'acompagnement"
site_data$category_site[site_data$Site== "MAISON DES INITIATIVES"] <- "Maison d'acompagnement"
site_data$category_site[site_data$Site== "Maison de L'Emploi Du 18eme"] <- "Maison d'acompagnement"
site_data$category_site[site_data$Site== "Maison des Initiatives Etudiantes du 06eme"] <- "Maison d'acompagnement"
site_data$category_site[site_data$Site== "MAISON DES ENTREPRISES ET DE L'EMPLOI DU 10eme"] <- "Maison d'acompagnement"
site_data$category_site[site_data$Site== "Maison de L'Emploi Du 13eme"] <- "Maison d'acompagnement"
site_data$category_site[site_data$Site== "Maison Du Developpement Economique Du 19eme"] <- "Maison d'acompagnement"
site_data$category_site[site_data$Site== "Espace Economie Emploi Du 14eme"] <- "Maison d'acompagnement"
site_data$category_site[site_data$Site== "MAISON DES ENTREPRISES ET DE L'EMPLOI DU 10em"] <- "Maison d'acompagnement"
site_data$category_site[site_data$Site== "Bourse Du Travail"] <- "Maison d'acompagnement"
site_data$category_site[site_data$Site== "Antenne Jeune Orillon"] <- "Maison d'acompagnement"
site_data$category_site[site_data$Site== "Maison des Acteurs du Paris durable"] <- "Maison d'acompagnement"
site_data$category_site[site_data$Site== "Maison Initiatives Etudiantes"] <- "Maison d'acompagnement"
site_data$category_site[site_data$Site== "Antenne Jeune Davout"] <- "Maison d'acompagnement"
site_data$category_site[site_data$Site== "Conseil de la Jeunesse"] <- "Maison d'acompagnement"




#if site contains Centre/musée --> Centres d'animation et culturel

site_data$category_site[grepl("Centre", site_data$Site)] <- "Centre d'animation et culturel"
site_data$category_site[grepl("Musee", site_data$Site)] <- "Centre d'animation et culturel"
site_data$category_site[site_data$Site== "Maison de Victor Hugo"] <- "Centre d'animation et culturel"
site_data$category_site[site_data$Site== "Carre Baudoin"] <- "Centre d'animation et culturel"
site_data$category_site[site_data$Site== "Espace des Blancs Manteaux"] <- "Centre d'animation et culturel"
site_data$category_site[site_data$Site== "Maison de Balzac"] <- "Centre d'animation et culturel"
site_data$category_site[site_data$Site== "Espace Reuilly"] <- "Centre d'animation et culturel"
site_data$category_site[site_data$Site== "Atelier de Restauration Photographique"] <- "Centre d'animation et culturel"
site_data$category_site[site_data$Site== "E.C.A Rive Gauche (Espace commerce Culturel)"] <- "Centre d'animation et culturel"
site_data$category_site[site_data$Site== "Activites artisanales et creation"] <- "Centre d'animation et culturel"


#if site contains sportifs/

site_data$category_site[grepl("Sportif", site_data$Site)] <- "Centre de sport et bien être"
site_data$category_site[grepl("Stade", site_data$Site)] <- "Centre de sport et bien être"
site_data$category_site[grepl("Solarium", site_data$Site)] <- "Centre de sport et bien être"
site_data$category_site[site_data$Site== "HALLE GEORGES CARPENTIER TENNIS DE TABLE"] <- "Centre de sport et bien être"
site_data$category_site[site_data$Site== "PISCINE GEORGES VALLEREY"] <- "Centre de sport et bien être"
site_data$category_site[site_data$Site== "HALLE GEORGES CARPENTIER ANNEXE HALL"] <- "Centre de sport et bien être"
site_data$category_site[site_data$Site== "HALLE GEORGES CARPENTIER"] <- "Centre de sport et bien être"
site_data$category_site[site_data$Site== "Institut du Judo"] <- "Centre de sport et bien être"

#ENNNNNNNND !!!

# cleaner une ligne de NA et renommer la database :

mapping_site_gps_catsite <- site_data  %>% filter(!is.na(category_site))

## cleaner les categories with factors

mapping_site_gps_catsite$category_site<-as.factor(mapping_site_gps_catsite$category_site)
save(mapping_site_gps_catsite, file = "mapping_site_gps_catsite",compress = TRUE)
load("mapping_site_gps_catsite")
View(mapping_site_gps_catsite)




#################################################################################
################# Creating a database for mapping the devices : #################
#################################################################################

#Adding a column about the category of the device
device_data <- full_data_base %>% select(os, browser, device) %>% unique() %>% mutate(category_device=NA)
device_data$category_device<-NA

##Completing the category ## grepl(search, in)
#if browser == Chrome Mobile ---> Smartphone
device_data$category_device[device_data$browser=="Chrome Mobile"] <- "smartphone"

#if browser == Chrome & OS == Android ---> Tablet
device_data$category_device[device_data$browser=="Chrome" & device_data$os=="Android"] <- "tablet"

#if device contains iPhone --> smartphone
device_data$category_device[grepl("iPhone", device_data$device)] <- "smartphone"
device_data$category_device[grepl("IPHONE", device_data$device)] <- "smartphone"

#if device contains Mac --> computer
device_data$category_device[grepl("Mac", device_data$device)] <- "computer"

#if OS contains Phone --> smartphone
device_data$category_device[grepl("Phone", device_data$os)] <- "smartphone"

#if OS contains Mac & device == Other --> computer
device_data$category_device[grepl("Mac", device_data$os) & device_data$device == "Other"] <- "computer"

#in all the device not treated yet (category_device == NA), if they contain "Windows" --> computer
device_data$category_device[grepl("Windows", device_data$os) & is.na(device_data$category_device)] <- "computer"

#if device contains iPad --> tablet
device_data$category_device[grepl("iPad", device_data$device)] <- "tablet"

#if device contains TAB, tab, Tab (& not treated yet) --> tablet
device_data$category_device[grepl("tab", device_data$device) & is.na(device_data$category_device)] <- "tablet"
device_data$category_device[grepl("TAB", device_data$device) & is.na(device_data$category_device)] <- "tablet"
device_data$category_device[grepl("Tab", device_data$device) & is.na(device_data$category_device)] <- "tablet"


#if device contains PAD, pad, Pad (& not treted yet) --> tablet
device_data$category_device[grepl("pad", device_data$device) & is.na(device_data$category_device)] <- "tablet"
device_data$category_device[grepl("PAD", device_data$device) & is.na(device_data$category_device)] <- "tablet"
device_data$category_device[grepl("Pad", device_data$device) & is.na(device_data$category_device)] <- "tablet"


#we must have covered most of the tablets in the dataset
#if os = Android & browser = Android --> smartphone
device_data$category_device[device_data$os=="Android" & device_data$browser=="Android" & is.na(device_data$category_device)] <- "smartphone"


#treating the BB case --> smartphone & tablet(1)
device_data$category_device[device_data$os=="BlackBerry Tablet OS"] <- "tablet"
device_data$category_device[grepl("Black",device_data$os) & is.na(device_data$category_device)] <- "smartphone"

#if device contains phone, Phone or PHONE --> smartphone
device_data$category_device[grepl("phone",device_data$device) & is.na(device_data$category_device)] <- "smartphone"
device_data$category_device[grepl("Phone",device_data$device) & is.na(device_data$category_device)] <- "smartphone"
device_data$category_device[grepl("PHONE",device_data$device) & is.na(device_data$category_device)] <- "smartphone"

#if device contains Kindle --> tablet
device_data$category_device[grepl("Kindle",device_data$device)] <- "tablet"

#Most of the remaining Android are smartphones
device_data$category_device[device_data$os=="Android" & is.na(device_data$category_device)] <- "smartphone"

#if device contains Nokia --> smartphone
device_data$category_device[grepl("Nokia",device_data$device) & is.na(device_data$category_device)] <- "smartphone"

#if os == Bada --> smartphone
#if os == Ubuntu --> computer
#if os == FreeBSD --> computer
device_data$category_device[device_data$os=="Bada" & is.na(device_data$category_device)] <- "smartphone"
device_data$category_device[device_data$os=="Ubuntu" & is.na(device_data$category_device)] <- "computer"
device_data$category_device[device_data$os=="FreeBSD" & is.na(device_data$category_device)] <- "computer"

#if os==Linux & device==Other --> computer
#remaining os==Linux --> smartphone
device_data$category_device[device_data$os=="Linux" & is.na(device_data$category_device) & device_data$device=="Other"] <- "computer"
device_data$category_device[device_data$os=="Linux" & is.na(device_data$category_device)] <- "smartphone"

#if device==other --> other
device_data$category_device[device_data$device=="Other" & is.na(device_data$category_device)] <- "other"

#if device contains PlayStation or device contains iPod --> other
device_data$category_device[grepl("PlayStation",device_data$device)] <- "other"
device_data$category_device[grepl("iPod",device_data$device)] <- "other"

#if os == Mac OS X or  os == iOS --> computer or smartphone
device_data$category_device[device_data$os=="Mac OS X" & is.na(device_data$category_device)] <- "computer"
device_data$category_device[device_data$os=="iOS" & is.na(device_data$category_device)] <- "smartphone"

#Remove NA
device_data<-device_data %>% filter(!is.na(device))

device_data %>% filter(is.na(category_device)) %>% View()
mapping_devices<-device_data


#####

#if device contains Samsung SM-T  --> tablet
mapping_devices$category_device[grepl("Samsung SM-T",mapping_devices$device) & is.na(mapping_devices$category_device)] <- "tablet"

#if device contains Samsung SM-P  --> tablet
mapping_devices$category_device[grepl("Samsung SM-P",mapping_devices$device) & is.na(mapping_devices$category_device)] <- "tablet"

#all other Samsung SM-..  --> smartphones
mapping_devices$category_device[grepl("Samsung SM",mapping_devices$device) & is.na(mapping_devices$category_device)] <- "smartphone"

#all Samsung GT-..  --> smartphones
mapping_devices$category_device[grepl("Samsung GT",mapping_devices$device) & is.na(mapping_devices$category_device)] <- "smartphone"

#all remaining Samsung --> smartphones
mapping_devices$category_device[grepl("Samsung",mapping_devices$device) & is.na(mapping_devices$category_device)] <- "smartphone"

#if device contains Archos 101 or Archos 80  --> tablet // all others --> smartphone
mapping_devices$category_device[grepl("Archos 101",mapping_devices$device) & is.na(mapping_devices$category_device)] <- "tablet"
mapping_devices$category_device[grepl("Archos 80",mapping_devices$device) & is.na(mapping_devices$category_device)] <- "tablet"
mapping_devices$category_device[grepl("Archos",mapping_devices$device) & is.na(mapping_devices$category_device)] <- "smartphone"


#if device == Spider --> computer (but Spider can be either a computer or a smartphone)
mapping_devices$category_device[mapping_devices$device=="Spider" & is.na(mapping_devices$category_device)] <- "computer"


#All remaining HTC --> smartphone
mapping_devices$category_device[grepl("HTC",mapping_devices$device) & is.na(mapping_devices$category_device)] <- "smartphone"

#if device contains Lenovo 13500 or A5500 or A7600 --> tablet // others --> smartphone
mapping_devices$category_device[grepl("Lenovo 13500",mapping_devices$device) & is.na(mapping_devices$category_device)] <- "tablet"
mapping_devices$category_device[grepl("Lenovo A5500",mapping_devices$device) & is.na(mapping_devices$category_device)] <- "tablet"
mapping_devices$category_device[grepl("Lenovo A7600",mapping_devices$device) & is.na(mapping_devices$category_device)] <- "tablet"
mapping_devices$category_device[grepl("Lenovo",mapping_devices$device) & is.na(mapping_devices$category_device)] <- "smartphone"

#All remaining Sony --> smartphone
mapping_devices$category_device[grepl("Sony",mapping_devices$device) & is.na(mapping_devices$category_device)] <- "smartphone"

#All remaining LG --> smartphone
mapping_devices$category_device[grepl("LG",mapping_devices$device) & is.na(mapping_devices$category_device)] <- "smartphone"


#Asus Transformer Prime TF201 --> computer
#Asus Live --> smartphone
#Asus K00L --> tablet
mapping_devices$category_device[grepl("Asus Transformer Prime TF201",mapping_devices$device) & is.na(mapping_devices$category_device)] <- "computer"
mapping_devices$category_device[grepl("Asus Live",mapping_devices$device) & is.na(mapping_devices$category_device)] <- "smartphone"
mapping_devices$category_device[grepl("Asus K00L",mapping_devices$device) & is.na(mapping_devices$category_device)] <- "tablet"

#Alcatel--> smartphone
mapping_devices$category_device[grepl("Alcatel",mapping_devices$device) & is.na(mapping_devices$category_device)] <- "smartphone"

#AppleTV --> other
#iOS-Device --> other
#Nintendo DSi --> other
mapping_devices$category_device[grepl("AppleTV",mapping_devices$device) & is.na(mapping_devices$category_device)] <- "other"
mapping_devices$category_device[grepl("iOS-Device",mapping_devices$device) & is.na(mapping_devices$category_device)] <- "other"
mapping_devices$category_device[grepl("Nintendo DSi",mapping_devices$device) & is.na(mapping_devices$category_device)] <- "other"

#All remaining --> smartphone
mapping_devices$category_device[is.na(mapping_devices$category_device)] <- "smartphone"



#ENNNNNNNND !!!


mapping_devices %>% filter(is.na(category_device)) %>% View()


## cleaner les categories with factors

mapping_devices$category_device<-as.factor(mapping_devices$category_device)

save(mapping_devices, file = "mapping_devices",compress = TRUE)
load("mapping_devices")
mapping_devices %>% View()


#################################################################################
#################################################################################
#################################################################################


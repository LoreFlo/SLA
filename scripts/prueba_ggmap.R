################################################################################
############### Cuantificacion de "urbano" y "rural" ##########################
################################################################################

### Cargar paquetes

#install.packages("ggplot2")
#install.packages("ggmap")
install.packages("tidyverse")

library(tidyverse)
library(ggmap)
library(tibble)

register_google(key = "AIzaSyBrKEbmKUhlCOBMD9QNqFcRvdjZAw3WrwE")

Pue <- geocode("Puebla, Mexico", zoom = 13)
Pue
ggmap(get_map(Pue, maptype = "roadmap")) 

# Source : https://maps.googleapis.com/maps/api/geocode/json?address=Puebla,+Mexico&key=xxx
###Warning: Geocoding "Puebla, Mexico" failed with error: 
###You must enable Billing on the Google Cloud Project at https://console.cloud.google.com/project/_/billing/enable Learn more at https://developers.google.com/maps/gmp-get-started



# Description -------------------------------------------------------------

# This R script prepares a map of biogas reactor locations

# Code --------------------------------------------------------------------

## load libraries

library(dplyr)
library(tidyr)
library(leaflet)
library(mapview)

## read data

locations <- readr::read_csv("data/final-sample-coordinates.csv")

locations_tidy <- locations %>% 
    separate(col = GPS, into = c("lat", "long"), sep = ", ") %>% 
    rename_all(tolower) %>% 
    mutate(lat = as.numeric(lat),
           long = as.numeric(long))

leaflet(locations_tidy) %>% 
    setView(lng = 35.0, 
             lat = -15.5,
             zoom = 8) %>% 
    addTiles() %>% 
    addCircleMarkers(
        lng = ~long, 
        lat = ~lat,
        label = ~location,
        radius = 5,
        stroke = FALSE,
        fillOpacity = 0.7)
        #labelOptions = labelOptions(noHide = T,
        #                           textsize = "8px",
        #                           direction = "left",
        #                           offset = c(-10, 0),
        #                           style = list(
        #                               "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
        #                               "border-color" = "rgba(0,0,0,0.5)"
        #                               #"font-style" = "italic"
        #                           ))) 
    
    mapshot(file = "figs/map-biogas-reactors.png")

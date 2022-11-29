# Description -------------------------------------------------------------

# This R script prepares a map of biogas reactor locations

# Code --------------------------------------------------------------------

## load libraries

library(dplyr)
library(tidyr)
library(leaflet)
library(mapview)
library(ggmap)

## read data

locations <- readr::read_csv(here::here("data/final-sample-coordinates.csv"))

locations_tidy <- locations %>% 
    separate(col = GPS, into = c("lat", "lon"), sep = ", ") %>% 
    rename_all(tolower) %>% 
    mutate(lat = as.numeric(lat),
           lon = as.numeric(lon))

# leaflet map -------------------------------------------------------------

locations_map <- leaflet(locations_tidy) %>% 
    setView(lng = 35.0, 
            lat = -15.5,
            zoom = 8) %>% 
    addTiles() %>% 
    addCircleMarkers(
        lng = ~long, 
        lat = ~lat,
        label = ~id,
        radius = 6,
        stroke = FALSE,
        fillOpacity = 0.7)
        #labelOptions = labelOptions(noHide = T,
        #                            textsize = "8px",
        #                            direction = "left",
        #                            offset = c(-10, 0),
        #                            style = list(
        #                                "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
        #                                "border-color" = "rgba(0,0,0,0.5)",
        #                                "font-style" = "italic"
        #                            ))) 
#



locations_map %>% 
    mapshot(file = "figs/map-biogas-reactors.png")


# ggmap map ---------------------------------------------------------------

locations_tidy

qmplot(x = lon, y = lat, data = locations_tidy, color = I("red"), zoom = 14)

library(spData)
data("us_states", package = "spData")

install.packages('spDataLarge', repos='https://nowosad.github.io/drat/', type='source')


spData::
# Description -------------------------------------------------------------

# This R script prepares a map of biogas reactor locations

# Code --------------------------------------------------------------------

## load libraries

library(dplyr)
library(tidyr)
library(leaflet)
library(mapview)
library(ggmap)
library(sf)
library(ggplot2)

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
        lng = ~lon, 
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
    mapshot(file = "figs/map-biogas-reactors-01.png")


# ggmap map ---------------------------------------------------------------

locations_tidy

locations_sf <- st_as_sf(locations_tidy, coords = c("lon", "lat"))

st_crs(locations_sf) <- 4326

#unzip(zipfile = "data/mwi_adm_nso_20181016_shp.zip", exdir = "data/mwi_adm_nso_20181016_shp/")

read_sf("data/mwi_adm_nso_20181016_shp/mwi_admbnda_adm0_nso_20181016.shp") %>% 
    ggplot() +
    geom_sf()

read_sf("data/mwi_adm_nso_20181016_shp/mwi_admbnda_adm1_nso_20181016.shp") %>% 
    ggplot(aes(fill = ADM1_EN)) +
    geom_sf()

malawi_shp_adm2 <- read_sf("data/mwi_adm_nso_20181016_shp/mwi_admbnda_adm2_nso_20181016.shp") 

malawi_shp_adm2_4326 <- st_transform(malawi_shp_adm2, crs = 4326)

ggplot()  +
    geom_sf(data = malawi_shp_adm2_4326 ,fill = "white") +
    geom_sf(data = locations_sf, 
            color = "steelblue", 
            alpha = 0.6,
            size = 3)

ggsave(filename = "figs/map-biogas-reactors-02.png")



# read_sf("data/mwi_adm_nso_20181016_shp/mwi_admbnda_adm3_nso_20181016.shp") %>% 
#     ggplot(aes(fill = ADM3_EN))  +
#     geom_sf()

# read_sf("data/mwi_adm_nso_20181016_shp/mwi_admbndl_admALL_nso_itos_20181016.shp") %>% 
#     ggplot() +
#     geom_sf(fill = "white")

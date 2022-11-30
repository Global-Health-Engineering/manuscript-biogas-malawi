# Description -------------------------------------------------------------

# This R script prepares a map of biogas reactor locations

# Code --------------------------------------------------------------------

## load libraries

library(dplyr)
library(tidyr)
library(leaflet)
library(mapview)
library(sf)
library(ggplot2)
library(cowplot)

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


# ggplot2 map ---------------------------------------------------------------

locations_tidy

locations_sf <- st_as_sf(locations_tidy, coords = c("lon", "lat"))

st_crs(locations_sf) <- 4326

locations_sf_bb <- st_as_sfc(st_bbox(locations_sf))

# data from: https://data.humdata.org/dataset/cod-ab-mwi

malawi_shp_adm2 <- read_sf("data/mwi_adm_nso_20181016_shp/mwi_admbnda_adm2_nso_20181016.shp") 

malawi_shp_adm2_4326 <- st_transform(malawi_shp_adm2, crs = 4326)

# Support from:
# https://www.sharpsightlabs.com/blog/mapping-texas-ports-with-geom_sf-part2/
# https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
# https://geocompr.github.io/post/2019/ggplot2-inset-maps/

map1 <- ggplot()  +
    geom_sf(data = malawi_shp_adm2_4326,  
            #color = "grey90",
            size = 0.2,
            fill = "white") +
    #geom_sf(data = locations_sf, 
    #        color = "steelblue",
    #        fill = "red",
    #        shape = 21,  
    #        alpha = 0.6,
    #        size = 3) +
    geom_sf(data = locations_sf_bb,
            fill = NA,
            color = "red",
            size = 1) +
    theme_void()



xlim <- c(
    st_bbox(locations_sf)$xmin[[1]],
    st_bbox(locations_sf)$xmax[[1]]
)

ylim <- c(
    st_bbox(locations_sf)$ymin[[1]],
    st_bbox(locations_sf)$ymax[[1]]
)


mytheme <- theme(panel.grid.major = element_line(color = '#cccccc', 
                                                  linetype = 'dashed',
                                                  size = .3
                 ),
                 #panel.background = element_rect(fill = 'antiquewhite1'),
                 #plot.title = element_text(size = 32),
                 #plot.subtitle = element_text(size = 14),
                 #axis.title = element_blank(),
                 axis.text = element_text(size = 8)
)

## add centroids
malawai_adm2_map_data <- malawi_shp_adm2_4326 |> 
    select(ADM2_EN) |> 
    mutate(centroid = st_centroid(geometry))


malawi_adm2_name_coordinates <- malawai_adm2_map_data |> 
    st_centroid() |> 
    st_coordinates() |> 
    as_tibble()

malawi_map2_point_data <- malawai_adm2_map_data |> 
    bind_cols(malawi_adm2_name_coordinates) |> 
    mutate(x_nudge = case_when(ADM2_EN == "Mangochi" ~ -0.15,
                               ADM2_EN == "Zomba" ~ 0.1,
                               ADM2_EN == "Mwanza" ~ -0.1,
                               ADM2_EN == "Chikwawa" ~ 0.06,
                               TRUE ~ 0),
           y_nudge = case_when(ADM2_EN == "Mangochi" ~ -0.08,
                               ADM2_EN == "Zomba" ~ -0.05,
                               TRUE ~ 0))

map2 <- ggplot() +
    geom_sf(data = malawi_shp_adm2_4326,  
            color = "grey50",
            size = 0.4,
            fill = "antiquewhite1") +
    geom_sf(data = locations_sf, 
            color = "red",
            fill = "steelblue",
            shape = 21, 
            stroke = 1, 
            alpha = 0.6,
            size = 5) +
    geom_text(data = malawi_map2_point_data,
              mapping = aes(x = X, y = Y, label = ADM2_EN),
              size = 4,
              fontface = 'bold',
              alpha = 0.7, 
              nudge_x = malawi_map2_point_data$x_nudge,
              nudge_y = malawi_map2_point_data$y_nudge) +
    coord_sf(xlim = xlim,
             ylim = ylim) +
    labs(title = "Observation Sites",
         subtitle = "Locations of 61 surveyed Biogas Reactors in Malawi") +
    ggspatial::annotation_scale(location = "tr", width_hint = 0.4) +
    ggspatial::annotation_north_arrow(location = "br", which_north = "true") +
    xlab("lon") + ylab("lat") +
    mytheme



ggdraw() +
    draw_plot(map2) +
    draw_plot(map1, 
              x = -0.05,
              y = 0.5, 
              width = 0.5,
              height = 0.5)


ggsave(filename = "figs/map-biogas-reactors-02.png")




# read_sf("data/mwi_adm_nso_20181016_shp/mwi_admbnda_adm3_nso_20181016.shp") %>% 
#     ggplot(aes(fill = ADM3_EN))  +
#     geom_sf()

# read_sf("data/mwi_adm_nso_20181016_shp/mwi_admbndl_admALL_nso_itos_20181016.shp") %>% 
#     ggplot() +
#     geom_sf(fill = "white")

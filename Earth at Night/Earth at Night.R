library(tidyverse)
library(sf)
library(giscoR)
library(mapview)
library(terra)
library(terrainr)
library(magick)
library(showtext)
library(showtextdb)

font_add_google('Merriweather', family = "title")
showtext_auto()


longlat_crs <- "+proj=longlat +datum=WGS84 +no_defs"
ortho_crs <-'+proj=ortho +lat_0=39.93 +lon_0=32.85 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs'


world <- giscoR::gisco_get_countries(year = "2016",
                                     epsg = "4326",
                                     resolution = "60") %>%
    sf::st_transform(longlat_crs) 

world_vect <- terra::vect(world)
  
ras <- terra::rast("/vsicurl/https://eoimages.gsfc.nasa.gov/images/imagerecords/144000/144898/BlackMarble_2016_01deg_geo.tif")
rascrop <- terra::crop(x = ras, y = world_vect, snap = "in")
ras_latlong <- terra::project(rascrop, longlat_crs)
ras_ortho <- terra::project(ras_latlong, ortho_crs)

r <- ifel(is.na(ras_ortho), 0, ras_ortho)
plot(r)



globe <- ggplot() +
  terrainr::geom_spatial_rgb(
    data = r,
    mapping = aes(x = x, y = y, r = red, g = green, b = blue)) +
    theme_minimal() +
    theme(plot.margin = unit(c(t = -1, r = -1, b = -1, l = -1), 
                             "lines"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "black", color = NA),
          panel.background = element_rect(fill = "black", color = NA),
          legend.background = element_rect(fill = "black", color = NA),
          panel.border = element_rect(fill = NA, color = "black")) +
  labs(x = "", y = "", title = "", subtitle = "", caption = "")



ggsave(filename = "nightlight_globe.png", width = 7, height = 7.5, dpi = 600, device = "png", globe)

map <- magick::image_read("nightlight_globe.png")

# set font color
clr <- "#FFFFBC"

# Title
map_title <- magick::image_annotate(map, "Earth at night",
                                    font = "title", 
                                    color = alpha(clr, .65), size = 200, gravity = "north",
                                    location = "+0+80")

# Caption
map_final <- magick::image_annotate(map_title, glue::glue(
  "Inspiration & Code Milos Popovic (https://milospopovic.net) | ", "Made by Muhammet Özkaraca | ", "Data: NASA Earth Observatory"),
  location = "+0+50",
  color = alpha(clr, .45), size = 50, gravity = "south")

magick::image_write(map_final, glue::glue("nightlight_globe_annotated1.png"))

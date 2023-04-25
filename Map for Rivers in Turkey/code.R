library(sf)
library(tidyverse)
library(ggthemes)
library(rnaturalearth)

# Data is accesible through (https://www.hydrosheds.org/products/hydrorivers)

river_data <- read_sf("HydroRIVERS_v10_eu_shp/HydroRIVERS_v10_eu.shp")
turkey_provinces <- ne_states(country = "turkey", returnclass = "sf") # data for turkish cities


new_prj <- sf::st_transform(turkey_provinces, crs = "+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs 
+south")


ggplot(new_prj) +
  geom_sf()

crop_bbox <- c(xmin = 25.7, ymin = 35.7, xmax = 45.3, ymax = 42.3)
ct_cropped <- st_crop(river_data, crop_bbox)

ct_cropped1 <- ct_cropped %>%
  filter(ORD_FLOW < 6)

ct_cropped <- sf::st_transform(ct_cropped, crs = "+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs 
+south")

riv_width <- ct_cropped  |>
  dplyr::mutate(
    width = as.numeric(ORD_FLOW),
    width = dplyr::case_when(width == 3 ~ 1,
                             width == 4 ~ 0.8,
                             width == 5 ~ 0.6,
                             width == 6 ~ 0.4,
                             width == 7 ~ 0.2,
                             width == 8 ~ 0.2,
                             width == 9 ~ 0.1,
                             width == 10 ~ 0.1,
                             TRUE ~ 0)) %>%
  sf::st_as_sf()



p2 <- ggplot() +
  # geom_sf(data = new_prj, color = "lightgrey", fill = "lightgrey") +
  geom_sf(data = riv_width, aes(color = factor(ORD_FLOW), size = width, alpha = factor(ORD_FLOW))) +
  scale_color_manual(name = "", values = c("#08306b", "#08519c", "#2171b5","#4292c6", "#6baed6", "#9ecae1",
                                           "#c6dbef", "#deebf7")) + 
  scale_size(range = c(0, .3)) +
  scale_alpha_manual(values = c("3" = 1, "4" = 1, "5" = .7, "6" = .6,
                                "7" = .4, "8" = .3, "9" = .2, "10" = .1)) +
  theme(legend.position = "none")
  

ggsave(filename = "european_rivers4.png", width = 8.5, height = 7, dpi = 600, device = "png", bg = "white", p2)







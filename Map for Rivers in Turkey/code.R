library(httr)
library(sf)
library(tidyverse)
library(ggthemes)
library(rnaturalearth)
library(showtext)
library(showtextdb)

font_add_google('EB Garamond', family = "title")
font_add_google('Open Sans', family = "caption")


showtext_auto()

turkey <- ne_countries(country = "turkey", returnclass = "sf") # data for turkish cities


get_data <- function() {
  
  url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_eu_shp.zip"
  
  res <- GET(url,
             write_disk("eu_rivers.zip"),
             progress())
  unzip("eu_rivers.zip")
  filenames <- list.files("HydroRIVERS_v10_eu_shp", pattern="*.shp", full.names=T)
  
  riv_list <- lapply(filenames, st_read)
  
  return(riv_list)
}

data <- get_data()
eu_riv <- data[[1]]
eu_riv_width <- eu_riv |>
  dplyr::mutate(
    width = as.numeric(ORD_FLOW),
    width = dplyr::case_when(
      width == 3 ~ 1,
      width == 4 ~ 0.8,
      width == 5 ~ 0.6,
      width == 6 ~ 0.4,
      width == 7 ~ 0.2,
      width == 8 ~ 0.2,
      width == 9 ~ 0.1,
      width == 10 ~ 0.1,
      TRUE ~ 0
    )
  ) |>
  sf::st_as_sf()



st_crs(eu_riv_width)
st_crs(turkey)

rivers_turkey <- eu_riv_width %>% 
  st_intersection(turkey)


p <- ggplot() +
  geom_sf(data = rivers_turkey, aes(color = factor(ORD_FLOW), size = width,alpha = factor(ORD_FLOW))) +
  labs(title = "Rivers in Turkey",
       caption = "Inspiration & Essential Code Milos Popovic (https://milospopovic.net) | Made by Muhammet Özkaraca | Data: HydroRIVERS",
       x = "",
       y = "",
       subtitle = "") +
  scale_color_manual(name = "", values = c("#08306b", "#08519c", "#2171b5",
                                           "#4292c6", "#6baed6", "#9ecae1",
                                           "#c6dbef", "#deebf7")) +
  scale_size(range = c(0, .3)) +
  scale_alpha_manual(values = c("3" = 1, "4" = 1, "5" = .7, "6" = .6,
                                "7" = .4, "8" = .3, "9" = .2, "10" = .1)) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.position = "none",
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(size = 120, face = "bold", color = "#2171b5", family = "title", hjust = 0.5, vjust = 0),
        plot.subtitle = element_text(size = 14, color = "#ac63a0", hjust = 0.5, vjust = 0),
        plot.caption = element_text(size = 40, color = "grey60", family = "caption", hjust = 0.5, vjust = 10),
        axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
        legend.text = element_text(size = 9, color = "grey20"),
        legend.title = element_text(size = 10, color = "grey20"),
        strip.text = element_text(size = 12),
        plot.margin = unit(c(t = 1, r = -2, b = -1, l = -2), "lines"),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())


ggsave(filename = "turkey_rivers.png", width = 8.5, height = 7, dpi = 600, 
       device = "png", bg = "white", p)






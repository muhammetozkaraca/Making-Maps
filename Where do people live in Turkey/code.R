library(tidyverse)
library(terra)
library(giscoR)
library(ggtext)
library(showtext)
library(showtextdb)

font_add_google('EB Garamond', family = "title")
font_add_google('Open Sans', family = "caption")
showtext_auto()



# 1. DOWNLOAD GHSL DATA
#----------------------

url <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2025_GLOBE_R2023A_4326_30ss/V1-0/GHS_POP_E2025_GLOBE_R2023A_4326_30ss_V1_0.zip"
file_name <- "GHS_POP_E2025_GLOBE_R2023A_4326_30ss_V1_0.zip"

download.file(
  url = url,
  path = getwd(),
  destfile = file_name
)

# 2. LOAD GHSL DATA
#----------------------

unzip(file_name)
raster_name <- gsub(
  ".zip", ".tif",
  file_name
)

pop <- terra::rast(raster_name)



get_country_borders <- function(){
  country <- giscoR::gisco_get_countries(
    country = "TR",
    resolution = "3"
  )
  
  return(country)
}

country <- get_country_borders()



turkey_pop <- terra::crop(
  pop,
  terra::vect(country),
  snap = "in",
  mask = T
)


turkey_pop_df <- as.data.frame(
  turkey_pop,
  xy = T, na.rm = T
)


head(turkey_pop_df)

names(turkey_pop_df)[3] <- "val"
turkey_pop_df <- turkey_pop_df |>
  dplyr::mutate(
    cat = dplyr::if_else(
      val > 0, "Yes", "No"
    )
  )

turkey_pop_df$cat <- as.factor(
  turkey_pop_df$cat
)

# 6. MAP
#-------

cols <- c("#0a1c29", "#edc241")

p <- ggplot() +
  geom_raster(data = turkey_pop_df, aes(x = x, y = y, fill = cat)) +
  scale_fill_manual(values = cols, na.value = "#0a1c29") +
  labs(title = "Where do people live in Turkey?",
       subtitle = "Yellow points demonstare areas of human population in Turkey",
       caption = "Data: Global Human Settlement Layer at 30 arcsec | Inspiration & Essential Code by Milos Popovic | Made by Muhammet Özkaraca") +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        legend.position = "none",
        plot.title = element_markdown(hjust = 0.5, family = "title", size = 65),
        plot.subtitle = element_markdown(hjust = 0.5, family = "title", size = 40),
        plot.caption = element_markdown(hjust = 0.5, family = "caption", size = 25)) 

ggsave("turkey_human_settlement_map.png", width = 12, height = 6)

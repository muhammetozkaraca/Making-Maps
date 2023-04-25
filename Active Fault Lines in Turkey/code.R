library(maps)
library(ggthemes)
library(foreign)
library(tidyterra)
library(geodata)
library(terra)
library(ggtext)
library(showtext)

font_add_google("Playfair Display", family = "title")
font_add_google("Merriweather", family = "caption")


showtext_auto()

# SOLUTION I

# Data can be accessible through (https://github.com/GEMScienceTools/gem-global-active-faults)

faults <- read_sf('gem_active_faults_harmonized.shp') %>%
  st_set_crs("+proj=longlat +datum=WGS84 +no_defs")

country <- giscoR::gisco_get_countries(country = "Turkey")

ggplot() +
  geom_sf(data = country) +
  geom_sf(data = faults, color = "red", size = 3) +
  coord_sf(xlim = c(26, 45), ylim = c(36, 42)) +
  theme(legend.position = "none")




# SOLUTION II


url <- "https://github.com/GEMScienceTools/gem-global-active-faults/raw/master/geopackage/gem_active_faults_harmonized.gpkg"
f <- basename(url)
if (!file.exists(f)) download.file(url, f, mode="wb")
flts <- vect(f)
wrld <- geodata::world(path=".")
epic <- cbind(37.203, 38.024)


fay_hatları <- ggplot() +
  geom_spatvector(data = wrld) +
  geom_spatvector(data = flts, color = "red", size = 3) +
  coord_sf(xlim = c(25, 45), ylim = c(32, 43)) +
  theme_bw() + 
  labs(title = "Türkiye'de Aktif Olan Fay Hatları",
       caption = "Kaynak: The GEM Global Active Faults Database") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "lightblue"),
        plot.title = element_markdown(family = "title", hjust = 0.5, size = 220),
        plot.caption = element_markdown(family = "caption", hjust = 0, size = 100))


ggsave("/Users/muhammetozkaraca/Desktop/fay_hatları.png", height = 9, width = 14,
       dpi=700)




library(tidyverse)
library(sf)
library(rnaturalearth)
library(elevatr)
library(showtext)
library(ggthemes)


font_add_google("Special Elite", family = "plottitle")
font_add_google("Roboto Slab", family = "caption") 

font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')
showtext_auto()


turkey_admin_boundaries <- ne_states(country = "Turkey", returnclass = "sf")
elev_data <- elevatr::get_elev_raster(locations = turkey_admin_boundaries, 
                                      z = 3,
                                      clip = "locations")


elev_mat <- terra::as.matrix(elev_data, wide = TRUE)
colnames(elev_mat) <- 1:ncol(elev_mat)
elev_df <- elev_mat |> 
  as_tibble() |> 
  mutate(y = row_number()) |> 
  pivot_longer(-y, names_to = "x") |> 
  mutate(x = as.numeric(x))

chars <- c("l", "I", "H", "M")
chars_map <- data.frame(value = seq_len(length(chars)),
                        value_letter = chars)
elev_plot <- elev_df |> 
  mutate(value = ntile(value, n = length(chars))) |> 
  left_join(chars_map, by = "value") |> 
  drop_na()


plot <- ggplot() +
  geom_text(data = elev_plot, 
            mapping = aes(x = x, y = y, label = value_letter),
            family = "elite",
            colour = "black",
            size = 4.5) +
  scale_y_reverse() +
  labs(title = "Typewriter Map of Turkey<br>",
       caption = "Inspiration: <span style='font-family:fa-brands'>&#xf099;</span> @nrennie35 | Plot: <span style='font-family:fa-brands'>&#xf09b; </span>muhammetozkaraca <span style='font-family:fa-brands'>&#xf099;</span> @muhammetozkrca") +
  coord_fixed() +
  theme_map() +
  theme(plot.title = element_markdown(family = "elite", size = 30,
                                      colour = "grey10", margin = margin(t = 20, b = -20)),
        plot.caption = element_markdown(family = "caption", size = 20, hjust = 0.5,
                                        colour = "grey10"),
        plot.background = element_rect(fill = "white", color = "white"))

ggsave("plot.png")





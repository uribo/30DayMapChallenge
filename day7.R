###########################################
# DAY07: ブラジルにおける2018年の火事
###########################################
library(data.table)
library(sf)
library(dplyr)
library(ggplot2)
library(ggpointdensity)
library(ggtext)

ne_brazil <-
  rnaturalearth::ne_countries(scale = 10, returnclass = "sf", country = "brazil")

# FROM) https://datacatalog.worldbank.org/dataset/major-rivers-world
sf_river <-
  st_read("~/Documents/projects2019/30DayMapChallenge/data-raw/majorrivers_0_0/MajorRivers.shp",
          stringsAsFactors = FALSE,
          as_tibble = TRUE) %>%
  janitor::clean_names() %>%
  st_intersection(ne_brazil) %>%
  select(name, miles)

dt <- fread(here::here("data-raw/DL_FIRE_M6_85251/fire_archive_M6_85251.csv"))
dt <- dt[, list(latitude, longitude, acq_date)]
dt[, month := lubridate::month(lubridate::as_date(acq_date), label = TRUE, abbr = FALSE, locale = "en_US")]


# mapping -----------------------------------------------------------------
base_p <-
  ggplot() +
  geom_sf(data = ne_brazil, size = 0.5, fill = "white", color = "#FFFFFF") +
  geom_sf(data = sf_river, color = "#0B2872") +
  theme_void() +
  coord_sf(datum = NA)
p_out <-
  base_p +
  geom_pointdensity(data = dt,
                    mapping = aes(x = longitude, y = latitude), size = 0.0005) +
  scale_color_viridis_c(option = "magma", begin = 0.6, end = 0.9) +
  facet_wrap(~ month) +
  xlab(NULL) +
  ylab(NULL) +
  guides(fill = guide_colorbar(title = NULL,
                               title.position = "top",
                               title.vjust = 0.95)) +
  theme(plot.background = element_rect(fill = "#479847"),
    plot.title = element_markdown(colour = "#F9DF4B"),
    plot.subtitle = element_text(color = "#F9DF4B"),
    strip.text = element_text(color = "#FFFFFF"),
    plot.caption = element_markdown(size = 6, color = "#0B2872"),
    legend.title = element_text(color = "#FFFFFF"),
    legend.text = element_text(color = "#FFFFFF"),
        legend.position = "right",
        legend.key.height = unit(4.0, "line")) +
  labs(
    title = glue::glue("<span style='font-family: \"Font Awesome 5 Free Solid\"'>&#63460;</span> Fire in Brazil in 2018"),
    subtitle = glue::glue("Source: NASA, Fire Information for Resource Management System MODIS"),
    caption = glue::glue("Base map: Natural Earth, River data: The World Bank<br><i>#30daymapchallenge</i> <span style='font-family: \"Font Awesome 5 Brands\"; color:#D35172'>&#61595;</span> uribo/30daymapchallenge <span style='font-family: \"Font Awesome 5 Brands\"; color:#D35172'>&#61593;</span> @uribo<br>"))
ggsave(here::here("figures/day07_brazil_fire.png"),
       p_out,
       width = 8,
       height = 6,
       dpi = 300)

###########################################
# DAY05: 国立公園の地区設定
###########################################
library(sf)
library(dplyr)
library(ggplot2)
library(h3forr)
library(jpmesh)
library(patchwork)

plot_np_area_hex <- function(data, area, fill_highclor = NULL) {
  area <- rlang::enquo(area)
  data <-
    data %>%
    dplyr::filter(!is.na(!! area)) %>%
    dplyr::select(area = !! area)
  ggplot() +
    geom_sf(data = np_daisetsu_union, fill = "transparent") +
    geom_sf(data = data,
            aes(fill = area, color = area),
            alpha = 0.6) +
    scale_fill_gradient(low = "#FFFFFF", high = fill_highclor) +
    scale_color_gradient(low = "#FFFFFF", high = fill_highclor) +
    coord_sf(datum = NA) +
    theme_void() +
    theme(plot.background = element_rect(fill = "#FBE9C9")) +
    guides(fill = FALSE, color = FALSE)
}

np_daisetsu <-
  jpnp::np %>%
  filter(name == "大雪山")
np_daisetsu_union <-
  rmapshaper::ms_dissolve(np_daisetsu)
d_np_hex <-
  np_daisetsu %>%
  st_make_grid(n = 30, what = "centers") %>%
  st_sf() %>%
  mutate(longitude = sf::st_coordinates(geometry)[, 1],
         latitude = sf::st_coordinates(geometry)[, 2]) %>%
  mutate(geocode = purrr::pmap(.,
                               ~ geo_to_h3(c(..3, ..2), res = 7) %>%
                                 h3_to_geo_boundary()))
st_geometry(d_np_hex) <-
  d_np_hex$geocode %>%
  purrr::map(
    geo_boundary_to_sf
  ) %>%
  purrr::reduce(rbind) %>%
  st_geometry()
d_np_hex <-
  d_np_hex %>%
  distinct(.keep_all = TRUE) %>%
  select(-geocode) %>%
  st_join(np_daisetsu %>%
            select(name_en),
          left = FALSE) %>%
  select(-longitude, -latitude)
d <-
  d_np_hex %>%
  mutate(area = purrr::pmap(.,
                            ~ st_crop(np_daisetsu, ..2) %>%
                              transmute(zone = area,
                                        area = st_area(geometry)) %>%
                              st_drop_geometry())) %>%
  distinct(.keep_all = TRUE)

d_tmp <-
  d$area %>%
  purrr::map(
    ~ tidyr::pivot_wider(.x, names_from = zone, values_from = area)
  ) %>%
  bind_rows() %>%
  set_names(c("Ordinary_Zones", "Class_II_Special_Zones", "Class_III_Special_Zones", "Class_I_Special_Zones", "Special_Protection_Zones"))

d <-
  d %>%
  select(-area) %>%
  mutate(zone1 = d_tmp$Ordinary_Zones,
         zone2 = d_tmp$Class_I_Special_Zones,
         zone3 = d_tmp$Class_II_Special_Zones,
         zone4 = d_tmp$Class_III_Special_Zones,
         zone5 = d_tmp$Special_Protection_Zones)

# d %>% mapview::mapview(zcol = "area4")
# d_tmp %>%
#   tibble::rowid_to_column() %>%
#   tidyr::pivot_longer(2:6)


base_p <-
  ggplot() +
  geom_sf(data = np_daisetsu %>%
            mutate(area = recode(area,
                                 `普通地域` = "Ordinary Zones",
                                 `第1種特別地域` = "Class I Special Zones",
                                 `第2種特別地域` = "Class II Special Zones",
                                 `第3種特別地域` = "Class III Special Zones",
                                 `特別保護地区` = "Special Protection Zones") %>%
                     forcats::fct_relevel(c("Ordinary Zones", "Class I Special Zones", "Class II Special Zones", "Class III Special Zones", "Special Protection Zones"))),
          aes(fill = area), color = "#EEEEEE") +
  scale_fill_manual(values = c("Ordinary Zones" = "#80c1d3",
                               "Class I Special Zones" = "#a084bc",
                               "Class II Special Zones" = "#f1bcad",
                               "Class III Special Zones" = "#bcd162",
                               "Special Protection Zones" = "#f2aa81")) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.text  = element_text(color = "#5E5E5F"))

p1 <- plot_np_area_hex(d, zone1, "#80c1d3")
p2 <- plot_np_area_hex(d, zone2, "#a084bc")
p3 <- plot_np_area_hex(d, zone3, "#f1bcad")
p4 <- plot_np_area_hex(d, zone4, "#bcd162")
p5 <- plot_np_area_hex(d, zone5, "#f2aa81")

p_out <-
  p1 + p2 + p3 + p4 + p5 +
  base_p +
  plot_layout(ncol = 3) +
  plot_annotation(title = "Daisetuzan National Park",
                  caption = "Source: Biodiversity Center of Japan, Ministry of the Environment
                  #30daymapchallenge @uribo")

ggsave(here::here("figures/day04_np_daisetsuzan_hexagons.png"),
       p_out,
       width = 10,
       height = 6,
       dpi = 300)

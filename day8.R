##########################################
# DAY08: 日本の国立公園位置のボロノイ分割。面積と利用者の関係
##########################################
# http://gis.biodic.go.jp/webgis/index.html
library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(readxl)
library(ggrepel)
library(biscale)
library(ggtext)
library(cowplot)

ne_jpn <-
  ne_states(country = "Japan", returnclass = "sf") %>%
  tibble::new_tibble(nrow = nrow(.), subclass = "sf") %>%
  arrange(iso_3166_2) %>%
  st_union()

# Download from: http://www.env.go.jp/park/doc/data.html
df_visitor <-
  read_xls(here::here("data-raw/naturalpark_4.xls"),
           range = "BH5:BK72",
           col_names = c("name", paste0("H", seq.int(27, 29)))) %>%
  select(name, visitor_2017 = H29) %>%
  tidyr::fill(name, .direction = "down") %>%
  slice(seq.int(2, nrow(.), by = 2)) %>%
  mutate(name = name %>%
           stringr::str_remove("\u203b") %>%
           stringr::str_squish() %>%
           stringr::str_remove("\\（.+\\）") %>%
           stringr::str_trim()) %>%
  mutate(name = recode(name,
                       `サロベツ` = "利尻礼文サロベツ",
                       `阿寒` = "阿寒摩周"))
df_area <-
  read_xls(here::here("data-raw/np_2.xls"),
           range = "A7:B40",
           col_types = rep("text", 2),
           col_names = c("name", "area")) %>%
  mutate(area = as.numeric(area))

sf_np_union <-
  jpnp::np %>%
  mutate(name_en = forcats::fct_inorder(name_en)) %>%
  st_simplify(dTolerance = 0.005) %>%
  group_by(name, name_en) %>%
  summarise() %>%
  ungroup()

sf_np <-
  sf_np_union %>%
  st_centroid() %>%
  left_join(df_visitor, by = "name") %>%
  left_join(df_area, by = "name") %>%
  select(name = name_en, visitor_2017, area)

sf_volonoi <-
  st_collection_extract(st_voronoi(do.call(c, st_geometry(sf_np)))) %>%
  st_set_crs(4326) %>%
  st_sf() %>%
  st_join(sf_np, join = st_contains) %>%
  st_crop(ne_jpn)

df_text <-
  sf_volonoi %>%
  st_centroid() %>%
  mutate(longitude = sf::st_coordinates(geometry)[, 1],
         latitude = sf::st_coordinates(geometry)[, 2]) %>%
  st_drop_geometry()

data <- bi_class(sf_volonoi,
                 x = visitor_2017,
                 y = area,
                 style = "quantile", dim = 3)
legend <- bi_legend(pal = "DkCyan",
                    dim = 3,
                    xlab = "Higher visitor ",
                    ylab = "Higher area ",
                    size = 8)
p <-
  ggplot() +
  geom_sf(data = data, aes(fill = bi_class),
          color = "#EEEEEE",
          size = 0.8,
          show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan", dim = 3) +
  geom_sf(data = ne_jpn, fill = "transparent", color = "black", size = 0.1) +
  geom_sf(data = sf_np_union, fill = "#E68B3B", color = "#E68B3B", size = 0.01) +
  geom_text_repel(data = df_text,
                  aes(x = longitude,
                      y = latitude,
                      label = name),
                  size = 2.0,
                  color = "#000000") +
  theme_void() +
  labs(
    caption = glue::glue("Source: Biodiversity Center of Japan, Ministry of the Environment<br><i>gis.biodic.go.jp/webgis/sc-026.html?kind=nps</i><br><i>#30daymapchallenge</i> <span style='font-family: \"Font Awesome 5 Brands\"; color:#000000'>&#61595;</span> uribo/30daymapchallenge <span style='font-family: \"Font Awesome 5 Brands\"; color:#55acee'>&#61593;</span> @u_ribo<br>")
  ) +
  theme(
    plot.caption = element_markdown(size = 6, lineheight = 1.1))
p_out <-
  plot_grid(p, legend,
            rel_widths = c(1, 0.4),
            ncol = 2)
p_title <-
  ggdraw() +
  draw_label( "Voronoi grid of Japan National Parks",
              fontface = "bold",
              x = 0.2,
              hjust = 0) +
  draw_label(
    "Park Visitor and Area",
    x = 0.35,
    y = 0.1,
    hjust = 0)
p_out <-
  plot_grid(p_title, p_out, ncol = 1, rel_heights = c(0.1, 1))
ggsave(here::here("figures/day08_np_voronoi.png"),
       p_out,
       width = 10,
       height = 7,
       dpi = 300)

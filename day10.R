###########################################
# DAY10: 新宿駅周辺のビル群を新宿駅からの距離で色分け
###########################################
library(dplyr)
library(sf)
library(ggplot2)
library(ggtext)

# data collect ------------------------------------------------------------
z <- 15
tile_coords <-
  slippymath::lonlat_to_tilenum(139.6986, 35.6907, zoom = z)
x <- tile_coords$x
y <- tile_coords$y
df <-
  tibble::tibble(x, y) %>%
  mutate(x = purrr::pmap(., ~ ..1 + seq.int(-2, 2)),
         y = purrr::pmap(., ~ ..2 + seq.int(-2, 2))) %>%
  tidyr::unnest_longer(x) %>%
  tidyr::unnest_longer(y) %>%
  tibble::add_column(z = z) %>%
  mutate(gsi_elements = purrr::pmap(.,
                                    ~ glue::glue("https://cyberjapandata.gsi.go.jp/xyz/experimental_bvmap/{..3}/{..1}/{..2}.pbf") %>%
                                      protolite::read_mvt_sf()))

sf_p <-
  st_point(c(139.6986838, 35.6907387)) %>%
  st_sfc(crs = 4326) %>%
  st_transform(crs = 6677)
sf_circle <-
  sf_p %>%
  st_buffer(dist = units::set_units(2000, m)) %>%
  st_transform(crs = 4326)
sf_building <-
  df %>%
  purrr::pluck("gsi_elements") %>%
  purrr::map("building") %>%
  purrr::reduce(rbind)
sf_building_crop <-
  sf_building %>%
  st_intersection(sf_circle)
sf_building_crop <-
  sf_building_crop %>%
  mutate(distance = sf_building_crop %>%
           st_transform(crs= 6677) %>%
           st_centroid() %>%
           mutate(dist = st_distance(geometry, sf_p)[, 1]) %>%
           pull(dist) %>%
           units::drop_units()) %>%
  tibble::new_tibble(subclass = "sf", nrow = nrow(.))

# sf_building_crop %>% arrange(desc(distance))
# hist(sf_building_crop$distance)

# mapping -----------------------------------------------------------------
p_out <-
  ggplot() +
  geom_sf(data = sf_building_crop,
          aes(color = distance),
          fill = "white",
          size = 0.3) +
  scale_color_gradient(low = "black", high = "gray",
                       name = "Distance from Shinjuku Station (m)") +
  coord_sf(datum = NA) +
  theme_void(base_family = "Times New Roman") +
  theme(title = element_markdown(hjust = 0.5, face = "bold"),
        plot.caption = element_markdown(),
        legend.position = "bottom",
        legend.key.width = unit(2.0, "line"),
        legend.key.height = unit(0.6, "line")) +
  labs(
    title = "Shinjuku Metropolitan Buildings",
    caption = glue::glue("Source: GSI Vector Tile Experiment (Geospatial Information Authority of Japan) <br>
       <i>#30daymapchallenge</i> <span style='font-family: \"Font Awesome 5 Brands\"; color:#000000'>&#61595;</span> uribo/30daymapchallenge <span style='font-family: \"Font Awesome 5 Brands\"; color:#55acee'>&#61593;</span> @u_ribo<br>"))

ggsave("figures/day10_tokyo_builing.png",
       p_out,
       width  = 6,
       height = 7,
       dpi    = 300)

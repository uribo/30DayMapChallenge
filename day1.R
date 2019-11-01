####################################
# DAY01: 地図記号を「地図記号」でプロット
####################################
renv::install(c("protolite", "ggimage", "slippymath"))
# download.file("http://www.gsi.go.jp/common/000076729.zip",
#               destfile = here::here("data-raw/000076729.zip"))
# unzip(here::here("data-raw/000076729.zip"),
#       exdir = here::here("data-raw"))
library(sf)
library(dplyr)
library(ggplot2)
library(ggimage)
library(ggtext)

sf_tile <-
  protolite::read_mvt_sf("https://cyberjapandata.gsi.go.jp/xyz/experimental_bvmap/14/14567/6427.pbf")
names(sf_tile)

df_symbols <-
  tibble::tibble(
  ftCode = c(3201, 3211, 3214, 3217, 3218, 3205,
             3231, 3232, 3241, 3243, 4104, 6301,
             6311, 6312, 6314, 6322, 6321, 6323,
             6327, 7101, 7102, 7103, 8105),
  object = c("官公署", "交番", "小・中学校", "図書館", "郵便局", "市役所",
             "神社", "寺院", "警察署", "病院", "煙突", "墓地",
             "田", "畑", "果樹園", "針葉樹林", "広葉樹林", "竹林",
             "荒地", "電子基準点", "電子基準点", "水準点", "電波塔")) %>%
  inner_join(
    tibble::tibble(
        path = fs::dir_ls(here::here("data-raw/tatemono_syokusei"),
                        recurse = TRUE,
                        regexp = ".png$")) %>%
      mutate(object = stringr::str_remove(basename(path), ".png")),
    by = "object")

df_symbols <-
  sf_tile$symbol %>%
  left_join(df_symbols, by = "ftCode") %>%
  mutate(longitude = st_coordinates(geometry)[, 1],
         latitude  = st_coordinates(geometry)[, 2]) %>%
  st_drop_geometry()

p_out <-
ggplot(df_symbols) +
  geom_image(aes(x = longitude, y = latitude,
                 image = path),
             size = 0.014) +
  geom_sf_text(data = sf_tile$label,
               aes(label = knj),
               size = 2,
               family = "IPAexGothic") +
  theme_minimal(base_family = "IPAexGothic",
                base_size = 6) +
  theme(plot.caption = element_markdown()) +
  labs(caption = glue::glue("国土地理院Vector (国土地理院ベクトルタイル提供実験) を瓜生真也が加工して作成<br>
       <i>#30daymapchallenge</i> <span style='font-family: \"Font Awesome 5 Brands\"; color:#000000'>&#61595;</span> uribo/30daymapchallenge <span style='font-family: \"Font Awesome 5 Brands\"; color:#55acee'>&#61593;</span> @uribo<br>"))

ggsave(here::here("figures/day01_point.png"),
       p_out,
       width = 5,
       height = 4.6,
       dpi = 300)

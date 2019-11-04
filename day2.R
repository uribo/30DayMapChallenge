##########################################
# DAY02: 日本の河川水系
##########################################
library(sf)
library(dplyr)
library(ggplot2)
library(stringr)
library(ggtext)
source(here::here("R/river.R"))

sf_river_japan <- collect_distict(seq.int(47))

sf_river_d01 <-
  sf_river_japan %>%
  filter(pref_code == "01")
sf_river_d02 <-
  sf_river_japan %>%
  filter(pref_code %in% str_pad(seq.int(2, 7), width = 2, pad = "0"))
sf_river_d03 <-
  sf_river_japan %>%
  filter(pref_code %in% str_pad(seq.int(8, 14), width = 2, pad = "0")) %>%
  st_crop(
    st_sf(geometry = st_as_sfc("POLYGON ((138.1421 34.84707, 138.1421 37.142, 140.9766 37.142, 140.9766 34.84707, 138.1421 34.84707))",
                               crs = 4326)))
sf_river_d04 <-
  sf_river_japan %>%
  filter(pref_code %in% str_pad(seq.int(15, 23), width = 2, pad = "0"))
sf_river_d05 <-
  sf_river_japan %>%
  filter(pref_code %in% str_pad(seq.int(24, 30), width = 2, pad = "0"))
sf_river_d06 <-
  sf_river_japan %>%
  filter(pref_code %in% str_pad(seq.int(31, 35), width = 2, pad = "0"))
sf_river_d07 <-
  sf_river_japan %>%
  filter(pref_code %in% str_pad(seq.int(36, 39), width = 2, pad = "0"))
sf_river_d08 <-
  sf_river_japan %>%
  filter(pref_code %in% str_pad(seq.int(40, 46), width = 2, pad = "0")) %>%
  st_crop(
    st_sf(geometry = st_as_sfc("POLYGON ((128.5181 28.66064, 128.5181 34.90173, 132.2534 34.90173, 132.2534 28.66064, 128.5181 28.66064))",
                                     crs = 4326)))
sf_river_d09 <-
  sf_river_japan %>%
  filter(pref_code == "47")

p_d01 <-
  map_river(sf_river_d01,
            "1) Hokkaido District")
p_d02 <-
  map_river(sf_river_d02,
            "2) Tohoku District")
p_d03 <-
  map_river(sf_river_d03,
            "3) Kanto DistrIzu and ict exclude Ogasawara Islands")
p_d04 <-
  map_river(sf_river_d04,
            "4) Cyubu District")
p_d05 <-
  map_river(sf_river_d05,
            "5) Kinki District")
p_d06 <-
  map_river(sf_river_d06,
            "6) Chugoku District")
p_d07 <-
  map_river(sf_river_d07,
            "7) Shikoku District")
p_d08 <-
  map_river(sf_river_d08,
            "8) Kyusyu District exclude Amami Islands")
p_d09 <-
  map_river(sf_river_d09,
            "9) Okinawa District")

ggsave(here::here("figures/day02_japan_river_01Hokkaido.png"),
       p_d01,
       width = 5,
       height = 4.6,
       dpi = 300)
ggsave(here::here("figures/day02_japan_river_02Tohoku.png"),
       p_d02,
       width = 5,
       height = 4,
       dpi = 300)
ggsave(here::here("figures/day02_japan_river_03Kanto.png"),
       p_d03,
       width = 5,
       height = 4.6,
       dpi = 300)
ggsave(here::here("figures/day02_japan_river_04Cyubu.png"),
       p_d04,
       width = 5,
       height = 4.6,
       dpi = 300)
ggsave(here::here("figures/day02_japan_river_05Kinki.png"),
       p_d05,
       width = 5,
       height = 4.6,
       dpi = 300)
ggsave(here::here("figures/day02_japan_river_06Chugoku.png"),
       p_d06,
       width = 5,
       height = 4.6,
       dpi = 300)
ggsave(here::here("figures/day02_japan_river_07Shikoku.png"),
       p_d07,
       width = 5,
       height = 4.6,
       dpi = 300)
ggsave(here::here("figures/day02_japan_river_08Kyusyu.png"),
       p_d08,
       width = 5,
       height = 4,
       dpi = 300)
ggsave(here::here("figures/day02_japan_river_09Okinawa.png"),
       p_d09,
       width = 5,
       height = 4,
       dpi = 300)

p_japan <-
  map_river(sf_river_japan,
            "") +
  theme_void(base_size = 16) +
  theme(legend.key.width = unit(0.7, "cm"),
      legend.key.height = unit(0.02, "cm"),
      legend.title = element_blank(),
      legend.position = "bottom",
      plot.caption = element_markdown(hjust = 1.0),
      legend.text = element_markdown(size = 10))

ggsave(here::here("figures/day02_japan_river.png"),
       p_japan,
       width = 10.5,
       height = 12,
       dpi = 300)

#
# ggplot() +
#   geom_sf(data = d,
#           aes(color = class,
#               size = class),
#           show.legend = "line") +
#   scale_size_manual(values = c("A" = 0.4, "B" = 0.2, "Other" = 0.05),
#                     labels = c("A" = "<i style='color:#1C1167'>Class A</i>",
#                                "B" = "<i style='color:#246BAE'>Class B</i>",
#                                "Other" = "<i style='color:#2C9AC4'>Other</i>")) +
#   scale_color_manual(values = c("A" = "#1C1167", "B" = "#246BAE", "Other" = "#2C9AC4")) +
#   coord_sf(datum = NA) +
#   theme_void() +
#   guides(color = FALSE) +
#   theme(legend.key.width = unit(1, "cm"),
#         legend.title = element_blank(),
#         legend.position = "bottom",
#         plot.caption = element_markdown(),
#         legend.text = element_markdown(size = 11))




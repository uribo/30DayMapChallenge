##########################################
# DAY02: 日本の河川水系
##########################################
library(sf)
library(dplyr)
library(ggplot2)
library(stringr)
library(ggtext)

water_system_classifier <- function(df) {
  df %>%
    dplyr::mutate(class = dplyr::recode(W05_003,
                                            `1級指定区間` = "A",
                                            `1級指定区間でかつ湖沼区間を兼ねる場合` = "A",
                                            `1級直轄区間` = "A",
                                            `1級直轄区間でかつ湖沼区間を兼ねる場合` = "A",
                                            `2級河川区間` = "B",
                                            `2級河川区間でかつ湖沼区間を兼ねる場合` = "B",
                                            `1` = "A",
                                            `2` = "A",
                                            `5` = "A",
                                            `3` = "B",
                                            `7` = "B",
                                        `4` = "Other",
                                            `8` = "Other",
                                            `0` = "Other",
                                            .default = "Other") %>%
                    forcats::fct_relevel("A", "B", "Other"),
                  size = dplyr::recode(class,
                                       `A` = 0.2,
                                       `B` = 0.1,
                                       `Other` = 0.025))
}

collect_distict <- function(pref_code) {
  pref_code <-
    pref_code %>%
    stringr::str_pad(width = 2, pad = "0")
  pref_code %>%
    purrr::map(
      ~ fs::dir_ls("~/Documents/resources/国土数値情報/W05/",
                   recurse = TRUE,
                   regexp = glue::glue("W05-.+_{.x}_GML/W05-.+_{.x}-g_Stream.shp")) %>%
        st_read(stringsAsFactors = FALSE,
                as_tibble = TRUE) %>%
        mutate(pref_code = .x) %>%
        select(seq.int(4), pref_code) %>%
        set_names(c(paste0("W05_", stringr::str_pad(seq.int(4), pad = "0", width = 3)),
                    "pref_code",
                     "geometry")) %>%
        mutate_at(vars(starts_with("W05")), as.character)) %>%
    purrr::reduce(rbind) %>%
    st_set_crs(4326) %>%
    water_system_classifier()
}

map_river <- function(data, subtitle = "") {
  ggplot() +
    geom_sf(data = data,
            aes(color = class,
                size = size),
            show.legend = "line") +
    scale_size_identity() +
    scale_color_manual(values = c("A" = "#1C1167", "B" = "#246BAE", "Other" = "#80A2C1"),
                       labels = c("A" = "<i style='color:#1C1167'>Class A</i>",
                                  "B" = "<i style='color:#246BAE'>Class B</i>",
                                  "Other" = "<i style='color:#80A2C1'>Other</i>")) +
    coord_sf(datum = NA) +
    theme_void(base_size = 4) +
    theme(legend.key.width = unit(0.7, "cm"),
          legend.key.height = unit(0.02, "cm"),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.caption = element_markdown(hjust = 1.0),
          legend.text = element_markdown(size = 3)) +
    labs(title = "River flowing through Japan",
         subtitle = subtitle,
         caption = glue::glue("Source: National Land numerical information (W05 data),<br>Ministry of Land, Infrastructure, Transport and Tourism<br>
       <i>#30daymapchallenge</i> <span style='font-family: \"Font Awesome 5 Brands\"; color:#000000'>&#61595;</span> uribo/30daymapchallenge <span style='font-family: \"Font Awesome 5 Brands\"; color:#55acee'>&#61593;</span> @uribo<br>"))
}

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




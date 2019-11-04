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

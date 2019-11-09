#####################################
# DAY09: 2018年度 都道府県別電力需要実績
#####################################
library(readxl)
library(dplyr)
library(ggplot2)
library(geofacet)
library(ggtext)
library(gghighlight)
library(jpndistrict)
library(cowplot)

parse_demand_xlsx <- function(file) {
  sheets <-
    readxl::excel_sheets(file) %>%
    stringr::str_subset("Sheet1", negate = TRUE) %>%
    .[1:12]
  sheets_mod <-
    sheets %>%
    stringr::str_replace("H28", "2016") %>%
    stringr::str_replace("H29", "2017") %>%
    stringr::str_replace("H30", "2018") %>%
    stringr::str_split("\\.", n = 2) %>%
    purrr::map_chr(~ stringr::str_pad(.x, width = 2, pad = "0") %>%
                     stringr::str_c(collapse = ""))
  purrr::map2_dfr(
    .x = sheets,
    .y = sheets_mod,
    .f =
      ~ read_xlsx(file,
                  sheet = .x,
                  range = "A2:J52") %>%
      slice(-seq.int(3)) %>%
      select(1, ncol(.)) %>%
      purrr::set_names(c("prefecture", "demand")) %>%
      readr::type_convert(col_types = "cd") %>%
      tibble::add_column(yymm = paste0("demand_", .y))
  )
}

input_files <-
  fs::dir_ls(here::here("data-raw"), regexp = "3-2.+.xlsx")

if (length(input_files) != 3L) {
  library(rvest)
  read_html("https://www.enecho.meti.go.jp/statistics/electric_power/ep002/results_archive.html") %>%
    html_nodes(css = '#main > ul > li > a') %>% {
      tibble::tibble(
        name = html_text(.),
        link = html_attr(., name = "href")
      )
    } %>%
    filter(stringr::str_detect(name, "")) %>%
    pull(link) %>%
    stringr::str_c("https://www.enecho.meti.go.jp/statistics/electric_power/ep002/", .) %>%
    purrr::walk(
      ~ download.file(url = .x,
                      destfile = here::here("data-raw", basename(.x)))
    )
}

df_demand <-
  input_files[1] %>%
  purrr::map_dfr(
    parse_demand_xlsx) %>%
  mutate(yymm = forcats::fct_inorder(stringr::str_remove(yymm, "demand_"))) %>%
  left_join(jpndistrict::jpnprefs %>%
              select(prefecture, name = prefecture_en) %>%
              mutate(name = stringr::str_remove(name, "-.+")),
            by = "prefecture")

p_base <-
  df_demand %>%
  ggplot(aes(as.numeric(yymm), demand, group = name, color = name)) +
  geom_line(size = 0.2) +
  scale_color_hue(h = c(40, 60), l = 100, c = 120) +
  ggdark::dark_theme_minimal(base_size = 9, base_family = "Helvetica") +
  guides(color = FALSE) +
  scale_x_continuous(breaks = c(1, 6, 12),
                     labels = c("2018/4", "2018/9", "2019/3")) +
  xlab(NULL) +
  theme(
    plot.title = element_markdown(size = 12, color = "#EEEEEE", face = "bold"),
    plot.subtitle = element_markdown(size = 8, color = "#FFFA58"),
    plot.caption = element_markdown(size = 6, lineheight = 1.1))

p_out <-
  p_base +
  gghighlight(use_direct_label = FALSE,
              unhighlighted_colour = alpha("grey", 0.8)) +　
  theme(axis.text = element_blank(),
        strip.text = element_text(colour = "#FFFA58", size = 6)) +
  facet_geo(~ name,
            grid = "jp_prefs_grid1") +
  guides(color = FALSE) +
  labs(
    title = "Japan Prefecture Electricity Demand in 2018",
    subtitle = "Source: Agency for Natural Resources and Energy of Japan",
    caption = glue::glue("<i>#30daymapchallenge</i> <span style='font-family: \"Font Awesome 5 Brands\"; color:#FFFFFF'>&#61595;</span> uribo/30daymapchallenge <span style='font-family: \"Font Awesome 5 Brands\"; color:#55acee'>&#61593;</span> @u_ribo<br>"))

ggsave(here::here("figures/day09_geofacet_electricity_demand.png"),
       p_out,
       width  = 10,
       height = 7,
       dpi    = 300)

p_summary <-
  p_base +
  gghighlight(name %in% c("Tokyo", "Aichi", "Osaka", "Miyagi", "Hokkaido", "Okinawa", "Fukuoka", "Hiroshima", "Ehime"),
              use_direct_label = FALSE,
              use_group_by = FALSE,
              unhighlighted_colour = alpha("grey", 0.8)) +　
  coord_cartesian(xlim = c(min(as.numeric(df_demand$yymm)), max(as.numeric(df_demand$yymm)) + 1)) +
  ggrepel::geom_text_repel(data = df_demand %>%
                            filter(name %in% c("Tokyo", "Aichi", "Osaka", "Miyagi", "Hokkaido", "Okinawa", "Fukuoka", "Hiroshima", "Ehime")) %>%
                           filter(yymm == "201903"),
                           aes(label = name),
                           segment.color = NA,
                           size = 3,
                           nudge_x = 0.25) +
  scale_y_continuous(name = "(1,000kWh)", labels = scales::comma)

p_bar <-
  df_demand %>%
  group_by(name) %>%
  summarise(demand = sum(demand)) %>%
  ggplot(aes(forcats::fct_reorder(name, demand), demand)) +
  geom_bar(aes(fill = name), stat = "identity") +
  scale_fill_hue(h = c(40, 60), l = 100, c = 120) +
  coord_flip() +
  guides(fill = FALSE) +
  xlab(NULL) +
  scale_y_continuous(name = "(1,000kWh)", labels = scales::comma) +
  ggdark::dark_theme_minimal(base_size = 9, base_family = "Helvetica") +
  theme(axis.text.y  = element_text(color = "#FFFA58", size = 6))

p_out2 <-
  plot_grid(p_summary, p_bar, ncol = 1, rel_heights = c(0.7, 1))

ggsave(here::here("figures/day09_geofacet_electricity_demand2.png"),
       p_out2,
       width  = 4,
       height = 7,
       dpi    = 300)

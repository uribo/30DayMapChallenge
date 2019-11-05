####################################
# DAY05: NOAA's SST Observed Data
####################################
library(rerddap)
library(tidync)
library(ggplot2)
library(ggtext)
library(cptcity)
library(dplyr)
library(lubridate)


# 2019-10 SST -----------------------------------------------------------------
# browse("jplMURSST41")
murSST <- griddap(info("jplMURSST41"),
                  latitude  = c(24., 46.),
                  longitude = c(122., 154.),
                  time      = c("2019-10-10", "2019-10-13"),
                  fields    = "analysed_sst")
nc_sst_201910 <-
  murSST$summary$filename %>%
  tidync()

df_nc_sst_201910 <-
  nc_sst_201910 %>%
  activate("analysed_sst") %>%
  hyper_tibble() %>%
  mutate(date = as_date(as_datetime(time)))

p_out <-
  ggplot() +
  geom_raster(data = df_nc_sst_201910,
              aes(longitude, latitude, fill = analysed_sst)) +
  scale_fill_gradientn(colours = cptcity::cpt("oc_sst")) +
  ggdark::dark_theme_minimal(base_size = 9, base_family = "Helvetica") +
  guides(fill = guide_colorbar(title = NULL,
                               title.position = "top",
                               title.vjust = 0.95)) +
  theme(legend.position = "right",
        legend.key.height = unit(4.0, "line"),
        plot.caption = element_markdown(size = 6)) +
  labs(title = "SST 2019-10-10 to 2019-10-13",
       caption = glue::glue("Source: NOAA's Multi-scale Ultra-high Resolution (MUR) SST Analysis fv04.1, Global, 0.01\u00b0, 2002-present, Daily<br>
       <i>#30daymapchallenge</i> <span style='font-family: \"Font Awesome 5 Brands\"; color:#FFFFFF'>&#61595;</span> uribo/30daymapchallenge <span style='font-family: \"Font Awesome 5 Brands\"; color:#FFFFFF'>&#61593;</span> @uribo<br>")) +
  facet_wrap(~ date, nrow = 1)

ggsave(here::here("figures/day05_sst.png"),
       p_out,
       width = 14,
       height = 5,
       dpi = 300)

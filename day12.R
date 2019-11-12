###########################################
＃ DAY12: 岡電バス運行ルート
###########################################
library(sf)
library(data.table)
library(gtfsrouter)
library(ggplot2)
library(ggspatial)
library(gganimate)
library(ggtext)

gtfs <-
  extract_gtfs("~/Documents/resources/GTFS/okaden.zip")

trip_ids <-
  gtfs$trips[.(gtfs$calendar[.(1),
                             on = c("monday")]),
             on = "service_id"] %>%
  .[, trip_id] %>%
  unique()

dd <-
  merge(gtfs$stop_times[.(trip_ids),
                        on = .(trip_id)] %>%
          .[, -c("stop_headsign", "pickup_type", "drop_off_type", "shape_dist_traveled")],
        gtfs$stops[, c("stop_id", "stop_name", "stop_lat", "stop_lon")],
        by = "stop_id")
setorder(dd, trip_id, stop_sequence)
# dd[, c("arrival_time", "departure_time") := lapply(.SD, hms::as_hms), .SDcols = c("arrival_time", "departure_time")]

sf_gtfs_stops <-
  dd %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)

p_base <-
  ggplot() +
  geom_sf(data = st_make_grid(sf_gtfs_stops, n = 1)) +
  annotation_map_tile(type = "stamenbw", zoomin = -1) +
  theme_void() +
  coord_sf() +
  theme(plot.caption = element_markdown(size = 8, lineheight = 1.1)) +
  labs(title = "Okayama GTFS Busstop {}",
       subtitle = "Data: Okaden Bus (http://www.okayama-kido.co.jp/bus/)",
       caption = glue::glue("Map tiles by Stamen Design, under CC BY 3.0. Data by \U00a9 OpenStreetMap contributors<br><i>#30daymapchallenge</i> <span style='font-family: \"Font Awesome 5 Brands\"; color:#000000'>&#61595;</span> uribo/30daymapchallenge <span style='font-family: \"Font Awesome 5 Brands\"; color:#55acee'>&#61593;</span> @u_ribo<br>"))

p_anime <-
  p_base +
  geom_point(data = dd %>%
               dplyr::mutate(group = seq.int(nrow(.))),
             aes(x = stop_lon, y = stop_lat, color = trip_id,
                 group = interaction(stop_lon, stop_lat)),
             show.legend = FALSE) +
  gganimate::transition_time(arrival_time) +
  shadow_mark(size = 2)
anim_save("figures/day12_bus.gif",
          p_anime,
          width = 500,
          height = 500,
          duration = 100,
          fps = 8)


###########################################
# DAY06: Lake Water Depth
###########################################
library(stars)
library(plotly)
file <- "~/Documents/resources/国土地理院/湖沼調査/kasumigaura-2018/湖沼データ_TIFF_霞ヶ浦/水深グリッド/kasumigaura-dem-2018_ras.tif"
r <-
  read_stars(file)
depth <-
  r %>%
  purrr::pluck(1)
depth <- -depth

p <-
  plot_ly(z = ~ depth,
          colors =  cptcity::cpt(pal = "cmocean_deep")) %>%
  add_surface(contours = list(
    z = list(
      show = TRUE,
      usecolormap = TRUE,
      highlightcolor = "#ff0000",
      project = list(z = TRUE)
    ))) %>%
  layout(
    title ="Kasumigaura\nSource: Geospatial Information Authority of Japan website",
    scene = list(
      camera = list(
        eye = list(x = 0.07, y = 0.08, z = 1.64)
      )
    )
  )
p

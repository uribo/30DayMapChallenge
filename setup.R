library(renv)
install("reticulate")
install(c("sf", "stars", "mapview"))
install(c("tidyverse", "fs", "here", "forcats"))
install("clauswilke/ggtext")

renv::install("fgdr")
renv::install("ropenscilabs/rnaturalearthhires")

renv::install(c("lemon", "rcartocolor"))

renv::install("uribo/jpmesh")
renv::install("uribo/jpnp")
renv::install("crazycapivara/h3forr")

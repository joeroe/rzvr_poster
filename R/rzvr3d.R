# rzvr3d.R
# Renders a pretty, ray-shaded map of the 2018 Razavar Valley Survey

library("tidyverse")
library("magrittr")
library("png")
library("rgdal")
library("raster")
library("rgl")
library("rayshader")
library("RColorBrewer")

# SRTM v3 DEM, 1-arc second resolution, smoothed using Sun's denoising algorithm
#   (n = 5, t = 0.95)
terrain <- raster("data/rzvr_e_srtm1v3s.tif") 

# Sites
sites <- read_csv("data/rzvr_sites.csv")

# Convert latlongs to matrix coordinates on the terrain raster
sites[,c("longitude", "latitude")] %>% 
  as.matrix() %>% 
  project("+proj=utm +zone=38 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>% 
  subtract(extent(terrain) %>% 
             as.matrix() %>% 
             t() %>% 
             magrittr::extract(rep(1, nrow(sites)),)) %>% 
  divide_by(res(terrain)[1]) %>% # assumes square cells
  round() %>% 
  as_tibble() %>% 
  transmute(x = longitude, y = latitude) %>% 
  bind_cols(sites) ->
  sites

# And terrain raster to a matrix
terrain %>% 
  matrix(nrow = ncol(.), ncol = nrow(.)) ->
  terrain

# Survey intensity overlay
raster("data/rzvr_survey_intensity.tif") %>% 
  matrix(nrow = ncol(.), ncol = nrow(.)) %>% 
  t() %>% 
  {array(
    c(matrix(1, nrow(.), ncol(.)), # Red
      matrix(1, nrow(.), ncol(.)), # Green
      matrix(0, nrow(.), ncol(.)), # Blue
      as.integer(. > 0) * 0.33),                        # Alpha
    dim = c(nrow(.), ncol(.), 4)
  )} ->
  survey
survey[survey < 0] <- 0

# Stream overlay
streams <- readPNG("data/rzvr_streams.png")

# Plot
sun <- 90
rayshade <- ray_shade(terrain, sunangle = sun, lambert = TRUE, multicore = TRUE)
ambshade <- ambient_shade(terrain, multicore = TRUE)
terrain %>% 
  sphere_shade(sunangle = sun, texture = "desert") %>% 
  add_overlay(survey) %>% 
  add_overlay(streams) %>%
  add_shadow(rayshade, 0.7) %>% 
  add_shadow(ambshade, 0.7) %>% 
  plot_3d(terrain, zscale = 10, solid = TRUE, theta = 225, phi = 30, zoom = 0.7)

# Labels
palette <- brewer.pal(9, "Set1")
names(palette) <- unique(sites$primary_period)
print(palette)

sites %>% 
  filter(archaeological == TRUE) %>% 
  dplyr::select(name_en, x, y, primary_period) %>% 
  pmap(function(name_en, x, y, primary_period, terrain) {
    render_label(terrain, "", x, y, z = 3500, zscale = 30, 
                 color = palette[primary_period])
  }, terrain = terrain) %>% 
  invisible()

# Bokeh
#render_depth(focus = 0.6, focallength = 200)

# Save snapshot
#render_snapshot("figures/model.png")
#rgl.postscript("figures/model.svg", fmt = "svg")

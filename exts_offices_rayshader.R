library(rayshader)
library(geoviz)
library(gifski)
source("helpers.R")

#Set up the area you want to visualise (ggmap::geocode can help you do this straight from R but you'll need a Google API key)
lat <- 46.3882
long <- 6.2341
square_km <- 40

#Increase this to ~60 for a higher resolution (but slower) image
max_tiles <- 10

#Get elevation data from Mapzen
dem <- mapzen_dem(lat, long, square_km, max_tiles = max_tiles)
campus_biotech <- geoviz::latlong_to_rayshader_coords(dem, 46.2220647,6.1461453)
epfl <- geoviz::latlong_to_rayshader_coords(dem, 46.5168877,6.5600643)

#Draw the rayshader scene
elmat = matrix(
  raster::extract(dem, raster::extent(dem), method = 'bilinear'),
  nrow = ncol(dem),
  ncol = nrow(dem)
)

sunangle <- 270

overlay_image <-
  slippy_overlay(
    dem,
    image_source = "stamen",
    image_type = "watercolor",
    png_opacity = 0.3
  )

#Draw the rayshader scene
scene <- elmat %>%
  sphere_shade(sunangle = sunangle, texture = "desert") %>%
#  add_overlay(overlay_image)
  add_water(detect_water(elmat), color="desert") %>%
  identity()

n_frames <- 80
waterdepths <- transition_values(from = 0, to = min(montereybay), steps = n_frames)
thetas <- transition_values(from = 135, to = 45, steps = n_frames)

img_frames <- paste0("drain", seq_len(n_frames), ".png")
for (i in seq_len(n_frames)) {
  message(paste(" - image", i, "of", n_frames))
  plot_3d(
    scene,
    elmat,
    zscale = raster_zscale(dem),
    #zscale = 150,
    waterdepth = 0,
    wateralpha = 0.5,
    theta=thetas[i],
    phi=30,
    water=TRUE,
    watercolor = "lightblue",
    waterlinealpha = 0.5,
    waterlinecolor = "white",
    solid = TRUE,
    shadow = TRUE,
    shadowdepth = -150,
    windowsize=c(1000,800),
    baseshape = "hex",
    zoom=0.5
  )
  render_label(elmat,x=campus_biotech$x,y=campus_biotech$y, z=4000,zscale=50,
               text = "EXTS GVA",textsize = 2,linewidth = 5)
  render_label(elmat,x=epfl$x,y=epfl$y, z=4000,zscale=50,
               text = "EXTS LSN",textsize = 2,linewidth = 5)
  #render_depth()
  render_snapshot(img_frames[i])
  rgl::clear3d()
}

# build gif
magick::image_write_gif(magick::image_read(img_frames),
                        path = "exts.gif",
                        delay = 6/n_frames)

# Rotated Markers
# from https://stackoverflow.com/questions/50452750/r-leaflet-plot-ship-direction

# load plotting functions
rotatedMarker <- 
  htmlDependency( name = "Leaflet.rotatedMarker", 
                  version = "0.1.2", 
                  src = "./www",
                  script = "leaflet.rotatedMarker.js")

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

wind.icon <- makeIcon( iconUrl = "./www/wind.png", 
                       iconWidth = 25,
                       iconHeight = 25 )


library(dplyr)

fs <- list.files("./gpx/", pattern = "gpx")

routes <- setNames(rep(list(list()), length(fs)), fs)

routes$indieVelo_Grand_Day_Out.gpx <- list(cuts = c(
  444,463,870,977,1082,1281,1323,1565,1953,1970,2022,2078,2191,
  2335,2345,2384,2393,2485,2985,3577,3693,3754,3803,
  4395,4437,4971,5110,5232,5279,5299,5409,5513,5575,5862,6033
))

make_outputs <- function(x, routes) {
  d <- gpxr::load_track_points(file.path("gpx", x))
  
  if("cuts" %in% names(routes[[x]])) {
    d <- filter(d, track_seg_point_id > routes[[x]]$cuts[1] & track_seg_point_id < tail(routes[[x]]$cuts, 1))
  }
  
  d <- gpxr::add_distance(d)
  d <- dplyr::select(d, id = track_seg_point_id, elevation = ele, distance = distance, slope = slope)
  
  out <- file.path("geojson", gsub("gpx", "geojson", x))
  
  unlink(out, force = TRUE)
  
  sf::write_sf(d, out)
  
  out <- file.path("svg", gsub("gpx", "svg", x))
  
  svglite::svglite(out)
  par(mar = c(0,0,0,0))
  plot(d$distance, d$elevation, col = NA)
  lines(d$distance, d$elevation)
  dev.off()
}

sapply(fs, make_outputs, routes = routes)

make_segments <- function(x, routes) {
  d <- gpxr::load_track_points(file.path("gpx", x))
  
  d <- dplyr::select(d, id = track_seg_point_id)
  
  if("cuts" %in% names(routes[[x]])) {
    
    segments <- lapply(1:(length(features) - 1), function(i, d, features) {
      message(i)
      sub <- d[features[i]:features[i+1],]
      line <- sf::st_sfc(
        sf::st_linestring(sf::st_coordinates(sf::st_geometry(sub))),
        crs = sf::st_crs(sub))
    }, d = d, features = features)
    
    return(sf::st_sf(id = seq(1,length(segments)), geom = do.call(c, segments)))
  } else {
    return()
  }
  
}
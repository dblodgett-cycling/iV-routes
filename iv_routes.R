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
  
  d$elevation <- d$elevation - 35
  
  out <- file.path("svg", gsub("gpx", "svg", x))
  
  svglite::svglite(out)
  
  par(mar = c(1,3,0,0))
  
  if(max(d$elevation) > 200) {
    range <- c(0, 250)
  } else if(max(d$elevation) > 150) {
    range <- c(0, 200)
  } else if(max(d$elevation) > 100) {
    range <- c(0, 150)
  } else {
    range <- c(0, 100)
  }
  
  plot(c(min(d$distance), max(d$distance)), range, col = NA, axes = FALSE)
  
  lines(d$distance, d$elevation)

  abline(0, 0, col = "black")
  
  text(0, 0, "0m", pos = 2, offset = 2, xpd = NA)
    
  abline(50, 0, col = "lightgrey", lty = "dashed")
  
  text(0, 50, "50m", pos = 2, offset = 1.5, xpd = NA)
  
  if(max(range) > 100) {
    abline(100, 0, col = "lightgrey", lty = "dashed")
    text(0, 100, "100m", pos = 2, offset = 1.5, xpd = NA)
  }
  
  if(max(range) > 150) {
    abline(150, 0, col = "lightgrey", lty = "dashed")
    text(0, 150, "150m", pos = 2, offset = 1.5, xpd = NA)
  }
  
  if(max(range) > 200) {
    abline(200, 0, col = "lightgrey", lty = "dashed")
    text(0, 200, "200m", pos = 2, offset = 1.5, xpd = NA)
  }
  
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
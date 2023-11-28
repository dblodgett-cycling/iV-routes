library(dplyr)

fs <- list.files("./gpx/", pattern = "gpx")

routes <- setNames(rep(list(list()), length(fs)), fs)

routes$indieVelo_Grand_Day_Out.gpx <- list(cuts = c(
  444,463,870,977,1082,1281,1323,1565,1953,1970,2022,2078,2191,
  2335,2345,2384,2393,2485,2985,3577,3693,3754,3803,
  4395,4437,4971,5110,5232,5279,5299,5409,5513,5575,5862,6033
))

routes$indieVelo_So_Near_Yet_So_Far.gpx <- list(cuts = c(457, 3689))

routes$indieVelo_Northern_Hill_Climb.gpx <- list(cuts = c(1, 1378))

routes$indieVelo_Base_Camp.gpx <- list(cuts = c(6, 1627))

get_color <- function(x, bins = c(-15.1, -5, -3, -1, 0, 1, 3, 5, 8, 15.1)) {

  x <- x * 100

  min <- -15
  max <- 15
  
  x[x < min] <- min
  x[x > max] <- max
  
  breaks <- as.numeric(cut(x, bins))

  c("blue4", "blue", "skyblue", 
    "springgreen", "greenyellow", 
    "yellow", "orange", "red", "purple")[breaks]

}

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
  
  segments(d$distance, 0, d$distance, d$elevation, col = get_color(d$slope))

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
  
  if(!"cuts" %in% names(routes[[x]])) {
    
    routes[[x]]$cuts <- c(1, tail(d$id, 1) - 1)
    
  }
  
  features <- routes[[x]]$cuts
  
  segments <- lapply(1:(length(features) - 1), function(i, d, features) {
    sub <- d[features[i]:features[i+1],]
    line <- sf::st_sfc(
      sf::st_linestring(sf::st_coordinates(sf::st_geometry(sub))),
      crs = sf::st_crs(sub))
  }, d = d, features = features)
  
  return(sf::st_sf(id = seq(1,length(segments)), geom = do.call(c, segments)))
  
}

line_version <- lapply(fs, make_segments, routes = routes)

spatial_svg <- function(polygons_sf, lines_sf) {
  
  par(mar = c(0,0,0,0), family = "Arial Unicode MS")
  plot(sf::st_buffer(sf::st_geometry(lines_sf), dist = units::set_units(100, "m")), border = NA)
  
  for(col_choose in 1:max(polygons_sf$plot_order)) {
    
    dat <- dplyr::filter(polygons_sf, plot_order == col_choose)
    
    plot(sf::st_geometry(dat), col = dat$fill, border = dat$col, lwd = dat$lwd, add = TRUE)
  }
  
  plot(sf::st_geometry(lines_sf), col = "black", lwd = 3, add = TRUE)  
  plot(sf::st_geometry(lines_sf), col = "lightgrey", add = TRUE)  
  
  start_point <- sf::st_sfc(
    sf::st_point(c(sf::st_coordinates(lines_sf)[1,1], sf::st_coordinates(lines_sf)[1,2])),
    crs = sf::st_crs(lines_sf))
  
  plot(start_point, pch = '\u25b7', cex = .75, add = TRUE)
  plot(start_point, pch = '\u25b7', cex = 1.5, add = TRUE)
  
  # text(sf::st_coordinates(start_point), "start", pos = 4)
}

polygons_sf <- sf::read_sf("base_map/gilbert_island_polygons.geojson")

orig_colors <- c("#29902EFF", 
                 "#36963BFF", 
                 "#A6A6A6FF",
                 "#5B95022E", 
                 "#CECECEFF", 
                 "#DFDFDFFA", 
                 "#623A14B0", 
                 "#F6C18FFF",
                 "#78BCFEFF", 
                 "#78BCFEEE",
                 "#ECD5BFE7")

polygons_sf <- mutate(polygons_sf, fill = case_when(fill == "#F6C18FFF" ~ "#943c1e",
                                                    fill == "#ECD5BFE7" ~ "#a98823",
                                                    fill == "#36963BFF" ~ "#3fa644",
                                                    TRUE ~ fill))

for(i in 1:length(fs)) {
  svglite::svglite(file.path("svg", gsub(".gpx", "_map.svg", fs[i])), width = 4, height = 4)
  spatial_svg(polygons_sf, line_version[[i]])
  dev.off()
}

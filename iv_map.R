base <- sf::read_sf("gilbert.geojson")

atts <- readr::read_csv("gilbert_geom.csv")

atts$x <- sf::st_coordinates(base)[,1]
atts$y <- sf::st_coordinates(base)[,2]

strings <- dplyr::group_by(atts, elem_idx) |>
  dplyr::group_split()

geoms <- lapply(1:length(strings), function(x) {

  y <- loops[[x]]
  
  coords <- as.matrix(y[c("x", "y")])  
  
  out <- try(sf::st_polygon(list(coords)), silent = TRUE)
  
  if(inherits(out, "try-error")) {
    out <- try(sf::st_linestring(coords), silent = TRUE)
  }
  
  if(inherits(out, "try-error")) message(x)
  out
})

types <- sapply(geoms, function (x) sf::st_geometry_type(x))

polygons <- geoms[types == "POLYGON"]
lines <- geoms[types == "LINESTRING"]

unique_atts <- function(x) {
  list(col = unique(x$col), 
       fill = unique(x$fill),
       lwd = unique(x$lwd))
}

polygon_atts <- lapply(strings[types == "POLYGON"], unique_atts)
line_atts <- lapply(strings[types == "LINESTRING"], unique_atts)

polygon_atts <- dplyr::bind_rows(polygon_atts)
line_atts <- dplyr::bind_rows(line_atts)

polygons_sf <- sf::st_sf(polygon_atts, geom = sf::st_sfc(polygons))
lines_sf <- sf::st_sf(line_atts, geom = sf::st_sfc(lines))

lake <- lines_sf[1,]

lake <- sf::st_sf(sf::st_drop_geometry(lake), 
                  geom = sf::st_sfc(sf::st_polygon(list(sf::st_coordinates(lake)[1:1951,1:2], 
                                                        sf::st_coordinates(lake)[2434:1954,1:2]))), crs = sf::st_crs(lake))

polygons_sf <- dplyr::bind_rows(polygons_sf, lake)

lines_sf <- lines_sf[2:nrow(lines_sf),]

plot(sf::st_geometry(polygons_sf), col = polygons_sf$fill, border = polygons_sf$col, lwd = polygons_sf$lwd)

order <- c("#29902EFF", 
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

plot(sf::st_geometry(polygons_sf))
for(col_choose in 1:length(order)) {
  polygons_sf <- dplyr::mutate(polygons_sf, plot_order = col_choose)
  
  dat <- dplyr::filter(polygons_sf, fill == order[col_choose])
  
  plot(sf::st_geometry(dat), col = , border = dat$col, lwd = dat$lwd, add = TRUE)
}

plot(sf::st_geometry(lines_sf), add = TRUE)

sf::st_crs(polygons_sf) <- sf::st_crs(3857)
sf::st_crs(lines_sf) <- sf::st_crs(3857)

polygons_sf <- sf::st_transform(polygons_sf, 4326)
lines_sf <- sf::st_transform(lines_sf, 4326)

m <- leaflet::leaflet(polygons_sf) |>
  leaflet::addPolygons(color = polygons_sf$col, fillColor = polygons_sf$fill, opacity = 1, fillOpacity = 1) |>
  leaflet::addPolylines(data = lines_sf, color = lines_sf$col, opacity = 1, weight = 1)
  
library(htmlwidgets)
saveWidget(m, file="base_map/gilbert_island.html", selfcontained = TRUE)

sf::write_sf(polygons_sf, "gilbert_island_polygons.geojson")
sf::write_sf(lines_sf, "gilbert_island_lines.geojson")

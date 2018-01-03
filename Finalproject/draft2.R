####################################################
# week 2: build geoms
####################################################


#' @title GeomTimeline
#' @description GeomHurricane constructs a ggproto object  to display the hurricane radii
#'
#' @examples
#' \dontrun{geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
#' na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
#' ggplot2::layer(geom = GeomTimeline, mapping = mapping, data = data, stat = stat,
#' position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#' params = list(na.rm = na.rm, ...))}}
#'
#' @importFrom ggplot2 ggproto Geom aes draw_key_point
#' @importFrom grid pointsGrob unit gpar
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 required_aes = c("x"),
                                 default_aes = ggplot2::aes(y = 0,
                                                            colour = "#66C2A4",
                                                            fill = "#41B6C4",
                                                            size = 4,
                                                            stroke = 1,
                                                            alpha = 0.4,
                                                            shape = 19),
                                 draw_key = ggplot2::draw_key_point,
                                 draw_panel = function(data, panel_scales, coord, na.rm = FALSE) {
                                   
                                   coords <- coord$transform(data, panel_scales)
                                   
                                   points <- grid::pointsGrob(
                                     x = coords$x,
                                     y = coords$y,
                                     pch = coords$shape,
                                     size = grid::unit(coords$size/4, "char"),
                                     gp = grid::gpar(col = coords$colour,
                                                     alpha = coords$alpha))
                                   
                                 }
)


#' @title GeomTimelineLabel
#' @description GeomHurricane constructs a ggproto object  to display the hurricane radii
#'
#' @examples
#' \dontrun{geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
#' position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
#' ggplot2::layer(geom = GeomTimelineLabel, mapping = mapping, data = data, stat = stat,
#' position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#' params = list(na.rm = na.rm, ...))}}
#'
#' @importFrom ggplot2 ggproto Geom aes draw_key_point
#' @importFrom dplyr mutate_ rename bind_rows arrange
#' @importFrom grid textGrob polylineGrob grobTree gpar
#' @importFrom geosphere destPoint
#' @importFrom magrittr %>%
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                                      required_aes = c("x", "label"),
                                      default_aes = ggplot2::aes(y = 0, angle = 45),
                                      draw_key = ggplot2::draw_key_point,
                                      draw_panel = function(data, panel_scales, coord) {
                                        coords <- coord$transform(data, panel_scales)
                                        
                                        line_coords <- coords %>%
                                          dplyr::mutate_(y = ~ y + 0.1) %>%
                                          dplyr::bind_rows(coords) %>%
                                          dplyr::arrange(x) %>%
                                          dplyr::mutate_(group = ~rep(1:nrow(coords), each = 2))
                                        
                                        text <- grid::textGrob(
                                          label = coords$label,
                                          x = coords$x,
                                          y = coords$y + 0.1,
                                          rot = 45,
                                          just = c("left", "center"),
                                          gp = grid::gpar(
                                            col = "#810F7C",
                                            fontsize = 3 * .pt
                                          ))
                                        
                                        lines <- grid::polylineGrob(
                                          x = line_coords$x,
                                          y = line_coords$y,
                                          id = line_coords$group,
                                          gp = grid::gpar(
                                            col = alpha("gray20", 0.25),
                                            lwd = 0.4 * .pt
                                          )
                                        )
                                        
                                        grid::grobTree(lines, text)
                                        
                                        
                                      }
)


#' @title NOAA timeline theme
#'
#' @description A simple theme fot the NOAACourser package
#'
#' @examples
#' \dontrun{ggplot() +
#' geom_timeline(data = eq_20e, aes(x = DATE, size = EQ_PRIMARY, y = COUNTRY, color = DEATHS)) +
#' scale_size_continuous(name  ="Richter scale value") +
#' scale_colour_continuous(name  ="# deaths") +
#' theme_timeline}
#'
#' @importFrom ggplot2 theme theme_bw element_line element_blank element_text
#' @export
theme_timeline <-   function(){
  ggplot2::theme_bw() +
    ggplot2::theme(legend.position='bottom',
                   legend.title = ggplot2::element_text(face = "bold" ),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.title.y	= ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_line(size = 1),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.line.x  = ggplot2::element_line(size = 1)
    )}

#' @title geom_timeline
#' @description Plots hurricane radii on a ggmap object representing the maximum windspeed
#' @inheritParams ggplot2::geom_point
#' @importFrom ggplot2 layer
#'
#' @examples
#' \dontrun{ggplot() +
#' geom_timeline(data = eq_20e, aes(x = DATE, size = EQ_PRIMARY, y = COUNTRY, color = DEATHS)) +
#' scale_size_continuous(name  ="Richter scale value") +
#' scale_colour_continuous(name  ="# deaths") +
#' theme_timeline}
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(geom = GeomTimeline, mapping = mapping, data = data, stat = stat,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, ...))
}


#' @title geom_timeline_label
#' @description Plots hurricane radii on a ggmap object representing the maximum windspeed
#' @inheritParams ggplot2::geom_text
#'
#' @examples
#' \dontrun{ggplot() +
#' geom_timeline(data = eq_20e, aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, color = DEATHS)) +
#' geom_timeline_label(data = noteworthy_eq, aes(x = DATE, y = COUNTRY, label = LOCATION_NAME)) +
#' scale_size_continuous(name  ="Richter scale value") +
#' scale_colour_continuous(name  ="# deaths") +
#' theme_timeline}
#'
#' @importFrom ggplot2 layer
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
                                na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(geom = GeomTimelineLabel, mapping = mapping, data = data, stat = stat,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, ...))
}

#########semana 3 la facil 
####################################################
# week 3: build leaflet
####################################################

#' @title eq_map
#' @description maps earthquakes on a leaflet object
#' @param eq_data a dataframe containing info on earthquakes. The data frame should have the following columns: LONGITUDE, LATITUDE, EQ_PRIMARY
#' @param  annot_col the name of the column containing information to show on the map.
#' @examples
#' \dontrun{readr::read_delim(eq_raw_file, delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#' eq_map(annot_col = "DATE")}
#'
#' @importFrom leaflet leaflet addCircleMarkers
#' @export
eq_map <- function(eq_data, annot_col){
  annot <- eq_data %>% magrittr::extract2(annot_col) %>% as.character
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = eq_data, lng = ~ LONGITUDE, lat = ~ LATITUDE, radius = ~ EQ_PRIMARY, weight = 2, popup = annot)
}


#' Creates a label for leaflet map
#'
#' This function creates a label for the \code{leaflet} map based on location
#' name, magnitude and casualties from NOAA earthquake data
#'
#' @param data A data frame containing cleaned NOAA earthquake data
#'
#' @return A character vector with labels
#'
#' @details The input \code{data.frame} needs to include columns LOCATION_NAME,
#' EQ_PRIMARY and TOTAL_DEATHS with the earthquake location, magintude and
#' total casualties respectively.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' eq_create_label(data)
#' }
eq_create_label <- function(data) {
  popup_text <- with(data, {
    part1 <- ifelse(is.na(LOCATION_NAME), "",
                    paste("<strong>Location:</strong>",
                          LOCATION_NAME))
    part2 <- ifelse(is.na(EQ_PRIMARY), "",
                    paste("<br><strong>Magnitude</strong>",
                          EQ_PRIMARY))
    part3 <- ifelse(is.na(TOTAL_DEATHS), "",
                    paste("<br><strong>Total deaths:</strong>",
                          TOTAL_DEATHS))
    paste0(part1, part2, part3)
  })
}

NOAA %>% 
  eq_clean_data() %>% 
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(Date) >= 2000) %>% 
  dplyr::mutate(popup_text = eq_create_label(.)) %>% 
  eq_map(annot_col = "popup_text")

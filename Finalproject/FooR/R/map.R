#' @title GeomTimeline
#' @description GeomHurricane constructs a ggproto object to display a map with leaftlet
#'
#' @importFrom ggplot2 ggproto Geom aes draw_key_point
#' @importFrom grid pointsGrob unit gpar
#'
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 required_aes = c("x"),
                                 default_aes =
                                 ggplot2::aes(y = 0, colour = "#66C2A4",
                                              fill = "#41B6C4", size = 4,
                                              stroke = 1, alpha = 0.4,
                                                            shape = 19),
                                 draw_key = ggplot2::draw_key_point,

                                 draw_panel = function(data, panel_scales, coord, na.rm = FALSE)
                                   {
                                    coords <- coord$transform(data, panel_scales)
                                    points <- grid::pointsGrob(
                                    x = coords$x,
                                    y = coords$y,
                                    pch = coords$shape,
                                    size = grid::unit(coords$size/4, "char"),
                                    gp = grid::gpar(col = coords$colour, alpha = coords$alpha))

                                 }
)

#' @title GeomTimelineLabel
#' @description GeomTimelineLabel constructs a ggproto object to display a map with leaftlet
#'
#' @importFrom ggplot2 ggproto Geom aes draw_key_point .pt alpha
#' @importFrom dplyr mutate_ rename bind_rows arrange
#' @importFrom grid textGrob polylineGrob grobTree gpar

GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                                      required_aes = c("x", "label"),
                                      default_aes = ggplot2::aes(y = 0, angle = 45),
                                      draw_key = ggplot2::draw_key_point,
                                      draw_panel = function(data, panel_scales, coord)
                                        {

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
                                        col = "#810F7C", fontsize = 3 * ggplot2::.pt  ))

                                        lines <- grid::polylineGrob(
                                          x = line_coords$x,
                                          y = line_coords$y,
                                          id = line_coords$group,
                                          gp = grid::gpar(
                                          col = ggplot2::alpha("gray20", 0.25),
                                          lwd = 0.4 * ggplot2::.pt) )

                                        grid::grobTree(lines, text)
                                      }
)

#' @title theme_timeline
#'
#' @description A pretty and simple theme fot the NOAA data
#'
#' @examples
#' \dontrun{`%>%` <- magrittr::`%>%`NOAA %>% eq_clean_data() %>%
#' dplyr::filter(COUNTRY %in% c("MEXICO"), YEAR > 2000, DEATHS>0) %>%
#' ggplot2::ggplot(ggplot2::aes(x = Date, y = COUNTRY, color = DEATHS,
#' size = as.numeric(EQ_PRIMARY)  )) + geom_timeline() }
#'
#' @importFrom ggplot2 theme theme_bw element_line element_blank element_text
#' @export

theme_timeline <-   function()
  {
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position='bottom',
                   legend.title = ggplot2::element_text(face = "plain" ),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.title.y	= ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_line(size = 2, colour = '#7FCDBB'),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.line.x  = ggplot2::element_line(size = 2, colour = '#7FCDBB')
    )}


#' @title geom_timeline
#'
#' @description The function to group the elements to complement the theme
#'
#' @examples
#' \dontrun{NOAA %>% eq_clean_data() %>% dplyr::filter(COUNTRY %in% c("MEXICO"),
#' YEAR > 2000, DEATHS>0) %>% ggplot2::ggplot(ggplot2::aes(x = Date, y = COUNTRY,
#' color = DEATHS, size = as.numeric(EQ_PRIMARY)  )) + geom_timeline()}
#'
#' @importFrom ggplot2 layer theme theme_bw element_line element_blank element_text
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(geom = GeomTimeline, mapping = mapping, data = data, stat = stat,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, ...))
}

#' @title geom_timeline_label
#' @description The function to group the elements to complement the theme with labels
#' @inheritParams ggplot2::geom_text
#'
#' @examples
#' \dontrun{NOAA %>% eq_clean_data() %>% dplyr::filter(COUNTRY %in% c("USA"),
#'  YEAR > 2000, DEATHS>0) %>% ggplot2::ggplot(ggplot2::aes(x = Date,
#'  y = COUNTRY, color = DEATHS, size = as.numeric(EQ_PRIMARY)  )) +
#'  geom_timeline() + geom_timeline_label(ggplot2::aes(label = LOCATION_NAME))}
#'
#' @importFrom ggplot2 layer
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
                                na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...)
  {
    ggplot2::layer(geom = GeomTimelineLabel, mapping = mapping, data = data, stat = stat,
                     position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                     params = list(na.rm = na.rm, ...))
  }

#' @title eq_map
#' @description Leaftlet map of earthquakes by country
#' @param eq_data a dataframe containing info on earthquakes. The data frame should be the output of 'eq_clean_data' function
#' @param  annot_col the name of the variable containing information to show on the map.
#' @examples
#' \dontrun{NOAA %>% eq_clean_data() %>% dplyr::filter(COUNTRY == "USA" & lubridate::year(Date) >= 2000) %>%
#' dplyr::mutate(popup_text = eq_create_label(.)) %>% eq_map(annot_col = "popup_text")}
#' @importFrom leaflet leaflet addCircleMarkers addProviderTiles addTiles
#' @export
#'
eq_map <- function(eq_data, annot_col)
  {
    annot <- eq_data %>% magrittr::extract2(annot_col) %>% as.character
    leaflet::leaflet() %>%
    leaflet::addProviderTiles(provider = "Esri.WorldTerrain") %>%
    #leaflet::addProviderTiles('OpenStreetMap.DE') %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = eq_data, lng = ~ LONGITUDE,
                              lat = ~ LATITUDE, radius = ~ EQ_PRIMARY, weight = 2, popup = annot, color = c("#6962df"))
  }

#' @title eq_create_label
#' @description Creates a label for leaflet map
#' @param data a dataframe containing cleaned NOAA (dataframe)
#' @return A character vector with labels
#' @export
#'
#' @examples
#' \dontrun{ eq_create_label(data) }
eq_create_label <- function(data)
  {
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


#' Geom for visualization of hurricane radius
#' 
#' This geom adds a radial quadrants for the wind speed based on the central 
#' position defined by \code{longitude} and \code{latitude}. It plot a radial 
#' chart based on radii that are 80% of these maximum wind radii.
#' 
#' @param longitude coordinate of the central position
#' @param latitude coordinate of the central position
#' @param ne northeast
#' @param nw northwest
#' @param se southeast
#' @param sw southwest
#' @param fill coloring intensity of the speed
#' @param color coloring intensity of the speed
#' 
#' @import dplyr
#' @import tidyr
#' @import geosphere
#' 
#' @examples 
#' library(ggmap)
#' Katarina_hurricane <- data_frame(storm_id = rep("Katrina-2005", 3),
#'                                 date = rep("2005-08-29 12:00:00",3),
#'                                 latitude = c(29.5, 29.5, 29.5),
#'                                 longitude = c(-89.6, -89.6, -89.6),
#'                                 wind_speed = c("34", "50", "64"),
#'                                 ne = c(200, 120, 90),
#'                                 nw = c(100, 75, 60),
#'                                 se = c(200, 120, 90),
#'                                 sw = c(150, 75, 60))
#'get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%
#'  ggmap(extent = "device") +
#'  geom_hurricane_radius(data = Katarina_hurricane,
#'                        aes(x = longitude, y = latitude, 
#'                            ne = ne, se = se, nw = nw, sw = sw,
#'                            fill = wind_speed, color = wind_speed)) + 
#'  scale_color_manual(name = "Wind speed (kts)", values = c("red", "orange", "yellow")) + 
#'  scale_fill_manual(name = "Wind speed (kts)", values = c("red", "orange", "yellow"))
#' 
#' @export
GeomRadius <- ggproto("GeomPolygon", Geom,
                      required_aes = c("x", "y", "ne", "se", "nw", 
                                       "sw","fill", "colour"),
                      default_aes = aes(scale_radii = 0.8, alpha = 0.8, 
                                        linetype = 1, size = 0.5),
                      draw_key = draw_key_polygon,
                      draw_group = function(data, panel_scales, coord){
                        mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
                          condition <- eval(substitute(condition), .data, envir)
                          .data[condition, ] <- .data[condition, ] %>% mutate(...)
                          .data
                        }
                        bearingDF <- tibble::data_frame(bearing = c(360,1:90,90:180,180:270,270:360),
                                                        direction = rep(c("ne", "se", "sw", "nw"),
                                                                        each = 91)) %>%
                          dplyr::bind_rows(tibble::data_frame(bearing = rep(0, 4),
                                                              direction = c("ne", "se", "sw", "nw")))
                        data <- data %>%
                          dplyr::select(x, y, ne, nw, se, sw, colour, fill,
                                        PANEL, group, scale_radii, alpha, linetype,size) %>%
                          tidyr::gather(direction, distance, -x, -y, -colour, -fill, -PANEL, 
                                        -group, -scale_radii, -alpha, -linetype, -size) %>%
                          dplyr::mutate(distance = distance * 1852 * scale_radii) %>%
                          dplyr::left_join(bearingDF, by = "direction") %>%
                          mutate_cond(bearing == 0, distance = 0)
                           
                          data <- data %>%
                            dplyr::bind_cols(as.data.frame(geosphere::destPoint(as.matrix(data[,1:2]),
                                                                                data$bearing,
                                                                                data$distance))) %>%
                            dplyr::select(-x, -y) %>%
                            dplyr::rename(x = lon, y = lat)
                          coords <- coord$transform(data, panel_scales)
                          first_row <- coords[1, , drop = FALSE]
                          grid::polygonGrob(coords$x, coords$y, default.units = "native",
                                            gp = grid::gpar(col = first_row$colour,
                                                            fill = scales::alpha(first_row$fill, first_row$alpha),
                                                            lwd = first_row$size * .pt,
                                                            lty = first_row$linetype
                                                           )
                                            )
                        })

geom_hurricane_radius <- function(mapping = NULL, 
                                  data = NULL, 
                                  stat = "identity", 
                                  position = "identity",
                                  na.rm = FALSE, 
                                  show.legend = NA, 
                                  inherit.aes = TRUE, ...){
  layer(geom = GeomRadius, mapping = mapping, data = data, stat = stat,
        position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes,  params = list(na.rm = na.rm, ...))}



###################### Visualiazation of Ike hurricane radii ######################

library(readr)
library(dplyr)
library(ggmap)
library(tidyr)

ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")

ext_tracks <- read_fwf("ebtrk_atlc_1988_2015.txt", 
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")

IKE_hurricane <- ext_tracks %>% 
  filter(storm_name == "IKE" & day == 13 & hour == 12) %>% 
  mutate(longitude = longitude*(-1), 
         storm_id = "Ike-2008", 
         date = paste(paste(year,month,day, sep = "-"), paste(hour, "00", "00", sep = ":")))

IKE_selected <- IKE_hurricane %>% 
  gather(wind_speed, ne, ends_with("ne")) %>% 
  mutate(wind_speed = c(34, 50, 64)) %>% 
  right_join(IKE_hurricane %>% 
               gather(wind_speed, nw, ends_with("nw")) %>% 
               mutate(wind_speed = c(34, 50, 64)), by = "wind_speed") %>% 
  right_join(IKE_hurricane %>% 
               gather(wind_speed, se, ends_with("se")) %>% 
               mutate(wind_speed = c(34, 50, 64)), by = "wind_speed") %>%
  right_join(IKE_hurricane %>% 
               gather(wind_speed, sw, ends_with("sw")) %>% 
               mutate(wind_speed = c(34, 50, 64)), by = "wind_speed") %>%
  rename(storm_id = storm_id.x, date = date.x, 
         latitude = latitude.x, longitude = longitude.x) %>% 
  select(storm_id, date, latitude, longitude, wind_speed, ne, nw, se, sw) %>% 
  mutate(wind_speed = as.character(wind_speed))

get_map("Texas", zoom = 5, maptype = "toner-background", source = "stamen") %>%
  ggmap(extent = "device") +
  geom_hurricane_radius(data = IKE_selected,
                        aes(x = longitude, y = latitude,
                            ne = ne, se = se, nw = nw, sw = sw,
                            fill = wind_speed, color = wind_speed)) +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow"))

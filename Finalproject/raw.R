### Clean and load
library(readr)
library(lubridate)
library(ggplot2)
data <- read_delim(file ='C:\\Users\\fou-f\\Desktop\\AdvanceR\\Finalproject\\results', delim = '\t',guess_max = 5420) 
fecha <- function(n)
{
  if(nchar(data[n, 'YEAR']) == 5 & as.numeric(data[n, 'YEAR']) < 0)
    return(as.character(data[n, 'YEAR']))
  if(nchar(data[n, 'YEAR']) == 4 & as.numeric(data[n, 'YEAR']) > 0)
     return(as.character(data[n, 'YEAR']))
  if(as.numeric(data[n, 'YEAR'])>0 )
    {
      if(nchar(as.character(data[n, 'YEAR']))==3)
        return(as.character(paste0('0', data[n, 'YEAR'])))
      if(nchar(as.character(data[n, 'YEAR']))==2)
        return(as.character(paste0('00', data[n, 'YEAR'])))
      if(nchar(as.character(data[n, 'YEAR']))==1)
        return(as.character(paste0('000', data[n, 'YEAR'])))
      if(as.character(nchar(data[n, 'YEAR']))==0)
        return(as.character(paste0(0000, data[n, 'YEAR'])))
  } else  {
   # if(nchar(data[n, 'YEAR'])==4)
    #  return(as.character(paste0('-0', substr(data[n, 'YEAR'], 2,4))))
    #if(nchar(data[n, 'YEAR'])==3)
     # return(as.character(paste0('-00', substr(data[n, 'YEAR'], 2,3))))
    #if(nchar(data[n, 'YEAR'])==1)
     # return(as.character(paste0('-000', substr(data[n, 'YEAR'], 2,4))))
    return(as.character(data[n, 'YEAR']))
  }
}
data[is.na(data[, 'YEAR']),'YEAR'] <- 'NA'
data[is.na(data[, 'MONTH']),'MONTH'] <- 'NA'
data[is.na(data[, 'DAY']),'DAY'] <- 'NA'
data$YEAR2 <- 0
data$YEAR2 <- mapply(fecha, 1:dim(data)[1])
data$Date2<- paste(data$YEAR2, data$MONTH, data$DAY, sep ='.')
data$Date <- ymd(data$Date2)
NOAA  <- data[-which(is.na(data$Date )),]
NOAA$LATITUDE <- as.numeric(NOAA$LATITUDE)
NOAA$LONGITUDE<- as.numeric(NOAA$LONGITUDE)
eq_location_clean<- function(NOAA)
{
  NOAA$LOCATION_NAME <- tolower(gsub(".*:","",NOAA$LOCATION_NAME))
  NOAA
}
NOAA$LOCATION_NAME <- eq_location_clean(NOAA)
NOAA <- as.data.frame(NOAA)
NOAA$DEATHS <- as.numeric(NOAA$DEATHS)
NOAA$YEAR <- as.numeric(NOAA$YEAR)
NOAA$EQ_MAG_ML <- as.numeric(NOAA$EQ_MAG_ML)

NOAA2 <- NOAA[, c('EQ_MAG_ML', 'YEAR', 'DEATHS')]

############### geom 
####################################################

GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 required_aes = c("x"),
                                 default_aes = ggplot2::aes(y = 0,
                                                            colour = "green4",
                                                            fill = "white",
                                                            size = 2,
                                                            stroke = 1,
                                                            alpha = 0.5,
                                                            shape = 20),
                                 draw_key = ggplot2::draw_key_point,
                                 draw_panel = function(data, panel_scales, coord, na.rm = FALSE) 
                                 {
                                   
                                   coords <- coord$transform(data, panel_scales)
                                   points <- grid::pointsGrob(
                                     x = coords$x,
                                     y = coords$y,#rep
                                     pch = coords$shape,
                                     size = grid::unit(coords$size/3, "char"),
                                     gp = grid::gpar(col = coords$colour,
                                                     alpha = coords$alpha))
                                   
                                 }
)

GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                                      required_aes = c("x", "label"),
                                      default_aes = ggplot2::aes(y = 0, angle = 60),
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
                                            col = "black",
                                            fontsize = 4 * .pt
                                          ))
                                        
                                        lines <- grid::polylineGrob(
                                          x = line_coords$x,
                                          y = line_coords$y,
                                          id = line_coords$group,
                                          gp = grid::gpar(
                                            col = alpha("gray20", 0.25),
                                            lwd = 0.5 * .pt
                                          )
                                        )
                                        
                                        grid::grobTree(lines, text)
                                      }
)


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

geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(geom = GeomTimeline, mapping = mapping, data = data, stat = stat,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, ...))
}
ggplot(aes(x = YEAR,
           y = COUNTRY,
           color = DEATHS,
           size = as.numeric(EQ_PRIMARY)
)) +
  geom_timeline() 

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
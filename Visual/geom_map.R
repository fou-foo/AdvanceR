#' Load the library to use the '%>%' operator
library(magrittr)
#' set your current directory
setwd('/home/fou/Desktop/Online/RCoursera/MasteringSoftwareDevelopmentR/AdvanceR/Visual')
#' Prepare the data...
#' Read the data
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
ext_tracks <- readr::read_fwf("ebtrk_atlc_1988_2015.txt", 
                       readr::fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")
ext_tracks$date <- lubridate::ymd_hms(with(ext_tracks, paste0(paste(year, month, day, sep = '-'),' ',
                        paste0(hour,":00:00"))))  
#' Construct 'date' column to 'date' type
#' Subset data for only a hurricane in a datetime 
huracanes <- c('IKE') #unique(ext_tracks$storm_name)[160	 Select a Hurricane 
data <- ext_tracks %>% dplyr::filter( storm_name %in% huracanes, as.character(date)  %in% c('2008-09-13 06:00:00') ) %>% dplyr::select(storm_name, date, latitude, 
                                                                        longitude, 
                                                                        radius_34_ne, radius_34_nw, radius_34_se, radius_34_sw,
                                                                        radius_50_ne, radius_50_nw, radius_50_se, radius_50_sw,
                                                                        radius_64_ne, radius_64_nw, radius_64_se, radius_64_sw, day , hour)
a1 <- data %>% tidyr::gather(key = radius1,  value = ne, radius_34_ne, radius_50_ne, radius_64_ne) %>% 
                dplyr::select(storm_name, date, latitude, longitude,  ne   )
a2 <- data %>% tidyr::gather(key = radius2,  value = nw, radius_34_nw, radius_50_nw, radius_64_nw) %>% 
                dplyr::select(storm_name, date, latitude, longitude,  nw   )
a3 <- data %>% tidyr::gather(key = radius3,  value = se, radius_34_se, radius_50_se, radius_64_se) %>% 
  dplyr::select(storm_name, date, latitude, longitude, radius3, se   )
a4 <- data %>% tidyr::gather(key = radius4,  value = sw, radius_34_sw, radius_50_sw, radius_64_sw) %>% 
  dplyr::select(storm_name, date, latitude, longitude, radius4, sw   ) 
a1$nw <- a2$nw
a1$se <- a3$se
a1$sw <- a4$sw
a1$wind_speed <- factor(c(34, 50, 64))
a1$longitude <- - a1$longitude
storm_observation <- a1

#' Definition of 'hurricane' proto it's inherate from 'geom' 
#' 
#' @param data a dataframe containing data to plot see the example 'storm_observation'
#' to see the requiered structure
#' 
#' @return a 'hurricane' polygon for use with a map plot    
#' 
hurricane <- ggproto('hurricane', Geom,
					 required_aes = c("x", "y","colour", "fill", "rne","rse","rsw","rnw"),
					 default_aes = aes(shape = 1, alpha = 0.65, scale_radii = 1),
					 draw_key = draw_key_polygon,
					 draw_group = function(data, panel_scales, coord) 
					 {
					 	x <- data$x[1]  #Retrieve localitation to plot
					 	y <- data$y[1]
					  	scale = 2000* data$scale_radii[1]  #It's exist because each parameter in aes() is a column in 'data' 
					 	#transform using the reconmedation of the mentors
					 	ne <- geosphere::destPoint( p=c(x,y), 0:90, data$rne[1]*scale)
					 	se <- geosphere::destPoint( p=c(x,y), 91:180, data$rse[1]*scale)
					 	sw <- geosphere::destPoint( p=c(x,y), 181:270, data$rsw[1]*scale)
					 	nw <- geosphere::destPoint( p=c(x,y), 271:360, data$rnw[1]*scale)
					 	polars <- do.call(rbind, list(ne, se, sw, nw))
					 	polars <- as.data.frame(polars)
					 	names(polars) <- c("x","y")
					 	polars <- cbind(polars, data[, !(names(data) %in% names(polars)) ])
					 	coords <- coord$transform(polars, panel_scales)
					 	grid::polygonGrob(x=coords$x, y = coords$y, 
					 					  gp = grid::gpar(col = coords$colour, fill=coords$fill, alpha=coords$alpha))
					 }
)


#' Function to plot the object returned by 'hurricane' over other ggplot object.
#' 
#' @param  @param data a dataframe containing data to plot see the example 'storm_observation'
#' to see the requiered structure
#' 
#' @return a ggplot layer with the concentrict information
#' 
#' @examples
#'  \dontrun{
#'  
#'  library(ggmap)
#'  get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%
#'	ggmap(extent = "device") +
#'	geom_foo(data = storm_observation,
#'				   aes(x = longitude, y = latitude, 
#'				   	r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
#'				   	fill = wind_speed, color = wind_speed)) + 
#'	scale_color_manual(name = "Wind speed (kts)", 
#'					   values = c("red", "orange", "yellow")) + 
#'	scale_fill_manual(name = "Wind speed (kts)", 
#'					  values = c("red", "orange", "yellow"))
#' }
#' 

geom_foo <- function(mapping = NULL, data = NULL, stat = "identity",
					 position = "identity", na.rm = FALSE,
					 show.legend = T, inherit.aes = T, ...) {
	ggplot2::layer(
		geom = hurricane, mapping = mapping,
		data = data, stat = stat, position = position,
		show.legend = show.legend, inherit.aes = inherit.aes,
		params = list(na.rm = na.rm, ...)
	)
}





map <-get_map("Louisiana", zoom = 6, maptype = "toner-background") %>% ggmap(extent = "device")
first_size <- map +	geom_foo(data = storm_observation, aes(x = longitude, y = latitude, colour=wind_speed, fill=wind_speed,
							rne = ne, rse = se, rnw = nw, rsw = sw, scale_radii = 1)) +
	scale_color_manual(name = "Wind speed (kts)", 
					   values = c("red", "orange", "yellow")) + 
	scale_fill_manual(name = "Wind speed (kts)", 
					  values = c("red", "orange", "yellow"))
second_size <- map +	geom_foo(data = storm_observation, aes(x = longitude, y = latitude, colour=wind_speed, fill=wind_speed,
														   rne = ne, rse = se, rnw = nw, rsw = sw, scale_radii = .5)) +
	scale_color_manual(name = "Wind speed (kts)", 
					   values = c("red", "orange", "yellow")) + 
	scale_fill_manual(name = "Wind speed (kts)", 
					  values = c("red", "orange", "yellow"))


maps <- gridExtra::grid.arrange(first_size, second_size, ncol = 2)

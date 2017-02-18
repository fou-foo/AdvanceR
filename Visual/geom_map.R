library(magrittr)
setwd('/home/fou/Desktop/Online/RCoursera/MasteringSoftwareDevelopmentR/AdvanceR/Visual')
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
huracanes <- unique(ext_tracks$storm_name)[160]
data <- ext_tracks %>% dplyr::filter( storm_name %in% huracanes) %>% dplyr::select(storm_name, date, latitude, 
                                                                        longitude, max_wind, rad_max_wind,
                                                                        radius_34_ne, radius_34_nw, radius_34_se, radius_34_sw,
                                                                        radius_50_ne, radius_50_nw, radius_50_se, radius_50_sw,
                                                                        radius_64_ne, radius_64_nw, radius_64_se, radius_64_sw)
a1 <- data %>% tidyr::gather(key = radius1,  value = ne, radius_34_ne, radius_50_ne, radius_64_ne) %>% 
                dplyr::select(storm_name, date, latitude, longitude, max_wind, rad_max_wind,  ne   )
a2 <- data %>% tidyr::gather(key = radius2,  value = nw, radius_34_nw, radius_50_nw, radius_64_nw) %>% 
                dplyr::select(storm_name, date, latitude, longitude, max_wind, rad_max_wind,  nw   )
a3 <- data %>% tidyr::gather(key = radius3,  value = se, radius_34_se, radius_50_se, radius_64_se) %>% 
  dplyr::select(storm_name, date, latitude, longitude, max_wind, rad_max_wind, radius3, se   )
a4 <- data %>% tidyr::gather(key = radius4,  value = sw, radius_34_sw, radius_50_sw, radius_64_sw) %>% 
  dplyr::select(storm_name, date, latitude, longitude, max_wind, rad_max_wind, radius4, sw   ) 
a1$nw <- a2$nw
a1$se <- a3$se
a1$sw <- a4$sw
datos  <- a1 %>% dplyr::arrange(storm_name, date)
momentos <- sort(unique(datos$date))[40]
storm_observation <- subset(datos, date %in% momentos )
storm_observation$longitude <- -storm_observation$longitude
#mapa <- ggplot2::ggplot(datos.Huracan, ggplot2::aes(-longitude, latitude)) + 
#	ggplot2::geom_path() + ggplot2::theme_void()
library(ggmap)
mapa <- get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%
	ggmap(extent = "device")
mapa
geom <- function(storm_observation, mapa)
{
	
	d <- storm_observation%>% tidyr::gather(key = tipo, value = power, ne, nw, se, sw)
	d$origen <- c('34', '50', '64')
	color <- c('red', 'orange', 'yellow')
	dona <- ggplot2::ggplot(d, ggplot2::aes(x = factor(tipo), y =  power, alpha = I(0.6), fill = origen)) + 
	ggplot2::geom_bar(stat = 'identity') + ggplot2::coord_polar() + 
	ggplot2::scale_fill_manual(values = scales::alpha(color, 0.6), name = 'Wind speed (kts)') + ggplot2::theme_void() 
	grid::grid.draw(mapa)
	cuadro <- grid::viewport(x = (-d$longitude[1]-min(range(-datos$longitude)) ) /(range(datos$longitude)[2] - range(datos$longitude)[1])  ,
	y = (d$latitude[1] - min(range(datos$latitude)))/ ( range(datos$latitude)[2] -range(datos$latitude)[1]) , 
	just = c("center", "center"),
	width = 0.5, height = 0.5,
	xscale = range(-datos$longitude), yscale = range(datos$latitude)) 
	grid::pushViewport(cuadro)
	grid::grid.draw(ggplot2::ggplotGrob(dona))
	grid::popViewport()
}
geom(storm_observation, datos )










library(ggplot2)
hurricane <- ggproto('hurricane', Geom,
				required_aes = c("x", "y", 'r_ne', 'r_se', 'r_nw', 'r_sw'), 
				default_aes = aes(),
				draw_key = draw_key_point,
				draw_panel = function(data, panel_scales, coord)
					{
						## Transform the data first
						d <- data %>% tidyr::gather(key = tipo, value = power, r_ne, r_nw, r_se, r_sw)
						#print(d)
						#coords <- coord$transform(d, panel_scales)
						coords <- d
						coords$origen <- c('34', '50', '64')
						coords$color <- c('red', 'orange', 'yellow')
						coords$alpha <- 0.6
						print(coords)
						
						dona <- ggplot2::ggplot(coords, ggplot2::aes(x = factor(tipo), y =  power, alpha = alpha, fill = origen)) + 
						   		ggplot2::geom_bar(stat = 'identity') + ggplot2::coord_polar() + 
							   	ggplot2::scale_fill_manual(values = scales::alpha(color, 0.6), name = 'Wind speed (kts)') + ggplot2::theme_void() 
					    cuadro <- grid::viewport(x = coords$x[1] ,
												 y = coords$y[1] , 
												 just = c("center", "center"),
												 width = 0.5, height = 0.5) 
					    #print(dona)
						#grid::pushViewport(cuadro)
						grid::grid.draw(ggplot2::ggplotGrob(dona))
						#grid::popViewport()
})
geom_hurricane <- function(mapping = NULL, data = NULL, stat = "identity",
							 position = "identity", na.rm = FALSE, 
							 show.legend = NA, inherit.aes = TRUE, ...) {
	ggplot2::layer(
		geom = hurricane, mapping = mapping,  
		data = data, stat = stat, position = position, 
		show.legend = show.legend, inherit.aes = inherit.aes,
		params = list(na.rm = na.rm, ...)
	)
}
ggplot(data = storm_observation, aes(x = longitude, y = latitude, r_ne = ne,
									 r_se = se , r_nw = nw, r_sw = sw)) + geom_hurricane()



library(ggmap)
mapa <- get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%
	ggmap(extent = "device")
mapa + geom_hurricane(data = storm_observation, aes(x = longitude, y = latitude, r_ne = ne,
												   r_se = se , r_nw = nw, r_sw = sw))

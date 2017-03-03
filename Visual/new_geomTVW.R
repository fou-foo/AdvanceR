library(readr)
library(data.table)
library(dtplyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(geosphere)
library(ggmap)
library(gridExtra)

#' should be in my profile
`%notin%` <- function(x,y) !(x %in% y)

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

ext_tracks <- readr::read_fwf("ebtrk_atlc_1988_2015.txt", fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                              na = "-99")

ext_tracks = as.data.table(ext_tracks)
ext_tracks[ , storm_id := paste(storm_name,year,sep="-")]
ext_tracks[, date := as.POSIXct(sprintf("%s-%s-%s",year,month,day)) + as.ITime(sprintf("%s:00",hour),format="%H")]

#' base subset not data table's
ext_tracks = subset(ext_tracks, select=-c(storm_name, max_wind,min_pressure,rad_max_wind,eye_diameter, month, 
                                    day, year, hour, pressure_1,pressure_2,storm_type, distance_to_land,final))
#' this data is in Atlantic Basis (west longitude)
ext_tracks[, longitude := -longitude]

#' something close to land
#one_obs = ext_tracks[ storm_id == "KATRINA-2005" & date == "2005-08-29 12:00:00", ] 
one_obs = ext_tracks[ storm_id == "IKE-2008" & date == "2008-09-13 06:00:00",]

#' to long format, recast to tbl_dt so I can keep using data table semantics
one_obs <- dtplyr::tbl_dt(one_obs %>%  tidyr::gather(key=key, value=radius, -c(storm_id,latitude,longitude,date)))
one_obs[ , wind_speed := as.integer(substr(key,8,9))]
one_obs[ , key := substr(key,11,12)]
one_obs <- one_obs %>% tidyr::spread(key, radius )

#' Helper function which translates radii from a center point into lat/longs of points
#' which will form a polygon
#' 
#' @param dt a data table, or cast to such, containing storm information for one observation
#' 
#' @return newd a data frame - required by coord$transform() - containing lat/long points to 
#'         plot along with various aesthetics: fill, color, etc.
#'        
#' @note x is longitude, y is latitude
#'        
#' @note because draw_group operates on one group at a time, wind_speed in this
#'       context, all variables in the data frame except x and y will be constant over the points generated.  
#'       For example, for a given call of this function color will be constant.
#'       
convert_data_to_plot<-function(dt)
{
    dt = as.data.table(dt)
    
    if( dim(dt)[1] > 1 ) stop("dt should only be a 1 row data table")   
    
    nms = names(dt)
    nms = nms[nms %notin% c("x","y", "rne","rse","rsw","rnw","scale_radii")]
    
    #only 1 row, but make sure
    x = dt[1,x]
    y = dt[1,y]
    scale_radii = dt[1, scale_radii]
    
    smult = 1852*scale_radii #1852: meters in naut mile
    
    ne = as.data.table(geosphere::destPoint(p=c(x,y),0:90,dt[1,rne]*smult))
    se = as.data.table(geosphere::destPoint(p=c(x,y),91:180,dt[1,rse]*smult))
    sw = as.data.table(geosphere::destPoint(p=c(x,y),181:270,dt[1,rsw]*smult))
    nw = as.data.table(geosphere::destPoint(p=c(x,y),271:360,dt[1,rnw]*smult))
    newd = rbind(rbind(ne,se),rbind(sw,nw))
    
    setnames(newd, old=c("lon","lat"), new=c("x","y"))
    
    for (name in nms) newd[[name]] = dt[[name]]
    
    as.data.frame(newd)
}

#' Code defining a new geom, mostly boiler plate, except for function
#' draw_group() where the real work is done.
#' 
#' @param data a dataframe containing hurricane data for one observation of one hurricane
#'    in par

#' @note draw_group processes the data one group (by wind_speed in this case) at a time.
#'     See: http://ggplot2.tidyverse.org/articles/extending-ggplot2.html for more details.
#'     
#' @note storm observation data converted to form useful for plotting via helper function
#'     convert_data_to_plot
#'     
#' @return a 'hurricane' polygon for use with a map plot    
#' 
GeomHurricane <- ggproto("GeomHurricane", Geom,
        required_aes = c("x", "y","colour", "fill", "rne","rse","rsw","rnw"),
        default_aes = aes(shape = 1, alpha = 0.8, scale_radii = 1),
        draw_key = draw_key_polygon,
        
        draw_group = function(data, panel_scales, coord) 
        {
            newd = convert_data_to_plot(data)
            #newd must be a data frame, in particular it can't be a data table
            #even though a data table ISA data frame
            coords <- coord$transform(newd, panel_scales)
            grid::polygonGrob(x=coords$x, y = coords$y, 
                              gp = grid::gpar(col = coords$colour, fill=coords$fill, alpha=coords$alpha))
        }
)

#' Function which puts a GeomHurricane layer on an existing plot (map).
#' 
#' @param data a data table/frame containing information on one storm observation
#' 
#' @return a ggplot layer to be used within another plot (map)
#' 
#' @examples
#'  \dontrun{
#' geom_hurricane(data = one_obs, aes(x = longitude, y = latitude, colour=wind_speed, fill=wind_speed,
#' rne = ne, rse = se, rnw = nw, rsw = sw, scale_radii = 1))
#' }
#' 
#' @note the mappings from storm observation data to geom variables occur when calling the function,
#' except for scale_radii, which can be set in the call (is not part of data).  
#' Scale_radii is a multiplier applied to the wind radii to make them larger or smaller than actual.   
#' 
geom_hurricane <- function(mapping = NULL, data = NULL, stat = "identity",
                             position = "identity", na.rm = FALSE,
                             show.legend = T, inherit.aes = T, ...) {
    ggplot2::layer(
        geom = GeomHurricane, mapping = mapping,
        data = data, stat = stat, position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}

#Hack because apparently integer isn't discrete enough
one_obs[ , wind_speed := as.factor(wind_speed)]

ledg = "Wind speed (kts)"
clrs = c("blue", "green", "yellow")

p1 = get_map("Louisiana", zoom = 6, maptype = "toner-background") %>% ggmap(extent = "device") +
     geom_hurricane(data = one_obs, aes(x = longitude, y = latitude, colour=wind_speed, fill=wind_speed,
                                        rne = ne, rse = se, rnw = nw, rsw = sw, scale_radii = 1)) +
     scale_colour_manual(name = ledg, values = clrs ) + scale_fill_manual(name = ledg, values = clrs)

get_map("Louisiana", zoom = 6, maptype = "toner-background") %>% ggmap(extent = "device") +
     geom_hurricane(data = one_obs, aes(x = longitude, y = latitude, colour=wind_speed, fill=wind_speed,
                                       rne = ne, rse = se, rnw = nw, rsw = sw, scale_radii = 0.5)) +
    scale_colour_manual(name = ledg, values = clrs ) + scale_fill_manual(name = ledg, values = clrs)

grid.arrange(p1, p2, ncol=2)


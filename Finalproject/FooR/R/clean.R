


#signif <- readr::read_delim(file ='C:\\Users\\fou-f\\Desktop\\AdvanceR\\Finalproject\\signif.txt', delim = '\t',guess_max = 5420)
#save(signif, file ='C:\\Users\\fou-f\\Desktop\\AdvanceR\\Finalproject\\FooR\\data\\signif.RData' )
# Load data
#data <- readr::read_delim(file ='data\\signif.txt', delim = '\t',guess_max = 5420)
#devtools::use_data(signif)
#data <- devtools::use_data(signif)


#' @title fecha
#'
#' @description function to standardize the variable 'YEAR' standardizes the years so that they all
#' have a length of four characters
#'
#' @param n the number of the record in the data set (numeric)
#'
#' @return a register with standardize
#'
#' @examples
#' \dontrun{dt.pretty_years <- mapply(fecha, 1:dim(data)[1])}
#'
#' @export

fecha <- function(n)
{
  if(nchar(data[n, 'YEAR']) == 5 & as.numeric(data[n, 'YEAR']) < 0)
    return(as.character(data[n, 'YEAR']))
  if(nchar(data[n, 'YEAR']) == 4 & as.numeric(data[n, 'YEAR']) > 0)
     return(as.character(data[n, 'YEAR']))
  if(as.numeric(data[n, 'YEAR'])>0 )
    {
      if(nchar(as.character(data[n, 'YEAR'])) == 3)
        return(as.character(paste0('0', data[n, 'YEAR'])))
      if(nchar(as.character(data[n, 'YEAR'])) == 2)
        return(as.character(paste0('00', data[n, 'YEAR'])))
      if(nchar(as.character(data[n, 'YEAR'])) == 1)
        return(as.character(paste0('000', data[n, 'YEAR'])))
      if(as.character(nchar(data[n, 'YEAR'])) == 0)
        return(as.character(paste0(0000, data[n, 'YEAR'])))
  } else  {
    return(as.character(data[n, 'YEAR']))
  }
}


#' @title eq_clean_datafunction to convert the variable 'YEAR' to a lubridate
#'  package format ymd
#'
#' @description function to convert the variable 'YEAR' to a lubridate
#'  package format ymd,convert the variable 'YEAR' of character type to
#' a manageable date with the lubridate package
#' @param n the number of the record in the data set (numeric)
#'
#' @return data output from function 'fecha' (dataset)
#' @examples
#' \dontrun{NOAA <- eq_clean_data(data)}
#' @importFrom lubridate ymd
#' @export

eq_clean_data <- function(data)
{
  data[is.na(data[, 'YEAR']),'YEAR'] <- 'NA'
  data[is.na(data[, 'MONTH']),'MONTH'] <- 'NA'
  data[is.na(data[, 'DAY']),'DAY'] <- 'NA'
  data$YEAR2 <- 0
  data$YEAR2 <- mapply(fecha, 1:dim(data)[1])
  data$Date2<- paste(data$YEAR2, data$MONTH, data$DAY, sep ='.')
  data$Date <- lubridate::ymd(data$Date2)
  NOAA  <- data[-which(is.na(data$Date )),]
  NOAA$LATITUDE <- as.numeric(NOAA$LATITUDE)
  NOAA$LONGITUDE<- as.numeric(NOAA$LONGITUDE)
  NOAA$DEATHS <- as.numeric(NOAA$DEATHS)
  NOAA$YEAR <- as.numeric(NOAA$YEAR)
  NOAA$EQ_MAG_ML <- as.numeric(NOAA$EQ_MAG_ML)
  NOAA

}


#' @title eq_location_clean
#'
#' @description After remove the name of the country in the 'LOCATION_NAME'
#' variable and the result transforms it into uppercase and lowercase
#' (a somewhat absurd requirement).
#'
#' @param NOAA dataset with 'LOCATION_NAME' variable (dataset)
#'
#' @return NOAA with nice 'LOCATION_NAME' (dataset)
#'
#' @examples
#' \dontrun{NOAA <- eq_location_clean(NOAA)}
#'
#' @export
eq_location_clean<- function(NOAA)
{
  NOAA$LOCATION_NAME <- tolower(gsub(".*:","",NOAA$LOCATION_NAME))
  NOAA
}

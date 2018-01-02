### Clean and load
library(readr)
library(lubridate)
library(ggplot2)
data <- read_delim(file ='C:\\Users\\fou-f\\Desktop\\AdvanceR\\Finalproject\\signif.txt', delim = '\t',guess_max = 5420) 
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
    return(as.character(data[n, 'YEAR']))
  }
}
eq_clean_data <- function(data)
{
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
  NOAA$DEATHS <- as.numeric(NOAA$DEATHS)
  NOAA$YEAR <- as.numeric(NOAA$YEAR)
  NOAA$EQ_MAG_ML <- as.numeric(NOAA$EQ_MAG_ML)
  NOAA
  
}
NOAA <- eq_clean_data(data)
eq_location_clean<- function(NOAA)
{
  NOAA$LOCATION_NAME <- tolower(gsub(".*:","",NOAA$LOCATION_NAME))
  NOAA
}
NOAA <- eq_location_clean(NOAA)

NOAA %>% eq_clean_data() %>%
  filter(COUNTRY %in% c("USA", "MEXICO", "JAPAN"), YEAR > 2000, DEATHS>0) %>%
  ggplot(aes(x = Date,
             y = COUNTRY,
             color = DEATHS,
             size = as.numeric(EQ_PRIMARY)
  )) +
  geom_timeline() +
  geom_timeline_label(aes(label = LOCATION_NAME)) +
  theme_timeline() +
  labs(size = "Richter scale value", color = "# deaths")

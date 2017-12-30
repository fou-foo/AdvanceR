### Clean and load
library(readr)
library(lubridate)
data <- read_delim(file ='C:\\Users\\fou-f\\Desktop\\AdvanceR\\Finalproject\\results', delim = '\t') 
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
    if(nchar(data[n, 'YEAR'])==4)
      return(as.character(paste0('-0', substr(data[n, 'YEAR'], 2,4))))
    if(nchar(data[n, 'YEAR'])==3)
      return(as.character(paste0('-00', substr(data[n, 'YEAR'], 2,3))))
    if(nchar(data[n, 'YEAR'])==1)
      return(as.character(paste0('-000', substr(data[n, 'YEAR'], 2,4))))
  }
}
data[is.na(data[, 'YEAR']),'YEAR'] <- 'NA'
data[is.na(data[, 'MONTH']),'MONTH'] <- 'NA'
data[is.na(data[, 'DAY']),'DAY'] <- 'NA'
data$YEAR2 <- 0
for (i in 1:dim(data)[1])
  data$YEAR2[i] <- fecha(i)
mapply(fecha, 1:dim(data)[1])
data$Date2<- (paste(data$YEAR, data$MONTH, data$DAY, sep ='-'))
data$Date <- ymd(data$Date)



df <- data.frame(ID=letters[1:5],
                 Datum=c("01.01.1990", "NA", "11.01.1990", "NA", "01.02.1990")) #NAs are quoted
df_copy <- df

df$Datum <- dmy(df$Datum)

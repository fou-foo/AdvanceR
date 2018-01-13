## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set( 
  collapse = TRUE,
  comment = "#>" 
)

## ----libraries-----------------------------------------------------------
library(FooR)
library(xtable)
library(pander)
library(dplyr)
library(ggplot2)
library(readr)

## ----print1, echo = FALSE------------------------------------------------
NOAA <- readr::read_tsv(system.file("extdata/signif.txt", package = "FooR"))

## ----tres, fig.width = 7, fig.height = 7---------------------------------
NOAA <- as.data.frame(NOAA)
NOAA <- eq_clean_data(NOAA)
NOAA <- eq_location_clean(NOAA)

## ----doctor,  fig.width = 16, fig.height = 17----------------------------
NOAA %>%
filter( DEATHS>0) %>%
     ggplot(aes(x = Date,
                y = COUNTRY,
                color = DEATHS,
                size = as.numeric(EQ_PRIMARY)
     )) +
     geom_timeline() +
     #geom_timeline_label(aes(label = LOCATION_NAME)) +
     theme_timeline() +
     labs(size = "Richter scale value", color = "# deaths",
          title = "Figure 1 : Doctor Who's view")

## ----human, fig.width = 12, fig.height = 3-------------------------------
NOAA %>%
filter(COUNTRY %in% c("USA"), 2020 > YEAR,  YEAR > 2000, DEATHS>0) %>%
     ggplot(aes(x = Date,
                y = COUNTRY,
                color = DEATHS,
                size = as.numeric(EQ_PRIMARY)
     )) +
     geom_timeline() +
     geom_timeline_label(aes(label = LOCATION_NAME)) +
     theme_timeline() +
     labs(size = "Richter scale value", color = "# deaths",
     title = "Figure 2: Human vision")

## ----human2, fig.width = 12, fig.height = 6------------------------------
NOAA %>%
filter(COUNTRY %in% c("USA", "MEXICO", "JAPAN"), 2020 > YEAR,  YEAR > 2000, DEATHS>0) %>%
     ggplot(aes(x = Date,
                y = COUNTRY,
                color = DEATHS,
                size = as.numeric(EQ_PRIMARY)
     )) +
     geom_timeline() +
     geom_timeline_label(aes(label = LOCATION_NAME)) +
     theme_timeline() +
     labs(size = "Richter scale value", color = "# deaths",
     title = "Figure 3: Human vision")

## ----mapa_simple, fig.width = 8, fig.height = 6--------------------------
NOAA %>% 
   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(Date) >= 2000) %>% 
  eq_map(annot_col = "DATE")

## ----mapa_con_marcas, fig.width = 8, fig.height = 6----------------------
NOAA %>%
 dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(Date) >= 2000) %>% 
  dplyr::mutate(popup_text = eq_create_label(.)) %>% 
  eq_map(annot_col = "popup_text")


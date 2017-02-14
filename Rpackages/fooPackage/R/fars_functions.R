#' @title fars_read
#'
#' @description
#' This function reads a path from a csv file and reads the file by returning
#' the dataset as a \code{tbl_df} object of the \code{dplyr} package. In case
#' the path does not exist the function throws a message
#'
#' @param filename A character string giving the path of the file of the subject
#'  to be read
#'
#' @return This function returns a dataset of class \code{tbl_df} (see \code{dplyr}
#' package) which is locate in \code{filename} file.
#'
#' @examples
#' tf <- tempfile()
#' fars_read(tf)
#'
#' @import readr
#' @import dplyr
#'
#' @seealso tbl_df
#'
#' @export
#'
fars_read <- function(filename)
	{
		if( !file.exists(filename))	stop("file '", filename, "' does not exist")
    	data <- suppressMessages(
    		{
    			readr::read_csv(filename, progress = FALSE)
        	}
    			)
        dplyr::tbl_df(data)
	}


#' @title make_filename
#'
#' @description
#'
#' This function builds a string with the following structure:
#'
#' \code{accident_\strong{year}.csv.bz2}
#'
#' @param year A character string or numeric vector which contains the year of
#' your interest.
#'
#' @return This function returns a character vector, a.k.a \code{string} in other
#' programming languages, with the structure mentioned in the function description.
#'
#' @examples
#' make_filename('2013')
#' make_filename(2014)
#'
#' @export
#'
make_filename <- function(year)
	{
		year <- as.integer(year)

        paste0(getwd(),sprintf("accident_%d.csv", year))
	}


#' @title fars_read_years
#'
#' @description
#'
#' This function reads a list of files (each one with the structure required by
#' the function \code{fras_read}) and return the subset of columns 'MONTH' and
#' 'year' where 'year' is tthe year in \code{years} param. The function throws a error
#' if any number in \code{years} is not finded.
#'
#' @param years A list of character string or numeric vector which contains the years
#'  of your interest.
#'
#' @return This function returns a list of datasets of class \code{\link{tbl_df}}
#' with columns 'MONTH' and 'year'.
#'
#' @examples
#' fars_read_years(c('2013', '2014', '2015'))
#' fars_read_years(2013:2015)
#'
#' @import  dplyr
#'
#' @seealso tbl_df
#' @export
#'
fars_read_years <- function(years)
	{
		lapply(years, function(year)
				{
					file <- make_filename(year)
					tryCatch(
						{
                        	dat <- fars_read(file)
                        	dplyr::mutate(dat, year = year) %>%
                            dplyr::select(MONTH, year)
                		},
						error = function(e)
						{
							warning("invalid year: ", year)
                        	return(NULL)
                		})
        		})
	}

#' @title fars_summarize_years
#'
#' @description
#'
#' This function summarise the counts of observations by year and month
#' from the files contained in the list \code{years} param.
#'
#' @param years A list of character string or numeric vector which contains the years
#'  of your interest.
#'
#' @return This function returns a dataset of class \code{\link{tbl_df}}
#' that summarises the contents of files of years within the list \code{years} by
#' year and month and which the columns are the years and the month.
#'
#' @examples
#' fars_summarize_years(c('2013', '2014', '2015'))
#' fars_summarize_years(2013:2015)
#'
#' @import dplyr
#' @import tidyr
#'
#' @seealso \code{\link{tbl_df}}
#' @export
#'
fars_summarize_years <- function(years)
	{
		dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
	}


#' @title fars_map_state
#'
#' @description
#'
#' This function plots the places were did it happen the incidentes recorded in
#' the year \code{year} in the state \code{state.num}. The function verify that
#' state is correct and that the state has incidents reported in the year.
#'
#'
#' @param year A character string or numeric vector which contains the year of
#' your interest to plot
#'
#' @param state.num A singleton numeric vector for which is interesting to plot.
#'
#' @return \code{NULL}
#'
#' @examples
#' fars_map_state(1, 2013)
#' fars_map_state(4, 2014)
#'
#' @import  dplyr
#' @import graphics
#' @import maps
#'
#'
#' @export
fars_map_state <- function(state.num, year)
	{
		filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)
        if(!(state.num %in% unique(data$STATE))) stop("invalid STATE number: ", state.num)

        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L)
        	{
        		message("no accidents to plot")
                return(invisible(NULL))
        	}
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
	}

library(dplyr)
library(reshape)

##Definition of class _LongitudinalData_ y _LD_
## Class constructor
make_LD <- function(x)
{
	structure(list(subject = unique(x$id), 
						   visit = unique(x$visit),
						   room = unique(x$room), 
						   datos = x),
			  class = "LongitudinalData")
	
}

make_LL <- function(x)
{
	structure(list(subject = unique(x$id), 
						   visit = unique(x$visit),
						   room = unique(x$room), 
						   datos = x),
			  class = "LD")
	
}


## Implementation of the print method for the LongitudinalData class
print.LongitudinalData <- function(x)
		  	{
		  		if (length(x$subject) > 1)
		  		{
		  			paste0("Longitudinal dataset with ", length(x$subject), " subjects")
		  		}
		  		else 
		  		{
		  			paste0('Subject ID:', x$subject)
		  		}
		  	}
		  

print.LD <- function(x)
		  	{
		  		print(paste0("ID: ", x$subject))
		  		print(paste0("Visit: ", x$visit))
		  		print(paste0("Room: ", x$room))
		  	}




## Definition to subset Longitudinal data by _subject_ slot 
subject <- function(x, idParticular) UseMethod("subject")
subject.LongitudinalData <- function(x, idParticular)
		   	{
		   		if( idParticular %in% unique(x$subject) )
		   		{
		   			d <- subset(x$datos, id %in% idParticular)
		   			return( make_LD (d))
		   		}
		   		else 
		   		{
		   			return(NULL)
		   		}
		   	}


## Definition to subset Longitudinal data by _visit_ slot 
visit <- function(x, lugar) UseMethod("visit")
visit.LongitudinalData <- function(x, lugar)
		   	{
		   		if( lugar %in% unique(x$visit) )
		   		{
		   			d <- subset(x$datos, visit %in% lugar)
		   			return( make_LD (d))
		   		}
		   		else 
		   		{
		   			return(NULL)
		   		}
		   }



## Definition to subset Longitudinal data by _room_ slot 
room <- function(x, cuarto) UseMethod("room")
room.LongitudinalData <- 
		   function(x, cuarto)
		   {
		   	if( cuarto %in% unique(x$room) )
		   	{
		   		d <- subset(x$datos, room %in% cuarto)
		   		return(
		   			make_LL(d))
		   		
		   	}
		   	else 
		   	{
		   		return(NULL)
		   	}
		   }


## Definition to _summary_ for the class _LongitudinalData_
summary.LongitudinalData <- function(x)
{
	print(paste0("ID: " , x$subject))
	ax <- x$datos %>% group_by(visit, room) %>% summarise(m = mean(value)) %>%
		cast( visit ~ room)
	print(ax)
}

summary.LD <- function(x)
{
	summary(x$datos["value"])
}

library(dplyr)
library(reshape)

##Definition of class _LongitudinalData_ y _LD_
setClass("LongitudinalData",
		 slots = list(subject = "numeric", 
		 			 visit = "numeric",
		 			 room = "numeric", 
		 			datos = "data.frame"))
setClass("LD",
		 contains = "LongitudinalData")

## Class constructor
make_LD <- function(x)
{
	return( new("LongitudinalData", subject = unique(x$id), 
				visit = unique(x$visit),
				room = unique(x$room),
				datos = x
				))
}

## Implementation of the print method for the LongitudinalData class
setGeneric("print")
setMethod("print",
		  signature = "LongitudinalData",
		  function(x)
		  	{
		  		if (length(x@subject) > 1)
		  		{
		  			paste0("Longitudinal dataset with ", length(x@subject), " subjects")
		  		}
		  		else 
		  		{
		  			paste0('Subject ID:', x@subject)
		  		}
		  }
		  	
		  )

setGeneric("print")
setMethod("print",
		  signature = "LD",
		  function(x)
		  {
		  	print(paste0("ID: ", x@subject))
		  	print(paste0("Visit: ", x@visit))
		  	print(paste0("Room: ", x@room))
		  }
)




## Definition to subset Longitudinal data by _subject_ slot 
setGeneric("subject", 
		   function(x, idParticular)
		   {
		   	if( idParticular %in% unique(x@subject) )
		   	{
		   		d <- subset(x@datos, id %in% idParticular)
		   		return( make_LD (d))
		   	}
		   	else 
		   	{
		   		return(NULL)
		   	}
		   }
		)

## Definition to subset Longitudinal data by _visit_ slot 
setGeneric("visit", 
		   function(x, lugar)
		   {
		   	if( lugar %in% unique(x@visit) )
		   	{
		   		d <- subset(x@datos, visit %in% lugar)
		   		return( make_LD (d))
		   	}
		   	else 
		   	{
		   		return(NULL)
		   	}
		   }
)


## Definition to subset Longitudinal data by _room_ slot 
setGeneric("room", 
		   function(x, cuarto)
		   {
		   	if( cuarto %in% unique(x@room) )
		   	{
		   		d <- subset(x@datos, room %in% cuarto)
		   		return(
		   			new("LD", subject = unique(d$id), 
		   				visit = unique(d$visit),
		   				room = unique(d$room),
		   				datos = d
		   			)
		   		)
		   	}
		   	else 
		   	{
		   		return(NULL)
		   	}
		   }
)

## Definition to _summary_ for the class _LongitudinalData_
setGeneric("summary", 
		  function(x)
		  {
		  	library(dplyr)
		  	library(reshape)
		  	print(paste0("ID: " , x@subject))
		  	ax <- x@datos %>% group_by(visit, room) %>% summarise(m = mean(value)) %>%
		  		cast( visit ~ room)
		  	print(ax)
		  }
)

setGeneric("summary", 
		   function(x)
		   {
		   	summary(x@datos["value"])
		   }
)

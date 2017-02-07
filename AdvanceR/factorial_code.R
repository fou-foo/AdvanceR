Factorial_loop <- function(x)
{
	facto <- 1
	for( i in 1:x)
	{
		facto <- facto * i
	}
	return(facto)	
}


Factorial_reduce <- function(x)
{
	lista <- 1:x
	lista <- as.integer(lista)
	#return(cumprod(lista)[length(lista)] )
	return(Reduce(`*`, lista))
}


Factorial_func <- function(x)
{
	ifelse(x == 1, return(1), return(x*Factorial_func(x-1)))
}


facto_tbl <- c(1, rep( NA, 1000))
Factorial_mem <- function(n)
{
	stopifnot(n > 0)
	if(!is.na(facto_tbl[n]))
	{
		facto_tbl[n]
	} 
	else 
	{
		facto_tbl[n - 1] <<- Factorial_mem(n - 1)
		n*facto_tbl[n-1] 
	}
}

library(microbenchmark)
compareTimes <- microbenchmark(mapply(Factorial_loop, 1:170),
			   mapply(Factorial_func, 1:170),
			   mapply(Factorial_reduce, 1:170),
			   map_dbl(1:170, Factorial_mem))
#write.table(compareTimes, file = "/home/fou/Desktop/Online/RCoursera/MasteringSoftwareDevelopmentR/AdvanceR/AdvanceR/factorial_output.txt")

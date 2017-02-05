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
	return(Reduce("*", lista))
}


Factorial_func <- function(x)
{
	ifelse (x == 1, return(1), return(x*Factorial_func(x-1)))
}

Factorial_mem

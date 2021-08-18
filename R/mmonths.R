NULL
#' months REPLACEMANT
#' 
#' @param  x an object. See \code{\link{months}}
#' @param ... arguments
#' 
#' @importFrom utils sessionInfo
#' 
#' @export 
#' 
#' 
#' 

months_f <- function(x,...) {
###	print('cisao')
	ss <- sessionInfo()
	spkgs <-  c(ss$basePkgs,names(ss$otherPkgs),names(ss$loadedOnly))
	####spkgs__ <<- spkgs
	if ("lubridate" %in% spkgs) {
		
		if (is.numeric(x)) {
			
			x <- as.Date("1990-01-01")+x-1
			
		}
		out <- months(x=x,...)
		##stop("lubridate loaded!")
	} else {
		out <- months(x=x,...)
	}
	
	return(out)
	
}




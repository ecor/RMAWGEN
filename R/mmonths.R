NULL
#' months REPLACEMANT
#' 
#' @param ... arguments
#' 
#' @importFrom utils sessionInfo
#' 
#' @export 
#' 
#' 
#' 

months_f <- function(...) {
	
	ss <- sessionInfo()
	spkgs <-  c(ss$basePkgs,names(ss$otherPkgs))
	if ("lubridaate" %in% spkgs) {
		
		stop("lubridate loaded!")
	} else {
		out <- months(...)
	}
	
	return(out)
	
}




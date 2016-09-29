NULL


#' 
#' 	counts NAs in each row of \code{data}
#' 
#' 
#' 
#' 
#'  
#'   
#' @param data a data input matrix
#'  
#'  @export 
#'
#'       
#' @return  the vector with numbers of NA values for each \code{data} column


countNAs <-
function (data){
	
	out <- array(0,dim=ncol(data))
	
	for (i in 1:ncol(data)) {
		
		vect <- data[,i]
		out[i] <- length(vect[is.na(vect)])
		
	}
	names(out) <- names(data)
	return(out)
}


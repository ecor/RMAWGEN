
NULL


#' 
#' Forecasts the expected value of a VAR realization given the prievious one 
#' 
#' @param var A VAR model represented by a \code{varest} object as returned by \code{\link{getVARmodel}} or \code{\link[vars]{VAR}}
#' @param xprev previous status of the random variable
#' @param exogen vector containing the values of the "exogen" variables (predictor) for the generation
#' @param cvar matrix of the VAR coefficients (one row per equation), as returned by \code{\link{coefmatrixEV}}. If \code{NULL} (default), it is computed from \code{var}. Passing it avoids recomputing \code{coef(var)} (and then \code{summary.lm}) at each time step.
#' 
#' @seealso \code{\link{forecastResidual}}
#' 
#'    
#'  @export
#'
#'      
#' 
#' 
#' @return  a vector of values





forecastEV <-
function(var,xprev=NULL,exogen=NULL,cvar=NULL) {

	
#	class(var) <- "varest"

	if (is.null(xprev)) xprev <- rnorm(var$p*var$K)

	if (!is.null(exogen)) xprev <- c(xprev,exogen)

	## cvar: coefficient matrix (K x number of regressors), computed once by the caller.
	## Recomputing coef(var) at each step calls summary.lm() on all K equations.
	if (is.null(cvar)) cvar <- coefmatrixEV(var)
	out <- as.vector(cvar %*% xprev)
	
	
	return(out)
	
}



#' Coefficient matrix of a VAR model
#'
#' Returns the coefficients of the VAR equations (column \code{"Estimate"} of \code{coef(var)}), one row per equation. It is computed once before the generation loop in \code{\link{newVARmultieventRealization}}.
#'
#' @param var a \code{varest} object
#' @return a matrix with one row per endogenous variable and one column per regressor
#' @export
#' @seealso \code{\link{forecastEV}}
coefmatrixEV <- function(var) {
	cv <- coef(var)
	do.call(rbind, lapply(cv, function(x) x[, "Estimate"]))
}

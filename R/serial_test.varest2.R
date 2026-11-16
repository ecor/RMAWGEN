NULL
#'
#'\code{serial.test} function for \code{varest2} object
#'
#' @param object a \code{varest2} object
#' @param ...   passed arguments
#'
#'
#' @export
#' @seealso \code{\link[vars]{serial.test}}
#'
#' @examples
#'
#' set.seed(122)
#' NSTEP <- 1000
#' x <- rnorm(NSTEP)
#' y <- x+rnorm(NSTEP)
#' z <- c(rnorm(1),y[-1]+rnorm(NSTEP-1))
#' df <- data.frame(x=x,y=y,z=z)
#'
#' ## VAR model ('varest2' object) and VAR model with GPCA ('GPCAvarest2' object)
#' var <- getVARmodel(data=df,suffix=NULL,p=3)
#' gpcavar <- getVARmodel(data=df,suffix=NULL,p=3,n_GPCA_iteration=5,
#'                        n_GPCA_iteration_residuals=5)
#'
#' ## Portmanteau test (default) on the residuals
#' serial_test(var)
#' serial_test(gpcavar)
#'
#' ## further arguments are passed to 'vars::serial.test'
#' st <- serial_test(gpcavar,lags.pt=20,type="PT.adjusted")
#' st$serial$p.value
#'
#' ## Breusch-Godfrey LM test
#' serial_test(var,lags.bg=5,type="BG")
#'
#' ## Second example: 'GPCAvarest2' object for daily maximum and minimum temperature
#' ## (handled as daily mean temperature and daily thermal range, i.e. option=2)
#' ## of some stations of the 'trentino' dataset
#' data(trentino)
#' year_min <- 1961
#' year_max <- 1990
#' station <- c("T0001","T0010","T0099")
#' param <- setComprehensiveTemperatureGeneratorParameters(station=station,
#'            Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN,
#'            year_min=year_min,year_max=year_max,
#'            sample="monthly",option=2,verbose=FALSE)
#' gpcavar_t <- getVARmodel(data=param[['data_for_var']],suffix=c("_T1","_T2"),p=1,
#'                          n_GPCA_iteration=5,n_GPCA_iteration_residuals=5)
#'
#' serial_test(gpcavar_t)
#' st_t <- serial_test(gpcavar_t,lags.pt=20,type="PT.adjusted")
#' st_t$serial$p.value
#'

serial_test <- function(object,...) {

	temp <- object@VAR
	if (inherits(object,"GPCAvarest2")) {
		if (length(object@GPCA_residuals)>0) {
			temp <- VAR_mod(object@GPCA_residuals$final_results,p=0)
		}
	}
	### class(temp) <- "varest"
	return(serial.test(temp, ...))

}



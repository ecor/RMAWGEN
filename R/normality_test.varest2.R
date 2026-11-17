NULL
#'
#'\code{normality.test} method for \code{varest2} object
#'
#' @param object a \code{varest2} object
#' @param ...   passed arguments
#'
#' @export
#'
#' @seealso \code{\link[vars]{normality.test}}
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
#' ## multivariate Jarque-Bera, skewness and kurtosis tests on the residuals
#' normality_test(var)
#' normality_test(gpcavar)
#'
#' ## p-values of the three multivariate tests
#' nt <- normality_test(gpcavar)
#' sapply(nt$jb.mul,function(t) {t$p.value})
#'
#' ## further arguments are passed to 'vars::normality.test',
#' ## e.g. univariate Jarque-Bera tests for each variable
#' nt_uni <- normality_test(var,multivariate.only=FALSE)
#' sapply(nt_uni$jb.uni,function(t) {t$p.value})
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
#' gpcavar_t <- getVARmodel(data=param[['data_for_var']],suffix=c("_T1","_T2"),p=5,
#'                          n_GPCA_iteration=5,n_GPCA_iteration_residuals=5)
#'
#' normality_test(gpcavar_t)
#' nt_t <- normality_test(gpcavar_t)
#' sapply(nt_t$jb.mul,function(t) {t$p.value})
#'

normality_test <- function(object,...) {

	temp <- object@VAR
	if (inherits(object,"GPCAvarest2")) {
		if (length(object@GPCA_residuals)>0) {
			temp <- VAR_mod(object@GPCA_residuals$final_results,p=0)
		}
	}
	### class(temp) <- "varest"
	return(normality.test(temp, ...))

}



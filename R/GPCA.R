NULL
#'
#' This function makes a Gaussianization procedure based on PCA iteration ( see \code{\link{GPCA_iteration}})
#'
#' @param x_prev previous set of the random variable \code{x}. If it is a \code{varest} object, the residuals are taken into account.
#' @param n number of reiterations
#' @param extremes  see \code{\link{normalizeGaussian_severalstations}}
#' @param nearPD logical. Default is \code{FALSE}. If \code{TRUE} covariance matrix is corrected through Nearest Positive Definite Matrix procedure, i.e. \code{\link[Matrix]{nearPD}}
#' @author Emanuele Cordano
#'
#' @export
#'
#' @return A  \code{\link{GPCA-class}} S3 object returned by \code{\link{GPCA_iteration}} at each iteration
#' 			and the final results of the G-PCA procedure (matrix \code{final_results})
#'
#' @seealso \code{\link{GPCA}},\code{\link{GPCA_iteration}},\code{\link{inv_GPCA_iteration}},\code{\link{inv_GPCA}},\code{\link{GPCA-class}} for 'GPCA' S3 class
#'
#'
#' @note This function re-iterates the equation (1) of "PCA Gaussianization for One-Class Remote Sensing Image" by V. Laparra et al., \url{https://www.uv.es/lapeva/papers/SPIE09_one_class.pdf},\url{https://www.uv.es/vista/vistavalencia/papers/SPIE_09_Gaussianization_presentation.pdf}
##### \url{http://dx.doi.org/doi/10.1117/12.834011}
#' @examples
#' library(RMAWGEN)
#' set.seed(1222)
#' nIterations <- 30
#' N <- 20
#' x <- rexp(N)
#' y <- x+rnorm(N)
#' df <- data.frame(x=x,y=y)
#'
#' GPCA <- GPCA(df,n=nIterations,extremes=TRUE)
#'
#' x <- rnorm(N)
#' y <- x+rnorm(N)
#' dfn <- data.frame(x=x,y=y)
#'
#' GPCAn <- GPCA(dfn,n=nIterations,extremes=TRUE)
#'
#'




GPCA <- function (x_prev,n=30,extremes=TRUE,nearPD=FALSE) {

	if (inherits(x_prev,'varest2')  | inherits(x_prev,'GPCAvarest2') | inherits(x_prev,'varest') ) {





		x_prev <- residuals(x_prev@VAR)

	}

	out <- list()

	if (n<1) {
		class(out) <- "GPCA"
		return(out)
	}
	for (i in 1:n) {



		DT <- GPCA_iteration(x_prev=x_prev,extremes=extremes,nearPD=nearPD)
		x_prev <- DT$x_next
		out[[i]] <- DT
	}

	iter_names <- sprintf("iteraton%03d",1:length(out))

	out[[length(out)+1]] <- normalizeGaussian_severalstations(x_prev,data=x_prev,extremes=extremes,inverse=FALSE)


	names(out) <- c(iter_names,"final_results")

	class(out) <- "GPCA"
	return(out)

}


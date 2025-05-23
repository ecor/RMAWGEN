NULL
#'
#' This function makes an iteration of PCA-Gaussianization process
#'
#' @param x_prev previous set of random variable \code{x}
#' @param extremes  see \code{\link{normalizeGaussian_severalstations}}
#' @param nearPD logical. Default is \code{FALSE}. If \code{TRUE} covariance matrix is corrected through Nearest Positive Definite Matrix procedure, i.e. \code{\link[Matrix]{nearPD}}
#' @author Emanuele Cordano
#'
#' @importFrom Matrix nearPD
#' @export
#' @return A \code{GPCA_iteration} S3 object which contains the following objects:
#'
#' \code{x_prev} Previous set of random variable, \code{x_prev} input variable
#'
#' \code{x_gauss_prev} Marginal Gaussianization of \code{x_prev} obtained through \code{\link{normalizeGaussian_severalstations}}
#'
#' \code{B_prev} rotation matrix (i. e. eigenvector matrix of the covariance matrix of  \code{x_gauss_prev}
#'
#' \code{x_next} results obtained by multiplying \code{B_prev} by  \code{x_gauss_prev} (see equation 1 of  the reference)
#'
#'
#' @note This function is based on equation (1) of "PCA Gaussianization for One-Class Remote Sensing Image" by V. Laparra et al.,  \url{https://www.uv.es/lapeva/papers/SPIE09_one_class.pdf} and  \url{https://ieeexplore.ieee.org/document/5413808/}



#' @seealso \code{\link{GPCA}},\code{\link{GPCA_iteration}},\code{\link{inv_GPCA_iteration}},\code{\link{inv_GPCA}}
#' @examples
#' library(RMAWGEN)
#' set.seed(1222)
#' N <- 20
#' x <- rexp(N)
#' y <- x+rnorm(N)
#' df <- data.frame(x=x,y=y)
#'
#' GPCA <- GPCA_iteration(df,extremes=TRUE)
#'
#' x <- rnorm(N)
#' y <- x+rnorm(N)
#' dfn <- data.frame(x=x,y=y)
#'
#' GPCAn <- GPCA_iteration(dfn,extremes=TRUE)
#'
#'


GPCA_iteration <- function (x_prev,extremes=TRUE,nearPD=FALSE) {


# The iterartion is formulated in eq. 1 of \url{www.uv.es/lapeva/papers/SPIE09_one_class.pdf}

	x_gauss_prev <- normalizeGaussian_severalstations(x=x_prev,data=x_prev,extremes=extremes,inverse=FALSE)

##	B_prev <- t(eigen(cor(x_gauss_prev,use="pairwise.complete.obs"))$vectors)



	if (!nearPD) {
	  B_prev <- t(eigen(cov(x_gauss_prev,use="pairwise.complete.obs"))$vectors) ###  t(eigen(cor(x_gauss_prev,use="pairwise.complete.obs"))$vectors)
  } else {
	  B_prev <- t(eigen(Matrix::nearPD(cov(x_gauss_prev,use="pairwise.complete.obs"),corr=FALSE,keepDiag=TRUE)$mat)$vectors) ## EC 20250225
  }
x_next <- B_prev %*% t(as.matrix(x_gauss_prev))


	out <- list(x_prev=x_prev,x_next=as.data.frame(t(x_next)),B_prev=B_prev,x_gauss=x_gauss_prev)
	class(out) <- "GPCAiteration"
	return(out)
}

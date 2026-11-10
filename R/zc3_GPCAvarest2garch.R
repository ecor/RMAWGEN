NULL
#' The class \code{GPCAvarest2garch}
#'
#' Extension of \code{\link{GPCAvarest2-class}} in which the (GPCA-Gaussianized) VAR residuals
#' are modelled with a diagonal GARCH(1,1) process after decorrelation
#' (orthogonal GARCH approach). Each decorrelated residual component \eqn{w_{j,t}} follows
#' \deqn{w_{j,t}=\sqrt{h_{j,t}}\,z_{j,t},\qquad h_{j,t}=\omega_j+\alpha_j w_{j,t-1}^2+\beta_j h_{j,t-1}}
#' with \eqn{z_{j,t}} i.i.d. standard normal. The residuals in GPCA space are \eqn{u_t=B w_t}
#' where \eqn{B} is the lower-triangular Cholesky factor of their covariance matrix.
#'
#' @slot GARCH a list with elements \code{B} (Cholesky factor), \code{coef} (matrix of GARCH(1,1) coefficients, one row per component),
#' \code{h_last} and \code{w_last} (conditional variance and decorrelated residual at the last time step, used to initialize generation)
#' and \code{loglik}, \code{convergence}.
#'
#' @seealso \code{\link{fitGARCH}}, \code{\link{getVARmodel}}, \code{\link{newVARmultieventRealization}}
#'
#' @name GPCAvarest2garch-class
#' @rdname GPCAvarest2garch-class
#' @exportClass GPCAvarest2garch
#' @title GPCAvarest2garch-class
#' @author Emanuele Cordano
#'
setClass("GPCAvarest2garch",representation(GARCH="list"),contains="GPCAvarest2")

NULL
#' Fits a GARCH(1,1) model on a univariate series by Gaussian maximum likelihood
#'
#' @param x numeric vector (zero-mean series)
#' @param max_persistence maximum allowed value of \eqn{\alpha+\beta} (to guarantee stationarity in generation)
#' @param ... further arguments for \code{\link{optim}}
#'
#' @return a list with \code{omega}, \code{alpha}, \code{beta}, \code{h} (conditional variances), \code{loglik} and \code{convergence}
#'
#' @importFrom stats optim plogis qlogis var
#' @noRd
#'
garch11_fit <- function(x,max_persistence=0.995,...) {

	x <- as.numeric(x)
	x <- x[!is.na(x)]
	n <- length(x)
	v <- stats::var(x)

	## reparameterization: omega=exp(p1), persistence=alpha+beta=max_persistence*plogis(p2), share of alpha=plogis(p3)
	unpack <- function(par) {
		pers <- max_persistence*stats::plogis(par[2])
		w <- stats::plogis(par[3])
		c(omega=exp(par[1]),alpha=pers*w,beta=pers*(1-w))
	}
	cond_var <- function(cf) {
		input <- cf["omega"]+cf["alpha"]*x[-n]^2
		h <- as.numeric(stats::filter(input,cf["beta"],method="recursive",init=v))
		c(v,h)
	}
	nll <- function(par) {
		h <- cond_var(unpack(par))
		if (any(!is.finite(h)) | any(h<=0)) return(1e10)
		0.5*sum(log(h)+x^2/h)
	}

	## starting values: alpha=0.05, beta=0.90, omega so that unconditional variance=v
	pers0 <- 0.95
	par0 <- c(log(v*(1-pers0)),stats::qlogis(pers0/max_persistence),stats::qlogis(0.05/pers0))
	opt <- stats::optim(par0,nll,...)
	cf <- unpack(opt$par)
	h <- cond_var(cf)

	out <- list(omega=as.numeric(cf["omega"]),alpha=as.numeric(cf["alpha"]),beta=as.numeric(cf["beta"]),
			h=h,loglik=-opt$value-0.5*n*log(2*pi),convergence=opt$convergence)
	return(out)
}

NULL
#' Fits a diagonal (orthogonal) GARCH(1,1) model on the residuals of a \code{GPCAvarest2} object
#'
#' @param object a \code{\link{GPCAvarest2-class}} object (e.g. returned by \code{\link{getVARmodel}} with \code{n_GPCA_iteration>0} or \code{n_GPCA_iteration_residuals>0})
#' @param max_persistence maximum allowed value of \eqn{\alpha+\beta} for each component. Default is 0.995.
#' @param ... further arguments for \code{\link{optim}}
#'
#' @details Residuals \eqn{u_t} (in GPCA space, see \code{\link{residuals.varest2}}) are decorrelated through
#' the inverse of the Cholesky factor \eqn{B} of their covariance matrix (the same matrix used by default by \code{\link{newVARmultieventRealization}}).
#' A GARCH(1,1) model is then fitted on each decorrelated component.
#'
#' @return a \code{\link{GPCAvarest2garch-class}} object
#'
#' @export
#' @author Emanuele Cordano
#' @seealso \code{\link{arch_test}},\code{\link{getVARmodel}}
#'
#' @examples
#' data(trentino)
#' year_min <- 1961
#' year_max <- 1990
#' period <- PRECIPITATION$year>=year_min & PRECIPITATION$year<=year_max
#' station <- names(PRECIPITATION)[!(names(PRECIPITATION) %in% c("day","month","year"))]
#' station <- station[1:2]
#' Tx <- TEMPERATURE_MAX[period,station]
#' Tn <- TEMPERATURE_MIN[period,station]
#' data <- cbind(Tx,Tn)
#' data <- (data-matrix(colMeans(data,na.rm=TRUE),nrow(data),ncol(data),byrow=TRUE))
#' \donttest{
#' var <- getVARmodel(data=data,suffix=NULL,p=1,n_GPCA_iteration=2)
#' vargarch <- fitGARCH(var)
#' vargarch@GARCH$coef
#' }
#'
fitGARCH <- function(object,max_persistence=0.995,...) {

	if (!inherits(object,"GPCAvarest2")) stop("fitGARCH: object must be a GPCAvarest2 object (use n_GPCA_iteration>0 or n_GPCA_iteration_residuals>0 in getVARmodel)")

	u <- as.matrix(residuals(object))
	B <- t(chol(stats::cov(u)))
	w <- t(solve(B,t(u)))

	fits <- lapply(1:ncol(w),function(j) garch11_fit(w[,j],max_persistence=max_persistence,...))

	coef <- t(sapply(fits,function(f) c(omega=f$omega,alpha=f$alpha,beta=f$beta)))
	rownames(coef) <- colnames(u)

	h_last <- sapply(fits,function(f) f$h[length(f$h)])
	w_last <- w[nrow(w),]

	out <- new("GPCAvarest2garch",object)
	out@GARCH <- list(B=B,coef=coef,h_last=h_last,w_last=w_last,
			loglik=sapply(fits,function(f) f$loglik),
			convergence=sapply(fits,function(f) f$convergence))

	return(out)

}

NULL
#' Generates decorrelated GARCH(1,1) noise for a \code{GPCAvarest2garch} object
#'
#' @param object a \code{\link{GPCAvarest2garch-class}} object
#' @param nrealization number of time steps
#' @param init initialization of the conditional variance: \code{"last"} (last fitted value) or \code{"unconditional"}
#'
#' @return a \code{nrealization x K} matrix of noise with time-varying conditional variance (unit unconditional variance for each component, approximately)
#'
#' @importFrom stats rnorm
#' @export
#' @seealso \code{\link{newVARmultieventRealization}}
#'
garch_noise <- function(object,nrealization=10,init=c("last","unconditional")) {

	init <- init[1]
	cf <- object@GARCH$coef
	K <- nrow(cf)
	omega <- cf[,"omega"]
	alpha <- cf[,"alpha"]
	beta <- cf[,"beta"]

	if (init=="last") {
		h <- object@GARCH$h_last
		w <- object@GARCH$w_last
	} else {
		h <- omega/(1-alpha-beta)
		w <- sqrt(h)*stats::rnorm(K)
	}

	out <- array(NA,c(nrealization,K))
	z <- array(stats::rnorm(K*nrealization),c(nrealization,K))
	for (i in 1:nrealization) {
		h <- omega+alpha*w^2+beta*h
		w <- sqrt(h)*z[i,]
		out[i,] <- w
	}

	return(out)

}

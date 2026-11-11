NULL
#' The class \code{GPCAvarest2garch}
#'
#' Extension of \code{\link{GPCAvarest2-class}} in which the (GPCA-Gaussianized) VAR residuals
#' are modelled, after decorrelation, with independent univariate GARCH processes
#' (orthogonal GARCH approach). The residuals in GPCA space are \eqn{u_t=B w_t},
#' where \eqn{B} is the lower-triangular Cholesky factor of their covariance matrix, and each
#' decorrelated component is \eqn{w_{j,t}=\sqrt{h_{j,t}}\,z_{j,t}} with \eqn{z_{j,t}} i.i.d. standard normal.
#'
#' Two specifications of the conditional variance \eqn{h_{j,t}} are available (see \code{\link{fitGARCH}}):
#' \describe{
#' \item{\code{"standard"}}{GARCH(\eqn{q},\eqn{p}) (Bollerslev, 1986) with \eqn{q} ARCH and \eqn{p} GARCH terms:
#' \deqn{h_t=\omega+\sum_{i=1}^{q}\alpha_i w_{t-i}^2+\sum_{k=1}^{p}\beta_k h_{t-k}}}
#' \item{\code{"component"}}{component GARCH (Engle and Lee, 1999), with a long-run component \eqn{q_t}:
#' \deqn{q_t=\omega+\rho\, q_{t-1}+\phi\,(w_{t-1}^2-h_{t-1})}
#' \deqn{h_t=q_t+\alpha\,(w_{t-1}^2-q_{t-1})+\beta\,(h_{t-1}-q_{t-1})}}
#' \item{\code{"common"}}{a scalar volatility factor \eqn{g_t} shared by all components (\eqn{h_{j,t}=g_t}):
#' \deqn{g_t=\omega+\alpha\,\bar{w^2}_{t-1}+\beta\,g_{t-1},\qquad \bar{w^2}_{t}=\frac{1}{K}\sum_{j=1}^{K}w_{j,t}^2}
#' Since \eqn{\bar{w^2}_t=u_t^T\Sigma^{-1}u_t/K}, the model does not depend on the choice of \eqn{B}: the whole residual covariance matrix is scaled by \eqn{g_t}.}
#' \item{\code{"common_idio"}}{common factor and an idiosyncratic GARCH(\eqn{q},\eqn{p}) of each component:
#' \eqn{h_{j,t}=g_t\,d_{j,t}}, where \eqn{d_{j,t}} is a standard GARCH of \eqn{x_{j,t}=w_{j,t}/\sqrt{g_t}}. Estimated in two steps (first \eqn{g_t}, then \eqn{d_{j,t}}).}
#' }
#'
#' @slot GARCH a list with elements \code{type}, \code{order}, \code{B} (Cholesky factor), \code{coef} (matrix of coefficients, one row per component),
#' \code{h} and \code{w} (fitted conditional variances and decorrelated residuals), \code{qlr} (long-run component, only for \code{"component"}),
#' \code{loglik}, \code{npar}, \code{BIC}, \code{AIC} and \code{convergence} (one value per component; for \code{"common"} and \code{"common_idio"} they are totals over all components),
#' \code{coef_common} and \code{g} (coefficients and series of the common factor, only for \code{"common"} and \code{"common_idio"}).
#'
#' @seealso \code{\link{fitGARCH}}, \code{\link{garch_noise}}, \code{\link{garch_standardize}}, \code{\link{getVARmodel}}, \code{\link{newVARmultieventRealization}}
#'
#' @references
#' Bollerslev T. (1986) Generalized autoregressive conditional heteroskedasticity. Journal of Econometrics 31, 307-327.
#'
#' Engle R.F., Lee G.G.J. (1999) A long-run and short-run component model of stock return volatility. In: Engle R.F., White H. (eds.), Cointegration, Causality, and Forecasting, Oxford University Press, 475-497.
#'
#' @name GPCAvarest2garch-class
#' @rdname GPCAvarest2garch-class
#' @exportClass GPCAvarest2garch
#' @title GPCAvarest2garch-class
#' @author Emanuele Cordano
#'
setClass("GPCAvarest2garch",representation(GARCH="list"),contains="GPCAvarest2")

NULL
#' Conditional variance recursion of a standard GARCH(q,p) model
#'
#' @param x numeric vector
#' @param omega,alpha,beta coefficients (\code{alpha} of length q, \code{beta} of length p, possibly 0)
#' @param v initial value for the past conditional variances and squared values
#' @importFrom stats filter
#' @noRd
garch_std_var <- function(x,omega,alpha,beta,v=stats::var(x)) {

	n <- length(x)
	q <- length(alpha)
	p <- length(beta)
	x2 <- c(rep(v,q),x^2)
	if (q>0) {
		## conv[k]=sum_i alpha_i*x2[k-i+1] ; input_t=omega+conv[t+q-1] (x_{t-i} has index t+q-i in x2)
		conv <- as.numeric(stats::filter(x2,alpha,method="convolution",sides=1))
		input <- omega+conv[(1:n)+q-1]
	} else {
		input <- rep(omega,n)
	}
	if (p>0) {
		h <- as.numeric(stats::filter(input,beta,method="recursive",init=rep(v,p)))
	} else {
		h <- input
	}
	return(h)
}

NULL
#' Conditional variance recursion of a component GARCH model (Engle and Lee, 1999)
#'
#' @param x numeric vector
#' @param cf named vector with \code{omega}, \code{rho}, \code{phi}, \code{alpha}, \code{beta}
#' @param v initial value for conditional variance and long-run component
#' @return a list with \code{h} (conditional variance) and \code{q} (long-run component)
#' @noRd
garch_cmp_var <- function(x,cf,v=stats::var(x)) {

	n <- length(x)
	omega <- cf[["omega"]]; rho <- cf[["rho"]]; phi <- cf[["phi"]]
	alpha <- cf[["alpha"]]; beta <- cf[["beta"]]
	h <- numeric(n); q <- numeric(n)
	hp <- v; qp <- v; x2p <- v
	for (t in 1:n) {
		qt <- omega+rho*qp+phi*(x2p-hp)
		ht <- qt+alpha*(x2p-qp)+beta*(hp-qp)
		h[t] <- ht; q[t] <- qt
		hp <- ht; qp <- qt; x2p <- x[t]^2
	}
	return(list(h=h,q=q))
}

NULL
#' Fits a univariate GARCH model by Gaussian maximum likelihood
#'
#' @param x numeric vector (zero-mean series)
#' @param order named integer vector \code{c(arch=q,garch=p)}, used if \code{type=="standard"}
#' @param type \code{"standard"} or \code{"component"}
#' @param max_persistence maximum allowed persistence (\eqn{\sum\alpha_i+\sum\beta_k} for \code{"standard"}, \eqn{\rho} for \code{"component"})
#' @param ... further arguments for \code{\link{optim}}
#'
#' @return a list with \code{coef}, \code{h}, \code{q} (long-run component or \code{NULL}), \code{loglik}, \code{npar} and \code{convergence}
#'
#' @importFrom stats optim plogis qlogis var
#' @noRd
#'
garch_fit <- function(x,order=c(arch=1,garch=1),type=c("standard","component"),max_persistence=0.995,...) {

	type <- match.arg(type)
	x <- as.numeric(x)
	x <- x[!is.na(x)]
	n <- length(x)
	v <- stats::var(x)

	if (type=="standard") {

		q <- as.integer(order[1]); p <- as.integer(order[2])
		m <- q+p
		if (q<1) stop("garch_fit: at least one ARCH term is required (order[1]>=1)")
		## parameters: log(omega), logit of persistence S/max_persistence, m-1 free logits of the shares (softmax, first share fixed)
		unpack <- function(par) {
			S <- max_persistence*stats::plogis(par[2])
			e <- exp(c(0,par[-(1:2)]))
			w <- e/sum(e)
			ab <- S*w
			list(omega=exp(par[1]),alpha=ab[seq_len(q)],beta=ab[q+seq_len(p)])
		}
		nll <- function(par) {
			cf <- unpack(par)
			h <- garch_std_var(x,cf$omega,cf$alpha,cf$beta,v)
			if (any(!is.finite(h)) | any(h<=0)) return(1e10)
			0.5*sum(log(h)+x^2/h)
		}
		## start: total alpha=0.05, total beta=0.90 (or 0.3 with no GARCH term), equally split
		alpha0 <- if (p>0) 0.05 else 0.30
		beta0 <- if (p>0) 0.90 else 0
		S0 <- alpha0+beta0
		w0 <- c(rep(alpha0/q,q),rep(beta0/max(p,1),p))/S0
		par0 <- c(log(v*(1-S0)),stats::qlogis(S0/max_persistence),log(w0[-1]/w0[1]))
		opt <- stats::optim(par0,nll,control=list(maxit=2000*m),...)
		cf <- unpack(opt$par)
		h <- garch_std_var(x,cf$omega,cf$alpha,cf$beta,v)
		coef <- c(omega=cf$omega,
				if (q>0) structure(cf$alpha,names=paste0("alpha",seq_len(q))),
				if (p>0) structure(cf$beta,names=paste0("beta",seq_len(p))))
		qlr <- NULL
		npar <- 1+m

	} else {

		## constraints: 0<rho<max_persistence, 0<=alpha+beta<rho, 0<=phi<beta, omega>0
		unpack <- function(par) {
			rho <- max_persistence*stats::plogis(par[2])
			S <- rho*stats::plogis(par[3])
			alpha <- S*stats::plogis(par[4])
			beta <- S-alpha
			phi <- beta*stats::plogis(par[5])
			c(omega=exp(par[1]),rho=rho,phi=phi,alpha=alpha,beta=beta)
		}
		nll <- function(par) {
			h <- garch_cmp_var(x,unpack(par),v)$h
			if (any(!is.finite(h)) | any(h<=0)) return(1e10)
			0.5*sum(log(h)+x^2/h)
		}
		## start: rho=0.99, alpha+beta=0.7, alpha=0.07, phi=0.02, long-run variance=v
		rho0 <- min(0.99,0.99*max_persistence)
		par0 <- c(log(v*(1-rho0)),stats::qlogis(rho0/max_persistence),stats::qlogis(0.7/rho0),
				stats::qlogis(0.1),stats::qlogis(0.02/0.63))
		opt <- stats::optim(par0,nll,control=list(maxit=5000),...)
		coef <- unpack(opt$par)
		vv <- garch_cmp_var(x,coef,v)
		h <- vv$h
		qlr <- vv$q
		npar <- 5

	}

	out <- list(coef=coef,h=h,q=qlr,loglik=-opt$value-0.5*n*log(2*pi),npar=npar,convergence=opt$convergence)
	return(out)
}

NULL
#' Fits a common (scalar) GARCH(1,1) volatility factor on a matrix of decorrelated residuals
#'
#' @param w matrix of decorrelated residuals (unit variance, uncorrelated columns)
#' @param max_persistence maximum allowed value of \eqn{\alpha+\beta}
#' @param ... further arguments for \code{\link{optim}}
#'
#' @return a list with \code{coef} (\code{omega}, \code{alpha}, \code{beta}), \code{g}, \code{loglik}, \code{npar} and \code{convergence}
#' @noRd
#'
garch_common_fit <- function(w,max_persistence=0.995,...) {

	w <- as.matrix(w)
	n <- nrow(w)
	K <- ncol(w)
	m <- rowMeans(w^2,na.rm=TRUE)
	s2 <- rowSums(w^2,na.rm=TRUE)
	v <- mean(m)
	unpack <- function(par) {
		S <- max_persistence*stats::plogis(par[2])
		a <- S*stats::plogis(par[3])
		c(omega=exp(par[1]),alpha=a,beta=S-a)
	}
	gfun <- function(cf) garch_std_var(sqrt(m),cf[["omega"]],cf[["alpha"]],cf[["beta"]],v)
	nll <- function(par) {
		g <- gfun(unpack(par))
		if (any(!is.finite(g)) | any(g<=0)) return(1e12)
		0.5*sum(K*log(g)+s2/g)
	}
	S0 <- 0.95
	par0 <- c(log(v*(1-S0)),stats::qlogis(S0/max_persistence),stats::qlogis(0.05/S0))
	opt <- stats::optim(par0,nll,control=list(maxit=2000),...)
	cf <- unpack(opt$par)
	list(coef=cf,g=gfun(cf),loglik=-opt$value-0.5*n*K*log(2*pi),npar=3,convergence=opt$convergence)
}

NULL
#' Fits a diagonal (orthogonal) GARCH model on the residuals of a \code{GPCAvarest2} object
#'
#' @param object a \code{\link{GPCAvarest2-class}} object (e.g. returned by \code{\link{getVARmodel}} with \code{n_GPCA_iteration>0} or \code{n_GPCA_iteration_residuals>0})
#' @param garch_order integer vector of length 2: number of ARCH terms (\eqn{q}) and of GARCH terms (\eqn{p}). It may be named \code{c(arch=q,garch=p)}. Default is \code{c(arch=1,garch=1)}. Used if \code{garch_type} is \code{"standard"} or \code{"common_idio"} (idiosyncratic part).
#' @param garch_type \code{"standard"} (GARCH(\eqn{q},\eqn{p}) for each component, default), \code{"component"} (component GARCH of Engle and Lee, 1999, for each component),
#' \code{"common"} (one scalar GARCH(1,1) volatility factor shared by all components) or \code{"common_idio"} (common factor times an idiosyncratic GARCH(\eqn{q},\eqn{p}) of each component). See \code{\link{GPCAvarest2garch-class}}.
#' @param max_persistence maximum allowed persistence: \eqn{\sum\alpha_i+\sum\beta_k} for \code{"standard"} and for the common factor, \eqn{\rho} for \code{"component"}. Default is 0.995.
#' @param ... further arguments for \code{\link{optim}}
#'
#' @details Residuals \eqn{u_t} (in GPCA space, see \code{\link{residuals.varest2}}) are decorrelated through
#' the inverse of the Cholesky factor \eqn{B} of their covariance matrix (the same matrix used by default by \code{\link{newVARmultieventRealization}}).
#' The GARCH model is then fitted on the decorrelated components by Gaussian maximum likelihood.
#' The ARCH and GARCH coefficients are constrained to be non-negative, which guarantees positive conditional variances.
#' With independent GARCH processes for each component (\code{"standard"}, \code{"component"}), volatility clustering is partially averaged out when the components are
#' mixed back through \eqn{B}; a common factor (\code{"common"}, \code{"common_idio"}) scales all components at the same time and preserves clustering shared by stations and variables.
#' The slot \code{GARCH} also contains log-likelihood, AIC and BIC, which can be used to compare specifications.
#'
#' @return a \code{\link{GPCAvarest2garch-class}} object
#'
#' @export
#' @author Emanuele Cordano
#' @seealso \code{\link{arch_test}},\code{\link{getVARmodel}},\code{\link{garch_standardize}}
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
#' vg11 <- fitGARCH(var)
#' vg12 <- fitGARCH(var,garch_order=c(arch=1,garch=2))
#' vgc <- fitGARCH(var,garch_type="component")
#' vcom <- fitGARCH(var,garch_type="common")
#' vci <- fitGARCH(var,garch_type="common_idio")
#' sapply(list(G11=vg11,G12=vg12,CGARCH=vgc,COMMON=vcom,COMMON_IDIO=vci),function(x) sum(x@GARCH$BIC))
#' vcom@GARCH$coef_common
#' }
#'
fitGARCH <- function(object,garch_order=c(arch=1,garch=1),garch_type=c("standard","component","common","common_idio"),max_persistence=0.995,...) {

	garch_type <- match.arg(garch_type)
	if (!inherits(object,"GPCAvarest2")) stop("fitGARCH: object must be a GPCAvarest2 object (use n_GPCA_iteration>0 or n_GPCA_iteration_residuals>0 in getVARmodel)")
	garch_order <- as.integer(garch_order)
	if (length(garch_order)!=2 | any(garch_order<0)) stop("fitGARCH: garch_order must be a vector of two non-negative integers")
	names(garch_order) <- c("arch","garch")
	if (garch_type=="component") garch_order <- c(arch=2L,garch=2L) ## the component GARCH is a constrained GARCH(2,2)
	if (garch_type=="common") garch_order <- c(arch=1L,garch=1L)

	u <- as.matrix(residuals(object))
	B <- t(chol(stats::cov(u)))
	w <- t(solve(B,t(u)))
	colnames(w) <- colnames(u)
	n <- nrow(w)
	K <- ncol(w)

	qlr <- NULL
	coef_common <- NULL
	g <- NULL
	coef <- NULL

	if (garch_type %in% c("common","common_idio")) {

		cfit <- garch_common_fit(w,max_persistence=max_persistence,...)
		coef_common <- cfit$coef
		g <- cfit$g
		h <- matrix(g,n,K)
		loglik <- cfit$loglik
		npar <- cfit$npar
		convergence <- cfit$convergence

		if (garch_type=="common_idio") {
			x <- w/sqrt(g)
			fits <- lapply(1:K,function(j) garch_fit(x[,j],order=garch_order,type="standard",max_persistence=max_persistence,...))
			coef <- t(sapply(fits,function(f) f$coef))
			rownames(coef) <- colnames(u)
			d <- sapply(fits,function(f) f$h)
			h <- g*d
			## exact Gaussian log-likelihood of w with conditional variance g*d
			loglik <- -0.5*sum(log(2*pi*h)+w^2/h)
			npar <- npar+sum(sapply(fits,function(f) f$npar))
			convergence <- c(common=convergence,sapply(fits,function(f) f$convergence))
		}
		names(loglik) <- names(npar) <- "total"

	} else {

		fits <- lapply(1:K,function(j) garch_fit(w[,j],order=garch_order,type=garch_type,max_persistence=max_persistence,...))
		coef <- t(sapply(fits,function(f) f$coef))
		rownames(coef) <- colnames(u)
		h <- sapply(fits,function(f) f$h)
		if (garch_type=="component") {
			qlr <- sapply(fits,function(f) f$q)
			colnames(qlr) <- colnames(u)
		}
		loglik <- sapply(fits,function(f) f$loglik)
		npar <- sapply(fits,function(f) f$npar)
		names(loglik) <- names(npar) <- colnames(u)
		convergence <- sapply(fits,function(f) f$convergence)
	}
	colnames(h) <- colnames(u)

	out <- new("GPCAvarest2garch",object)
	out@GARCH <- list(type=garch_type,order=garch_order,B=B,coef=coef,coef_common=coef_common,g=g,h=h,w=w,qlr=qlr,
			loglik=loglik,npar=npar,AIC=-2*loglik+2*npar,BIC=-2*loglik+log(n)*npar,
			convergence=convergence)

	return(out)

}

NULL
#' GARCH-standardized residuals of a \code{GPCAvarest2garch} object
#'
#' @param object a \code{\link{GPCAvarest2garch-class}} object
#' @param space \code{"residuals"} (default): standardized residuals mapped back to the GPCA residual space through \eqn{B}, i.e. \eqn{B\,(w_t/\sqrt{h_t})};
#' \code{"decorrelated"}: \eqn{w_t/\sqrt{h_t}}.
#'
#' @return a matrix of standardized residuals, which should be homoscedastic if the GARCH model is adequate (see \code{\link{arch_test}})
#'
#' @export
#' @seealso \code{\link{fitGARCH}}
#'
garch_standardize <- function(object,space=c("residuals","decorrelated")) {

	space <- match.arg(space)
	G <- object@GARCH
	z <- G$w/sqrt(G$h)
	if (space=="residuals") z <- z %*% t(G$B)
	colnames(z) <- colnames(G$w)
	return(z)
}

NULL
#' Generates decorrelated GARCH noise for a \code{GPCAvarest2garch} object
#'
#' @param object a \code{\link{GPCAvarest2garch-class}} object
#' @param nrealization number of time steps
#' @param init initialization of the recursion: \code{"last"} (last fitted values) or \code{"unconditional"} (unconditional variance)
#'
#' @return a \code{nrealization x K} matrix of noise with time-varying conditional variance
#'
#' @importFrom stats rnorm
#' @export
#' @seealso \code{\link{newVARmultieventRealization}}
#'
garch_noise <- function(object,nrealization=10,init=c("last","unconditional")) {

	init <- match.arg(init)
	G <- object@GARCH
	type <- if (is.null(G$type)) "standard" else G$type
	K <- ncol(G$B)
	z <- array(stats::rnorm(K*nrealization),c(nrealization,K))
	out <- array(NA,c(nrealization,K))
	nfit <- if (is.null(G$w)) 0 else nrow(G$w)

	if (type=="component") {

		cf <- G$coef
		omega <- cf[,"omega"]; rho <- cf[,"rho"]; phi <- cf[,"phi"]; alpha <- cf[,"alpha"]; beta <- cf[,"beta"]
		if (init=="last") {
			h <- G$h[nfit,]; ql <- G$qlr[nfit,]; w <- G$w[nfit,]
		} else {
			h <- ql <- omega/(1-rho)
			w <- sqrt(h)*stats::rnorm(K)
		}
		for (i in 1:nrealization) {
			qn <- omega+rho*ql+phi*(w^2-h)
			h <- qn+alpha*(w^2-ql)+beta*(h-ql)
			h <- pmax(h,1e-8)
			ql <- qn
			w <- sqrt(h)*z[i,]
			out[i,] <- w
		}
		return(out)
	}

	common <- type %in% c("common","common_idio")
	idio <- type %in% c("standard","common_idio")

	## common factor g (1 if absent)
	if (common) {
		cc <- G$coef_common
		if (init=="last") {
			g <- G$g[nfit]
		} else {
			g <- cc[["omega"]]/(1-cc[["alpha"]]-cc[["beta"]])
		}
	} else {
		g <- 1
	}

	## idiosyncratic GARCH(q,p) on x=w/sqrt(g) (d=1 if absent)
	if (idio) {
		cf <- G$coef
		omega <- cf[,"omega"]
		alpha <- cf[,grep("^alpha",colnames(cf)),drop=FALSE]; beta <- cf[,grep("^beta",colnames(cf)),drop=FALSE]
		q <- ncol(alpha); p <- ncol(beta)
		if (init=="last") {
			gfit <- if (common) G$g else rep(1,nfit)
			xfit <- G$w/sqrt(gfit)
			dfit <- G$h/gfit
			xb <- xfit[nfit:max(1,nfit-q+1),,drop=FALSE]
			db <- dfit[nfit:max(1,nfit-max(p,1)+1),,drop=FALSE]
		} else {
			du <- omega/(1-rowSums(alpha)-rowSums(beta))
			xb <- matrix(sqrt(du),q,K,byrow=TRUE)*matrix(stats::rnorm(q*K),q,K)
			db <- matrix(du,max(p,1),K,byrow=TRUE)
		}
	}

	if (init=="last") {
		w <- G$w[nfit,]
	} else {
		w <- sqrt(g)*stats::rnorm(K)
	}

	for (i in 1:nrealization) {
		if (common) g <- cc[["omega"]]+cc[["alpha"]]*mean(w^2)+cc[["beta"]]*g
		if (idio) {
			d <- omega+colSums(t(alpha)*xb^2)
			if (p>0) d <- d+colSums(t(beta)*db[1:p,,drop=FALSE])
		} else {
			d <- rep(1,K)
		}
		x <- sqrt(d)*z[i,]
		w <- sqrt(g)*x
		out[i,] <- w
		if (idio) {
			xb <- rbind(x,xb)[1:q,,drop=FALSE]
			db <- rbind(d,db)[1:max(p,1),,drop=FALSE]
		}
	}

	return(out)

}

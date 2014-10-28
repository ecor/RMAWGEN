\name{inv_GPCA}
\alias{inv_GPCA}
\title{This function makes an inverse Gaussianization procedure besad on PCA iteration ( see \code{\link{inv_GPCA_iteration}}}
\usage{
inv_GPCA(x = NULL, GPCA_param, type = 3, extremes = TRUE)
}
\arguments{
  \item{x}{gaussian random variable to transform}

  \item{GPCA_param}{\code{\link{GPCA-class}} S3 object
  returned by the function \code{\link{GPCA}}}

  \item{extremes}{see
  \code{\link{normalizeGaussian_severalstations}}}

  \item{type}{see
  \code{\link{normalizeGaussian_severalstations}}}
}
\value{
the non-Gaussian random variable
}
\description{
This function makes an inverse Gaussianization procedure
besad on PCA iteration ( see
\code{\link{inv_GPCA_iteration}}
}
\note{
This function re-iterates the inverse of equation (1) of
"PCA Gaussianization for One-Class Remote Sensing Image" by
V. Laparra et al.,
\url{http://dx.doi.org/doi/10.1117/12.834011}
}
\examples{
library(RMAWGEN)
set.seed(1222)
nIterations <- 30
N <- 20
x <- rexp(N)
y <- x+rnorm(N)
df <- data.frame(x=x,y=y)

GPCA <- GPCA(df,n=nIterations,extremes=TRUE)

x <- rnorm(N)
y <- x+rnorm(N)
dfn <- data.frame(x=x,y=y)

GPCAn <- GPCA(dfn,n=nIterations,extremes=TRUE)

df_out <- inv_GPCA(GPCA_param=GPCA,extremes=TRUE)
dfn_out <- inv_GPCA(GPCA_param=GPCAn,extremes=TRUE)
}
\author{
Emanuele Cordano
}
\seealso{
\code{\link{GPCA}},\code{\link{GPCA_iteration}},\code{\link{inv_GPCA_iteration}},\code{\link{inv_GPCA}}
}


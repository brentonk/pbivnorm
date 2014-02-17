\name{pbivnorm}
\alias{pbivnorm}
\title{Standard bivariate normal CDF}
\usage{
pbivnorm(x, y, rho = 0)
}
\arguments{
  \item{x}{vector of upper integration limits for the CDF.
  May also be a two-column matrix, in which case \code{y}
  should not be used.}

  \item{y}{vector of upper integration limits, must be same
  length as \code{x}.}

  \item{rho}{correlation parameter.  May be a vector of the
  same length as \code{x}, a scalar, or a vector of any
  other length that can be recycled to conform with
  \code{x}.}
}
\value{
Numeric vector of probabilities.
}
\description{
Calculate probabilities from the CDF of a standard
bivariate normal distribution.
}
\details{
This function returns values identical to those of
\code{biv.nt.prob} in the \pkg{mnormt} package, but is
vectorized to reduce the number of Fortran calls required
for computation of many probabilities.
}
\examples{
x <- rnorm(10)
y <- rnorm(10)
rho <- runif(10)

pbivnorm(x, y, rho)

X <- cbind(x, y)
pbivnorm(X, rho = rho)

## rho can be a single value
rho <- runif(1)
pbivnorm(x, y, rho)
}
\author{
Fortran code by Alan Genz (see references).  R interface by
Brenton Kenkel (\email{brenton.kenkel@gmail.com}), based on
code from Adelchi Azzalini's \pkg{mnormt} package.
}
\references{
Genz, A.  (1992).  Numerical Computation of Multivariate
Normal Probabilities.  \emph{J. Computational and Graphical
Statist.}, \strong{1}, 141--149.

Genz, A.  (1993).  Comparison of methods for the
computation of multivariate normal probabilities.
\emph{Computing Science and Statistics}, \strong{25},
400--405.

Genz, A.  Fortran code for \code{MVTDSTPACK} available at
\url{http://www.math.wsu.edu/math/faculty/genz/software/fort77/mvtdstpack.f}
(as of 2011-02-21).
}


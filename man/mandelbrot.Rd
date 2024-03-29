% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mandelbrot.R
\name{mandelbrot}
\alias{mandelbrot}
\title{The Mandelbrot Set}
\usage{
mandelbrot(x = c(-2, 1), y = c(-1.5, 1.5), nx = 600, ny = 600, iter = 50)
}
\arguments{
\item{x}{x limits}

\item{y}{y limits}

\item{nx}{x resolution}

\item{ny}{y resolution}

\item{iter}{maximum number of iterations}
}
\value{
A list with components: \code{x}, a vector of \eqn{x} coordinates (real
part), \code{y}, a vector of \eqn{y} coordinates (imaginary part), and \code{z},
a \code{nx} * \code{ny} matrix with the number of interations \eqn{i} used to
evaluate the complex point \eqn{c = x + yi}.
}
\description{
Calculate the Mandelbrot set
}
\details{
The set is obtained by iterating \eqn{z_{j+1}=z_j^2+c} where
\eqn{z} and \eqn{c} are complex numbers. The initial value of \eqn{z},
\eqn{z_0} is 0, and the value of \eqn{c} is chosen arbitrarily.
It can be shown that if after a certain number of iterations, the modulus
of \eqn{Z} is greater than 2, then the value of \eqn{z} will head off to
infinity. The Mandelbrot set is defined as the set of points \eqn{c} in the
complex plane for which the iteration procedure is bound (that is, for which
\eqn{z} does not head off to infinity).
}
\examples{
z <- mandelbrot()
# The set is black
image(z, col=c(hcl.colors(n=100, pal="Lisbon"), "black"), las=1)

}

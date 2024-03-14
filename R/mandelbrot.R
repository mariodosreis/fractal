#######################################################################
# Function to calculate the Mandelbrot set. This function calls a
# C routine in order to perform the calculations faster.
#
# Written by Mario dos Reis. September 2003
# Modified for package release on December 2016
#
# Modified: added if(is.list(x)){...} at start to check if co-ords
#           are from locator() or similar - Jason Turner oct 2 2003
#######################################################################

#' The Mandelbrot Set
#'
#' Calculate the Mandelbrot set
#'
#' @param x x limits
#' @param y y limits
#' @param nx x resolution
#' @param ny y resolution
#' @param iter maximum number of iterations
#'
#' @details The set is obtained by iterating \eqn{z_{j+1}=z_j^2+c} where
#' \eqn{z} and \eqn{c} are complex numbers. The initial value of \eqn{z},
#' \eqn{z_0} is 0, and the value of \eqn{c} is chosen arbitrarily.
#' It can be shown that if after a certain number of iterations, the modulus
#' of \eqn{Z} is greater than 2, then the value of \eqn{z} will head off to
#' infinity. The Mandelbrot set is defined as the set of points \eqn{c} in the
#' complex plane for which the iteration procedure is bound (that is, for which
#' \eqn{z} does not head off to infinity).
#'
#' @return A list with components: \code{x}, a vector of \eqn{x} coordinates (real
#' part), \code{y}, a vector of \eqn{y} coordinates (imaginary part), and \code{z},
#' a \code{nx} * \code{ny} matrix with the number of interations \eqn{i} used to
#' evaluate the complex point \eqn{c = x + yi}.
#'
#' @examples
#' z <- mandelbrot()
#' # The set is black
#' image(z, col=c(hcl.colors(n=100, pal="Lisbon"), "black"), las=1)
#'
#' @useDynLib fractal mandelbrot_
#' @export

mandelbrot <- function(x = c(-2, 1), y = c(-1.5, 1.5), nx = 600, ny = 600,
                       iter = 50)
{
  if(is.list(x)) {
     y <- range(x$y)
     x <- range(x$x)
   }

  xcoo <- seq(x[1], x[2], len = nx) # x coordinates
  ycoo <- seq(y[1], y[2], len = ny) # y coordinates
  set = numeric(nx*ny)              # this will store the output of
                                    # the C routine

  # This is the call to the C function itself
  the.set <- .C("mandelbrot_",
    xcoo = as.double(xcoo),
    ycoo = as.double(ycoo),
    nx = as.integer(nx),
    ny = as.integer(ny),
    set = as.integer(set),
    iter = as.integer(iter),
    PACKAGE = "fractal")$set

  # Create a list with elements x, y and z,
  # suitable for image(), persp(), etc. and return it.
  return(list(x = xcoo, y = ycoo, z = matrix(the.set, ncol = ny, byrow = TRUE)));
}

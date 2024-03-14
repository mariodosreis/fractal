#' Zoom into the Mandelbrot set
#'
#' Selects a region of a fractal image and zooms into it.
#' @param col the color palette to plot the set
#' @param fun which fractal function to plot, currently only the Mandelbrot
#' @param mu  parameter from the Julia set (not used)
#' @param nx  number of subdivisions in the x-axis (for plotting)
#' @param ny  number of subdivisions in the y-axis (for plotting)
#' @param iter number of iterations to calculate the set
#' @param plot whether to plot the zoomed-in area
#' @param invisible whether to return a matrix with the calculated iterations
#'
#' @details
#' Currently the function only zooms into the Mandelbrot set. The set should
#' be plotted with the \code{image} function. Then, a call to \code{zoom} uses
#' \code{locator} to read the coordinates of two points on the image, selected
#' by the user using the mouse.
#'
#' It is recommended that you increase the number of iterations as you zoom
#' into the boundary areas of the set.
#'
#' @examples
#' \dontrun{
#' z <- mandelbrot()
#' # The set is black
#' cols <- c(hcl.colors(n=100, pal="Lisbon"), "black")
#' image(z, col=cols, las=1)
#' points(c(-.375,.125), c(.5, 1), pch=19, col="orange")
#'
#' # Run the zoom command below, then use your mouse to click on
#' # the two orange dots on the image
#' zoom(col=cols, iter=100)
#' # now try to zoom in deeply by clicking on two arbitrary points
#' # around a region of interest
#' }
#'
#' @export
zoom <- function(col = c(heat.colors(49), "black"), fun = "mandelbrot", mu,
                 nx = 600, ny = 600, iter = NULL, plot = TRUE, invisible = TRUE)
{
  coo = locator(2)

  if (coo$x[1] > coo$x[2]) coo$x <- rev(coo$x)
  if (coo$y[1] > coo$y[2]) coo$y <- rev(coo$y)

  coo$y[2] = coo$y[1] + diff(range(coo$x))

  if(is.null(iter)) # not working well yet!
    iter <- floor(5 * 30 / diff(range(coo$x)))

  if(fun == "mandelbrot")
    frac <- mandelbrot(x = coo$x, y = coo$y, nx = nx, ny = ny, iter = iter)
  if(fun == "julia")
    frac <- julia(x = coo$x, y = coo$y, mu = mu, nx = nx, ny = ny, iter = iter)

  rect(coo$x[1], coo$y[1], coo$x[2], coo$y[2], lwd = 2, border="blue")

  #dev.new()

  if(invisible)
    image(x = frac$x, y = frac$y, z = -1/frac$z, col = col, xlab="", ylab="", main=iter)
  else
    image(frac, col = col, xlab="", ylab="", main=iter)

  invisible(frac)
}

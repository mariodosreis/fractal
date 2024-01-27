#' Plot chaotic logistic growth
#'
#' @param x.init numeric, the initial population size, X(0)
#' @param gen numeric, the number of generations
#' @param rlim numeric, the growth rate, r, range, for plotting
#' @param ylim numeric, the y-axis limits, for plotting
#' @param pch the plotting character for points
#' @param col the plotting color for points
#' @param div numeric, the number of divisions along the x-axis, for plotting
#' @param k numeric, the number of burn-out generations
#'
#' Consider a population with growth
#'
#' X(t+1) = r * X(t) * (1 - X(t))
#'
#' where r is the growth rate and X(t) is the population size at generation t, with
#' 0 <= X(t) <= 1. This equation shows chaotic behaviour for r > 3.57.
#' For example, for r < 3, the population growths until converging to a limit. For
#' 3 < r < 3.57 the population oscillates between two values. Beyond r = 3.57, the
#' behaviour becomes chaotic.
#'
#' This function plots the long-term values of X(t) as a function of r.
#'
#' @references
#' Tien-Yin Li and James A. Yorke. (1975) Period three implies chaos. \emph{
#' The American Mathematical Monthly}, 82: 985--992.
#'
#' May R. (1976) Simple mathematical models with very complicated dynamics.
#' \emph{Nature}, 261: 459--467.
#'
#' @author
#' Mario dos Reis
#'
#' @examples
#' logistic()
#'
#' \dontrun{
#' # higher resolution but slow
#' logistic(div=4000, k=128)
#' }
#'
#' @export
logistic <- function(x.init = .01, gen = 50, rlim = c(3.2, 4), ylim=c(0, 1), pch='.',
                     col=rgb(.0, .0, .0, .3), div = 2000, k = 32) {

  # non linear, chaotic function
  # r is growth rate and x is population size (from 0 to 1)
  lg = function(x, r) r * x * (1 - x)

  plot(x=0, y=0, type = 'n', xlim = rlim, ylim = c(0, 1),
       xlab = 'r', ylab = 'X(t)', las=1)

  for(r in seq(rlim[1], rlim[2], by = rlim[2] / div)) {

    x.init. <- x.init

    # run for 'gen' iterations to reach 'stationarity'
    for(i in seq(0, gen)) {

      x.next = lg(x.init., r)
      x.init. = x.next

    }

    # plot state for 'k' additional iterations and plot
    if (r < 3.4) k. <- 4 else k. <- k
    x. <- numeric(k.); x.[1] <- x.init.
    for(i in 2:k.) {

      x.[i] <- lg(x.[i-1], r)
    }
    points(rep(r, k.), x., pch = pch, col = col)
  }
}

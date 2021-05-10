library(fractal)

rm(list=ls())

par(mfrow=c(2,2))

n <- 40
z <- mandelbrot(iter=n)
image(z, col=c(heat.colors(n),"black"))
#zoom() # uses mouse clicks on image

# Fractal

An R package to calculate the Mandelbrot set. The function that calculates the set is written in C, and so it is very fast.

If you have the devtools package installed (and if you have set up an appropriate C compiler), you can install the package by typing in the R command prompt

~~~
> devtools::install_github ("mariodosreis/fractal")
~~~

# Examples

Plot the set

~~~
z <- mandelbrot(iter=40)
par(pty="s")
image(z, col=c(topo.colors(n),"black"), las=1)
~~~

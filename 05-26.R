f <- function(x){(x^2+(8/x)^2)}

x <- seq(-10,10,0.001)
plot(x,f(x),type = "l", ylim = c(0, 20))
abline(v=2.82)  
x <- seq(2.82,2.83,0.001)
plot(x,f(x),type = "l")

Df <- function(x){1/2*(x^2+(8/x)^2)^(-1/2)*(2*x-128*x^(-3))}
uniroot(Df,c(2,3))$root

64^(1/4)
2*sqrt(2)

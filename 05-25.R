f <- function(x){x*(300-x)/2}

x <- seq(0,300)
plot(x, f(x), type = "l")

f<- function(x){300*x-2*x^2}
plot(f,0,150)

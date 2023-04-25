f <- function(x){x^3}

x <- seq(-3,3,0.1)

plot(x,f(x),type = "l", ylim = c(-5,10), main = "f(x+2) shifts Left")
lines(x, f(x+2), type = "l",col = 2)

plot(x,f(x),type = "l", ylim = c(-5,10), main = "f(x-2) shifts Right")
lines(x, f(x-2), type = "l",col = 3)

plot(x,f(x),type = "l", ylim = c(-5,10), main = "f(x)+2 shifts up")
lines(x, f(x)+2, type = "l",col = 4)

plot(x,f(x),type = "l", ylim = c(-5,10), main = "f(x)+2 shift down")
lines(x, f(x)-2, type = "l",col = 5)



f <- function(x){sqrt(1-x^2)}

x <- seq(-3,3,0.1)

plot(x,f(x),type = "l", ylim = c(-3,3), main = "f(2*x) causes a ...")
lines(x, f(2*x), type = "l",col = 2)

plot(x,f(x),type = "l", ylim = c(-3,3), main = "2*f(x) causes a ...")
lines(x, 2*f(x), type = "l",col = 2)

plot(x,f(x),type = "l", ylim = c(-3,3), main = "3*f(2*x) causes a ...")
lines(x, 3*f(2*x), type = "l",col = 2)




f.quad1 <- function(x,a=1,b=0,c=0){
  a*x^2 + b*x + c
}

f.quad2 <- function(x,a,b,c){
  a*x^2 + b*x + c
}

f.quad1(1/2)
f.quad2(1/2)

f.quad1(0,1,2,7)
f.quad2(0,1,2,7)

f.quad1(-1/3)
f.quad2(-1/3,1,0,0)




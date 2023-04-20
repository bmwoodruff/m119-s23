(-2)^2
(-1)^2
(0)^2

v <- c(-2,-1,0,1)
v
v^2
v^2+3

v <- seq(-2,5)
v
v^2

(-2:5)^2


v <- seq(-2,5,2)
v
v^2


?plot



#Plot a function
v <- seq(-2,5)
v
v^2
plot(v,v^2)
plot(v,v^2, type = "l")
?plot


f <- function(x){x^2}
f(10)

x <- seq(-2,5,0.01)
plot(x, f(x), type = "l")
plot(f, -2,5)

f <- function(x){
  ifelse(x < 1, 2*x, 3)
}

f(-2)
f(-1)
f(0)
f(1)
f(45.6)

v <- c(2,5)
f(v)


plot(f,-5,5)
x <- seq(-5,5, 0.001)
plot(x, f(x), type = "l")

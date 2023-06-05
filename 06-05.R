library(data4led)
bulb <- led_bulb(1,seed=123)
t <- bulb$hours
y <- bulb$percent_intensity

plot(t,y)
## Solve the system
b <- sum((y-100)*t)
d <- sum(t^2)
b/d


f1 <- function(x, a1 = b/d){100 + a1*x}
x <- seq(0,5000,10)
plot(t,y)
lines(x,f1(x),type = "l") 




solve_system <- function(b11,b12,c1,b21,b22,c2){
  x <- (c1*b22-b12*c2)/(b11*b22-b12*b21)
  y <- (b11*c2-c1*b21)/(b11*b22-b12*b21)
  c(x,y)
}

t
y

b11 <- sum(t^2)
b12 <- sum(t^3)
c1  <- sum((y-100)*t)
b21 <- b12
b22 <- sum(t^4)
c2  <- sum((y-100)*t^2)

sol <- solve_system(b11,b12,c1,b21,b22,c2)
sol
sol[1] #This is a1
sol[2] #This is a2

f2 <- function(x, a1 = sol[1], a2 = sol[2]){100 + a1*x + a2*x^2}
x <- seq(0,5000,10)
plot(t,y)
lines(x,f2(x),type = "l") 



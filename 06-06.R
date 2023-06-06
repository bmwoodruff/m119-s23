library(data4led)
bulb <- led_bulb(1,seed=123) #Remember to use your assigned seed!

t <- bulb$hours
y <- bulb$percent_intensity

c.11 <- sum(t^2)
c.12 <- sum(t^3)
c.22 <- sum(t^4)
b.1 <- sum((y-100)*t)
b.2 <- sum((y-100)*t^2)
  
## Create a function to solve a system of equations. 
solvesystem <- function(c11, c12,b1,c21,c22,b2){ 
  c((b1*c22 - c12*b2)/(c11*c22 - c21*c12),
    (c11*b2 - b1*c21)/(c11*c22 - c21*c12))
}

sol <- solvesystem(c.11, c.12, b.1, c.12, c.22, b.2)
best.a1 <- sol[1] 
best.a2 <- sol[2] 

best.a1
best.a2

D <- (-c.11)*(-c.22)-(-c.12)^2
D
D>0
fxx <- -c.11
fxx
#D>0 (concavity is same in all directions)
#fxx <0 so concave DOWN in all directions
#We are at a maximum. 


f2 <- function(x,a0=100,a1=best.a1,a2=best.a2){
  a0 + a1*x + a2*x^2
}

x <- seq(-10,80001,2)
par(mfrow=c(1,2),mar=c(2.5,2.5,1,0.25))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f2')
lines(x,f2(x),col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,f2(x),col=2)


solve_me <- function(x){f2(x)-80}
uniroot(solve_me,c(0,80000))$root
f2(25000)





b11 <- sum(t^2)
b12 <- sum((1-exp(-.0003*t))*t)
c1 <- sum((y-100)*t)
b21 <- sum(t*(1-exp(-.0003*t)))
b22 <- sum((1-exp(-.0003*t))^2)
c2 <- sum((y-100)*(1-exp(-.0003*t)))



## Create a function to solve a system of equations. 
solvesystem <- function(c11, c12,b1,c21,c22,b2){ 
  c((b1*c22 - c12*b2)/(c11*c22 - c21*c12),
    (c11*b2 - b1*c21)/(c11*c22 - c21*c12))
}

sol <- solvesystem(b11, b12, c1, b21, b22, c2)
best.a1 <- sol[1] 
best.a2 <- sol[2] 

best.a1
best.a2

D <- (-c.11)*(-c.22)-(-c.12)^2
D
D>0
fxx <- -c.11
fxx
#D>0 (concavity is same in all directions)
#fxx <0 so concave DOWN in all directions
#We are at a maximum. 


f6 <- function(x,a0=100,a1=best.a1,a2=best.a2){
  a0 + a1*x + a2*(1-exp(-0.0003*x))
}

x <- seq(-10,80001,2)
par(mfrow=c(1,2),mar=c(2.5,2.5,1,0.25))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f2')
lines(x,f6(x),col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,f6(x),col=2)


solve_me <- function(x){f2(x)-80}
uniroot(solve_me,c(0,80000))$root
f2(25000)


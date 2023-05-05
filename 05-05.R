f.shift <- function(t){(log(3*t,2)-2)-2}
uniroot(f.shift,c(0,10))$root


f <- function(t){(log(3*t,2)-2)}
uniroot(function(x){f(x)-2},c(1,10))$root


(log(10))^2
log(10)^2
log(10^2)


g <- function(x){
  3*x-15-exp(-x+6)
}

uniroot(g,c(0,5))$root

x <-seq(0,30,1)
plot(x,g(x), type="l")
abline(h=0, col = "lightgray")



f <- function(x){
  3*x-5
}

uniroot(function(x){f(x)-0},c(0,5))$root
x <-seq(0,30,1)
plot(x,f(x), type="l")

####The third problem.
#I'll define the right hand side as a function
#I'm looking for where they intersect.
f <- function(x){
  3*x-5
}
rhs <- function(x){
  exp(-x)
}

uniroot(function(x){f(x)-rhs(x)},c(0,5))$root
x <-seq(0,3,1)
plot(x,f(x), type="l")
lines(x,rhs(x), type="l", col = 2)

########## 4
f <- function(x){
  3*x-5
}
rhs <- function(x){
  log(x)
}

uniroot(function(x){f(x)-rhs(x)},c(1,5))$root
x <-seq(0.1,3,.1)
plot(x,f(x), type="l")
lines(x,rhs(x), type="l", col = 2)



########## 5
f <- function(x){
  x^2+x-6
}
rhs <- function(x){
  0*x
}

uniroot(function(x){f(x)-rhs(x)},c(1,5))$root
uniroot(function(x){f(x)-rhs(x)},c(-5,0))$root
x <-seq(0.1,3,.1)
plot(x,f(x), type="l")
lines(x,rhs(x), type="l", col = 2)


########## 6
f <- function(x){
  x^2-8*x+12
}
rhs <- function(x){
  0*x
}

uniroot(function(x){f(x)-rhs(x)},c(1,5))
x <-seq(-5,10,.1)
plot(x,f(x), type="l")
lines(x,rhs(x), type="l", col = 2)

########## New one we made up
f <- function(x){
  (x-3)*(x-1)*(x+2)
}
rhs <- function(x){
  0*x
}

uniroot(function(x){f(x)-rhs(x)},c(-4,-1))$root
uniroot(function(x){f(x)-rhs(x)},c(-1,2))$root
uniroot(function(x){f(x)-rhs(x)},c(2,5))$root
x <-seq(-5,5,.1)
plot(x,f(x), type="l")
lines(x,rhs(x), type="l", col = 2)



rm(list=ls())
library(data4led)
bulb <- led_bulb(1,seed = 123)

t <- bulb$hours
y <- bulb$percent_intensity

f2 <- function(x,a0=0,a1=0,a2=1){ a0 + a1*x + a2*x^2 }
f3 <- function(x,a1=0,a2=1){ (100-a1) + a1*exp(-a2*x) }
f4 <- function(x,a0=0,a1=0,a2=1){a0+a1*x+a2*log(0.005*x+1)}
f5 <- function(x,a0=100,a1=0,a2=1){ (a0 + a1*x)*exp(-a2*x) }

x <- seq(-10,80001,2)
y0 <- f2(x,a0=100,a1=0,a2=0)
y1 <- f2(x,a0=100,a1=7e-4,a2=0)
y2 <- f2(x,a0=100,a1=1.1e-3,a2=-1.5e-7)
y3 <- f3(x,a1=-1.9,a2=0.00114)
y4 <- f4(x,a0=100,a1=-1.81e-4,a2=0.83)
y5 <- f5(x,a0=100,a1=6.23e-3,a2=5.06e-5)


par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f0')
lines(x,y0,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y0,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f1')
lines(x,y1,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y1,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f2')
lines(x,y2,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y2,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f3')
lines(x,y3,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y3,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f4')
lines(x,y4,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y4,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f5')
lines(x,y5,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y5,col=2)



uniroot(function(x){f5(x,a0=100,a1=6.23e-3,a2=5.06e-5)-95},c(0,80000))$root

solve_me <- function(x){f2(x,a0=100,a1=1.1e-3,a2=-1.5e-7)-80}
uniroot(solve_me, c(0,80000))$root

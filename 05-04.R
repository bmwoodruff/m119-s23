f <- function(x){
  (5+3*exp(2*x))-7
}

x <- seq(-2,0,0.1)
plot(x,f(x), type = "l")
abline(h=0)

my_root <- uniroot(f,c(-100,100))$root
my_root
abline(v=my_root)




rm(list=ls())
library(data4led)
dist <- led_time(2100)
hist(dist$percent_intensity,probability = TRUE)


#Project 1 Task 2
f0 <- function(x,a=0,b=1){
  # Make sure a < b when using this function.
  ifelse((a < x) & (x < b), 1/(b-a) + 0*x[(a < x) & (x < b)],NaN)
}

#Change these values as needed to compare plots. 
a1 <- 92
a2 <- 98

b1 <- 104
b2 <- 104

x <- seq(min(a1,a2),max(b1,b2),0.1)
y1 <- f0(x,a1,b1)
y2 <- f0(x,a2,b2)

xlimits = c(min(a1,a2)-2,max(b1,b2)+2)
ylimits = c(0,1)

par(mfrow=c(1,2),mar=c(2.5,2.5,2,0.25),oma=c(0,0,1,0))
plot(x,y1,type='l',xlim=xlimits,ylim=ylimits)
mtext(paste('a =',a1," and b=",b1), side = 3, line = 0)
plot(x,y2,type='l',xlim=xlimits,ylim=ylimits)
mtext(paste('a =',a2," and b=",b2), side = 3, line = 0)
mtext('Plot of f0',side=3,line=0,outer=TRUE)



#Project 1 Task 3
f0 <- function(x,a=0,b=1){
  # Make sure a < b when using this function.
  1/(b-a) + 0*x
}

a <- 99.5
b <- 103.5

x <- seq(a,b,0.01)
y <- f0(x,a=a,b=b)

par(mfrow=c(1,1),mar=c(2,2,3,0.25),oma=rep(0.5,4))
hist(dist$percent_intensity,
     probability = TRUE,
     xlim=c(98,105),
     ylim=c(0,0.8),
     main="Histogram of Lightbulb Intensities \n with fitted f0 function")
lines(x,y,col=2)



library(data4led)
bulb <- led_bulb(1,seed = 45876)

t <- bulb$hours
y1 <- bulb$percent_intensity

par(mfrow=c(1,1),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour", ylab="Intensity(%) ", pch=16)


f5 <- function(x,a0=100,a1=0,a2=1){ (a0 + a1*x)*exp(-a2*x) }

x <- seq(-10,800001,2)
yM <- f5(x,a0=100,a1=0.0045,a2=0.0000368)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f5')
lines(x,yM,col=2)
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,yM,col=2)




f1 <- function(x,a0=0,a1=0){ a0 + a1*x }

x <- seq(-10,80001,2)
yM <- f1(x,a0=101,a1=0.00025)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f1')
lines(x,yM,col=2)
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,yM,col=2)

f2 <- function(x,a0=0,a1=0,a2=0){ a0 + a1*x +a2*x^2 }

x <- seq(-10,80001,2)
yM <- f2(x,a0=100,a1=0.001, a2=-0.00000013)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f2')
lines(x,yM,col=2)
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,yM,col=2)


exp(2)

f <- function(x){3^(-x)}
x<- seq(-2,2,0.01)
plot(x, f(x), type = "l")

plot(f, -2,2)

my_data <- data.frame(x = x, y = f(x))


x <- seq(-4,20,0.1)
y <- 10-sqrt(x+4)
par(mar=c(2.5,2.5,0.5,0.5))
plot(x,y,type='l')

x <- seq(-4,200,0.1)
y <- 10-sqrt(x+4)
par(mar=c(2.5,2.5,0.5,0.5))
plot(x,y,type='l')



library(data4led)
dist <- led_time(2100)
hist(dist$percent_intensity,probability = TRUE)

rm(list=ls())
f0 <- function(L,a=0,b=1){
  # Make sure a < b when using this function.
  ifelse(L < a,NaN, ifelse(L <= b, 1/(b-a), NaN))
}

a <- 95
b <- 103
L <- seq(a,b,0.1)
y <- f0(L,a,b)

par(mfrow=c(2,2),mar=c(2.5,2.5,1,0.25))
plot(L,y,type='l',xlim=c(90,110), ylim = c(0,1))
mtext('For f0: a=95, b=103', side = 3, line = 0)

a <- 98
L <- seq(a,b,0.1)
y <- f0(L,a,b)

plot(L,y,type='l',xlim=c(90,110), ylim = c(0,1))
mtext('change a=98 (keep b=103)', side = 3, line = 0)


a <- 99
L <- seq(a,b,0.1)
y <- f0(L,a,b)
plot(L,y,type='l',xlim=c(90,110), ylim = c(0,1))
mtext('change a=99 (keep b=103)', side = 3, line = 0)

a <- 90
L <- seq(a,b,0.1)
y <- f0(L,a,b)
plot(L,y,type='l',xlim=c(90,110), ylim = c(0,1))
mtext('change a=90 (keep b=103)', side = 3, line = 0)

f <- function(x){x^2}
x1 <- 3
x2 <- 3.00001
slope = (f(x2)-f(x1))/(x2-x1)
slope


f <- function(x){x^2}
x1 <- 3
x2 <- c(4,3.1, 3.01, 3.001, 3.0001, 3.00001)
slope = (f(x2)-f(x1))/(x2-x1)
data.frame(x1 = x1, x2=x2, slope = slope)



#Define the functions.
f1 <- function(x){x^4 -10*x^2 +3*x}
f2 <- function(x){exp(2*x)-1}
f3 <- function(x){sign(x-1)*(abs(x-1))^(1/3)}
f4 <- function(x){3*log(x-2)}

#Graph the functions.
x <- seq(-5,5,1e-3)

###figure 1###
y1 <- f1(x)
par(mfrow=c(2,2),mar=c(2,2,0.25,0.25))
plot(x,y1,type='l',xlim=c(-4,4))
points(3,f1(3),pch=16,col=2)
plot(x,y1,type='l',xlim=c(3-1,3+1),ylim=c(-50,50))
points(3,f1(3),pch=16,col=2)
plot(x,y1,type='l',xlim=c(2.75,3.25),ylim=c(-12.5,12.5))
points(3,f1(3),pch=16,col=2)
plot(x,y1,type='l',xlim=c(2.9,3.1),ylim=c(-5,5))
points(3,f1(3),pch=16,col=2)


###figure 2###
y2 <- f2(x)
par(mfrow=c(2,2),mar=c(2,2,0.25,0.25))
plot(x,y2,type='l',xlim=c(-5,5))
points(0,f2(0),pch=16,col=2)
plot(x,y2,type='l',xlim=c(-1,1),ylim=c(-5,5))
points(0,f2(0),pch=16,col=2)
plot(x,y2,type='l',xlim=c(-0.5,0.5),ylim=c(-2.5,2.5))
points(0,f2(0),pch=16,col=2)
plot(x,y2,type='l',xlim=c(-0.01,0.01),ylim=c(-.05,.05))
points(0,f2(0),pch=16,col=2)

###figure 3###
x3 <- seq(0,5,1e-3)
y3 <- f3(x3)
par(mfrow=c(2,2),mar=c(2,2,0.25,0.25))
plot(x3,y3,type='l',xlim=c(0,5), ylim=c(-2.5,2.5) )
points(2,f3(2),pch=16,col=2)
plot(x3,y3,type='l',xlim=c(1,3),ylim=c(0,2))
points(2,f3(2),pch=16,col=2)
plot(x3,y3,type='l',xlim=c(1.5,2.5),ylim=c(0.5,1.5))
points(2,f3(2),pch=16,col=2)
plot(x3,y3,type='l',xlim=c(1.99,2.01),ylim=c(0.99,1.01))
points(2,f3(2),pch=16,col=2)

###figure 4###
x4 <- seq(2,10,1e-3)
y4 <- f4(x4)
par(mfrow=c(2,2),mar=c(2,2,0.25,0.25))
plot(x4,y4,type='l',xlim=c(0,10), ylim = c(-5,5))
points(2.75,f4(2.75),pch=16,col=2)
plot(x4,y4,type='l',xlim=c(1.75,3.75),ylim=c(f4(2.75)-1,f4(2.75)+1))
points(2.75,f4(2.75),pch=16,col=2)
plot(x4,y4,type='l',xlim=c(2.5,3),ylim=c(f4(2.75)-0.25,f4(2.75)+0.25))
points(2.75,f4(2.75),pch=16,col=2)
plot(x4,y4,type='l',xlim=c(2.7,2.8),ylim=c(f4(2.75)-0.05,f4(2.75)+0.05))
points(2.75,f4(2.75),pch=16,col=2)




#Define the functions.
f1 <- function(x){x^4 -10*x^2 +3*x}
f2 <- function(x){exp(2*x)-1}
f3 <- function(x){sign(x-1)*(abs(x-1))^(1/3)}
f4 <- function(x){3*log(x-2)}

#Create a function called zooming_in that will generate a plot and 3 zoomed in versions.
zooming_in <- function(f, xvalue, xwidth=10, ywidth=10, zoom = c(0.1, 0.01, 0.001)){
  yvalue <- f(xvalue)
  
  #Create sequence of values for plots
  x <- seq(xvalue - xwidth/2, xvalue + xwidth/2, xwidth/100)
  y <- f(x)
  
  #Generate original plot, and then zoom in 3 times by factors given by zoom
  par(mfrow=c(2,2),mar=c(2,2,0.25,0.25))
  plot(x,y,type='l',xlim=xvalue + c(-1,1)*xwidth/2, ylim = yvalue + c(-1,1)*ywidth/2)
  points(xvalue,yvalue,pch=16,col=2)
  plot(x,y,type='l',xlim=xvalue + c(-1,1)*xwidth/2*zoom[1],ylim=yvalue + c(-1,1)*ywidth/2*zoom[1])
  points(xvalue,yvalue,pch=16,col=2)
  plot(x,y,type='l',xlim=xvalue + c(-1,1)*xwidth/2*zoom[2],ylim=yvalue + c(-1,1)*ywidth/2*zoom[2])
  points(xvalue,yvalue,pch=16,col=2)
  plot(x,y,type='l',xlim=xvalue + c(-1,1)*xwidth/2*zoom[3],ylim=yvalue + c(-1,1)*ywidth/2*zoom[3])
  points(xvalue,yvalue,pch=16,col=2)
}

#Use zooming_in() to plot each of the 4 original functions.
zooming_in(f1,3,10,400)
zooming_in(f2,0)
zooming_in(f3,2)
zooming_in(f4,2.75)

f5 <- function(x){abs(x)}
zooming_in(f5,0.1,xwidth=50, ywidth=50,zoom = c(0.001, 0.0001, 0.00001))



f1 <- function(x){x^4 -10*x^2 +3*x}
x <- c(2, 2.5, 2.9, 2.99, 2.999, 3, 3.001, 3.01, 3.1, 3.5, 4)
y <- f1(x)
slope <- (f1(x)-f1(3))/(x-3)
data.frame(x=x,y=y,slope=slope)



#Define the linearization.
t <- function(x,a,fa,df){
  fa + df*(x - a)
}



#Define the linearization.
t <- function(x,a,fa,df){
  fa + df*(x - a)
}


#Visualize these linearizations.
x <- seq(-5,5,1e-3)

###figure 1###
m1 <- 51
  par(mfrow=c(1,2),mar=c(2,2,0.25,0.25))
plot(x,f1(x),type='l',xlim=c(-4,4))
points(3,f1(3),pch=16,col=2)
lines(x,t(x,3,f1(3),m1),col=3)
plot(x,f1(x),type='l',xlim=c(2.9,3.1),ylim=c(-50,50))
points(3,f1(3),pch=16,col=2)
lines(x,t(x,3,f1(3),m1),col=3)




f2 <- function(x){exp(2*x)-1}
x <- c(-1,-.1,-.01,-.001, 0, .001,.01,.1, 1)
y <- f2(x)
slope <- (f2(x)-f2(0))/(x-0)
data.frame(x=x,y=y,slope=slope)



f3 <- function(x){(x-1)^(1/3)}
x <- c(-1,-.1,-.01,-.001, 0, .001,.01,.1, 1)+2
y <- f3(x)
slope <- (f3(x)-f3(2))/(x-2)
data.frame(x=x,y=y,slope=slope)

f4 <- function(x){3*log(x-2)}
x <- c(-1,-.1,-.01,-.001, 0, .001,.01,.1, 1)+2.75
y <- f4(x)
slope <- (f4(x)-f4(2.75))/(x-2.75)
data.frame(x=x,y=y,slope=slope)






#Define the linearization.
t <- function(x,a,fa,df){
  fa + df*(x - a)
}

#Visualize these linearizations.
x <- seq(-5,5,1e-3)

###figure 1###
m1 <- 51
  par(mfrow=c(1,2),mar=c(2,2,0.25,0.25))
plot(x,f1(x),type='l',xlim=c(-4,4))
points(3,f1(3),pch=16,col=2)
lines(x,t(x,3,f1(3),m1),col=3)
plot(x,f1(x),type='l',xlim=c(2.9,3.1),ylim=c(-50,50))
points(3,f1(3),pch=16,col=2)
lines(x,t(x,3,f1(3),m1),col=3)


###figure 2###
m2 <- 2
  par(mfrow=c(1,2),mar=c(2,2,0.25,0.25))
plot(x,f2(x),type='l',xlim=c(-5,5))
points(0,f2(0),pch=16,col=2)
lines(x,t(x,0,f2(0),m2),col=3)
plot(x,f2(x),type='l',xlim=c(-0.01,0.01),ylim=c(-0.25,0.25))
points(0,f2(0),pch=16,col=2)
lines(x,t(x,0,f2(0),m2),col=3)

###figure 3###
x3 <- seq(0,5,1e-3)
m3 <- .3333333333
  par(mfrow=c(1,2),mar=c(2,2,0.25,0.25))
plot(x3,f3(x3),type='l',xlim=c(-5,5))
points(2,f3(2),pch=16,col=2)
lines(x3,t(x3,2,f3(2),m3),col=3)
plot(x3,f3(x3),type='l',xlim=c(1.99,2.01),ylim=c(0.99,1.01))
points(2,f3(2),pch=16,col=2)
lines(x3,t(x3,2,f3(2),m3),col=3)

###figure 4###
x4 <- seq(2,10,1e-3)
m4 <- 4
  par(mfrow=c(1,2),mar=c(2,2,0.25,0.25))
plot(x4,f4(x4),type='l',xlim=c(0,10))
points(2.75,f4(2.75),pch=16,col=2)
lines(x4,t(x4,2.75,f4(2.75),m4),col=3)
plot(x4,f4(x4),type='l',xlim=c(2.7,2.8),ylim=c(-2,1))
points(2.75,f4(2.75),pch=16,col=2)
lines(x4,t(x4,2.75,f4(2.75),m4),col=3)


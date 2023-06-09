f <- function(x){x*exp(-x)}
Df <- function(x){1*exp(-x) - x*(exp(-x))}
D2f <- function(x){-2*exp(-x) + x*exp(-x)}

plot(f,0,10)
x <- seq(0,10,0.1)
plot(x,f(x), type = "l")

uniroot(Df,c(0,100))$root
uniroot(Df,c(0,2))$root

Df(1)
D2f(1)
#Since D2f(1) is negative and Df(1)=0, we have a maximum at x=1



uniroot(Df,c(-10,10))$root
cv <- uniroot(Df,c(-10,10))$root
Df(cv) #Check we get Df = 0
Df(1) #Uniroot gives approximates, so we may need to round.

D2f(1)
D2f(cv)
#OR
-2*exp(-1) + 1*exp(-1)


x <- seq(-10,10,0.001)
par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f(x),type = "l")


x <- seq(-1,10,0.001)
par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f(x),type = "l",xlim=c(-1,10),ylim=c(-3,1))


x <- seq(0,10,0.001)
par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f(x),type = "l",xlim=c(0,10),ylim=c(0,0.5))


my_plot <- function(f,left_bound,right_bound,gap = (right_bound-left_bound)/100, mar = c(2.5,2.5,0.25,0.25), type = "l",... ){
  x <- seq(left_bound,right_bound,gap)
  par(mar=mar)
  plot(x,f(x),type = type,...)
}
my_lines <- function(f,left_bound,right_bound,gap = (right_bound-left_bound)/100, type = "l",... ){
  x <- seq(left_bound,right_bound,gap)
  lines(x,f(x),type = type,...)
}

par(mfrow=c(2,3))
my_plot(f,-10,10,0.001) #We specify plotting another point every 0.0001. 
my_plot(f,-10,10) #The default in our custom function uses 101 points. 
my_plot(f,-10,10,2) #We specify plotting another point every 2. 
my_plot(f,-1,10)
my_plot(f,-1,10,ylim=c(-3,1))
my_plot(f,0,10,ylim=c(0,0.5))



a<-0
b<-10
par(mfrow=c(1,1))
my_plot(f,a,b,ylim=c(-0.5,0.5))
my_lines(Df,a,b,col = "red")
abline(h=0, lty = 2)
my_lines(D2f,a,b, col = "green")
abline(v=cv,col="blue",lty = 2)
points(cv,f(cv))
points(cv,Df(cv),col="red")
points(cv,D2f(cv),col="green")
legend(6, -.2, legend=c("f", "f\'", "f\'\'"),
       col=c("black","red", "green"), lty=1, cex=0.8)






g <- function(x){x*(1-x)}
Dg <- function(x){1-2*x}
D2g <- function(x){0*x-2} #Is the zero important?

uniroot(Dg,c(-100,100000))$root
plot(g,-10,10)
abline(v=1/2)
D2g(1/2)


h <- function(x){x^3-x}
Dh <- function(x){3*x^2-1}
D2h <- function(x){6*x}

uniroot(Dh,c(-100,100000))$root
plot(h,-2,2)

cv <- uniroot(Dh,c(-1,0))$root
cv
D2h(cv)

cv <- uniroot(Dh,c(0,1000000))$root
cv
D2h(cv)

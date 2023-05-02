log(17, base = 2)
2^4.0875

log(1)
log(2)
log(2, exp(1))
ln(2)



fun.1 <- function(x){ 4*2^x }
fun.2 <- function(x){ (1/2)^x - 2 }
fun.3 <- function(x){ (1/2)*3^(x-1) }
fun.4a <- function(x){ 2^(3-x)+5 }
fun.4b <- function(x){ (1/2)^(x-3)+5 }


x <- seq(-5,5,0.1)
y <- fun.1(x)
par(mar=c(2.5,2.5,1,0.25))
plot(x,y,type="l",ylim=c(-10,30))
abline(h=0,col='gray',lty=3)
abline(v=0,col='gray',lty=3)

x <- seq(-5,5,0.1)
y <- fun.2(x)
plot(x,y,type="l",ylim=c(-10,30))
abline(h=0,col='gray',lty=3)
abline(v=0,col='gray',lty=3)
abline(h=-2,col='red',lty=3)

x <- seq(-5,5,0.1)
y <- fun.3(x)
plot(x,y,type="l",ylim=c(-10,30))
abline(h=0,col='gray',lty=3)
abline(v=0,col='gray',lty=3)
abline(h=0,col='red',lty=3)

x <- seq(-5,5,0.1)
y <- fun.4a(x)
plot(x,y,type="l",ylim=c(-10,30))
abline(h=0,col='gray',lty=3)
abline(v=0,col='gray',lty=3)
abline(h=5,col='red',lty=3)

x <- seq(-5,5,0.1)
y <- fun.4b(x)
plot(x,y,type="l",ylim=c(-10,30))
abline(h=0,col='gray',lty=3)
abline(v=0,col='gray',lty=3)
abline(h=5,col='red',lty=3)


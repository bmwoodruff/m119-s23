draw_rect_approx <- function(f,a,b,num_rectangles, method = "mid"){
  n <- num_rectangles
  dx <- (b-a)/n
  x <- c(a,seq(a,b,dx/100),b,a)
  y <- c(0,f(seq(a,b,dx/100)),0,0)
  par(mar=c(2.5,2.5,0.25,0.25))
  plot(x,y,type = "l")
  
  if(method == "left"){
    xi <- seq(a+0*dx/2,b-dx/2,dx)
    lines(xi,f(xi),type = "h")
    lines(xi,f(xi),type = "s")
    lines(c(xi[n],xi[n]+dx),f(c(xi[n],xi[n])),type = "l")
    lines(c(xi[n],xi[n]+dx),f(c(xi[n],xi[n])),type = "h")
  }
  else if(method == "right"){
    xi <- seq(a+dx,b+dx/2,dx)
    lines(xi-dx,f(xi),type = "h")
    lines(xi-dx,f(xi),type = "s")
    lines(c(xi[n]-dx,xi[n]),f(c(xi[n],xi[n])),type = "l")
    lines(c(xi[n]-dx,xi[n]),f(c(xi[n],xi[n])),type = "h")
  } 
  else{#Use midpoint
    xi <- seq(a+dx/2,b,dx)
    lines(xi-dx/2,f(xi),type = "h")
    lines(xi-dx/2,f(xi),type = "s")
    lines(c(xi[n]-dx/2,xi[n]+dx/2),f(c(xi[n],xi[n])),type = "l")
    lines(c(xi[n]-dx/2,xi[n]+dx/2),f(c(xi[n],xi[n])),type = "h")
  }
}

g <- function(x){x^2*exp(-x)}
a <- 1
b <- 4
n <- 6
draw_rect_approx(g,a,b,n,method = "right")
dx <- (b-a)/n
xi <- seq(a+dx,b,dx)  #Can you tell what part of this line of code has us use right endpoints?
data.frame(right_point = xi, function_at_xi = g(xi), area_i = g(xi)*dx)
c(riemann_sum_using_right_endpoints = sum(g(xi)*dx))
n <- 10000000
dx <- (b-a)/n
xi <- seq(a+dx,b,dx)  
A <- sum(g(xi)*dx)
A
k <- 1/A
k
f <- function(x){k*g(x)}
a <- 1
b <- 4
n <- 6
draw_rect_approx(f,a,b,n,method = "right")
dx <- (b-a)/n
xi <- seq(a+dx,b,dx)  #Can you tell what part of this line of code has us use right endpoints?
data.frame(right_point = xi, function_at_xi = g(xi), area_i = g(xi)*dx)
c(riemann_sum_using_right_endpoints = sum(f(xi)*dx))
n <- 10000000
dx <- (b-a)/n
xi <- seq(a+dx,b,dx)  
A <- sum(f(xi)*dx)
A
EV <- sum(xi*f(xi)*dx)
EV
Var <- sum((xi-EV)^2*f(xi)*dx)
Var


a <- 1
b <- 3 #We adjust the upper bound to be 3, instead of 4. 
n <- 10000000
dx <- (b-a)/n
xi <- seq(a+dx,b,dx)  
sum(f(xi)*dx)


a <- 2
b <- 3 #We adjust the upper bound to be 3, instead of 4. 
n <- 10000000
dx <- (b-a)/n
xi <- seq(a+dx,b,dx)  
sum(f(xi)*dx)






g <- function(x){sqrt(49-x^2)}
a <- 0
b <- 7
n <- 20000000
dx <- (b-a)/n
xi <- seq(a+dx,b,dx)  
A <- sum(g(xi)*dx)
A
k <- 1/A
k
f <- function(x){k*g(x)}
dx <- (b-a)/n
xi <- seq(a+dx,b,dx)  
A <- sum(f(xi)*dx)
A
EV <- sum(xi*f(xi)*dx)
EV
Var <- sum((xi-EV)^2*f(xi)*dx)
Var






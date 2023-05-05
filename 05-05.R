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
  0
}

uniroot(function(x){f(x)-rhs(x)},c(1,5))$root
x <-seq(0.1,3,.1)
plot(x,f(x), type="l")
lines(x,rhs(x), type="l", col = 2)



f <- function(x){log(x)+log(x-1)-log(6)}

x <- seq(1,5,0.1)
plot(x, f(x), type = "l")
abline(h=0)
abline(v=3)

f(3)
uniroot(f, c(2,4))




f <- function(x){3^x-17}
interval <- c(2,3)
my_root <- uniroot(f, interval)$root
my_root
#The code below produces a plot that illustrates what uniroot found. 
x<-seq(interval[1],interval[2],0.1)
plot(x,f(x),type="l")
abline(h=0,col = "gray")
abline(v=my_root,col = "gray")
points(my_root,0, col = "red")



rm(list=ls()) #Clears the environment

h <- function(x){
  3*x -15
}

#What does the $root code do below?
uniroot(h,c(0,30))
uniroot(h,c(0,30))$root
x <-seq(0,30,1)
plot(x,h(x), type="l")
abline(h=0, col = "lightgray")



h.shift <- function(x){
  h(x) - 4
}

uniroot(h.shift,c(0,5))$root

uniroot(h.shift,c(0,10))$root



g <- function(x){
  3*x-15-exp(-x+6)
}

uniroot(g,c(0,6))$root

x <-seq(0,30,1)
plot(x,g(x), type="l")
abline(h=0, col = "lightgray")



f <- function(x){
  1/x
}

uniroot(f,c(-10,-3))$root

uniroot(f,c(-1,1))$root

uniroot(f,c(-1,1))


rm(list=ls()) #Clears the environment

##############
##############
h <- function(x){
  3*x -5
}
uniroot(h,c(0,30))$root
x <-seq(0,30,1)
plot(x,h(x), type="l")
abline(h=0, col = "lightgray")


##############
##############
h.shift <- function(x){
  h(x)-7
}
uniroot(h.shift,c(0,30))$root

##############
##############
solve_me <- function(x){
  h(x)-exp(-x)
}
uniroot(solve_me,c(0,3))$root

solve_me <- function(x){
  h(x)-log(x)
}
uniroot(solve_me,c(1,2))$root
x <-seq(0,5,0.01)
plot(x,solve_me(x), type="l")
abline(h=0, col = "lightgray")


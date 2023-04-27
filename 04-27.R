k <- seq(4,8)
k
5-k
sum(5-k)

sum(5-4:8)


n <- seq(1,4)
prod(n)
factorial(4)
factorial(10)
n <- seq(1,10)
prod(n)

i <- seq(3,7)
2+i^2
1/3*sum(2+i^2)



f <- function(x){
  ifelse(x < -2, -x-4, 
         ifelse(x <= 2, -2, 
                x-4))
}

x <- seq(-5,5, 0.01)
plot(x,f(x), type = "l")

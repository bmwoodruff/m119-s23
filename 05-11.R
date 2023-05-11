p <- function(x,lambda=2){
  # x must be a whole number
  (lambda^x/factorial(x))*exp(-lambda)
}


p.3v1 <- function(x,lambda=2){
  # each element of x must be a whole number
  prod((lambda^x/factorial(x))*exp(-lambda))
}

p.3v2 <- function(x1,x2,x3,lambda=2){
  # x1, x2, and x3 must be whole numbers
  (lambda^(x1+x2+x3)/(factorial(x1)*factorial(x2)*factorial(x3)))*exp(-3*lambda)
}


lam <- 2
p(8,lam)*p(2,lam)*p(8,lam)*p(8,lam)*p(4,lam)

lam <- seq(0,10, 0.1)
likelihoodvalues <- p(8,lam)*p(2,lam)*p(8,lam)*p(8,lam)*p(4,lam)

plot(lam, likelihoodvalues, type = "l")

p.3v1( x =c(8,2,8,8,4), lambda = 2 )
lam <- seq(0,10, 0.1)
p.3v1( x = c(8,2,8,8,4), lambda = lam )

lam <- seq(0,10, 0.1)
y <- sapply(lam, p.3v1, x = c(8,2,8,8,4))
plot(lam, y, type = "l")

lam <- seq(4.71,4.73, 0.00001)
y <- sapply(lam, p.3v1, x = c(8,6,4,3,2,4,6))
plot(lam, y, type = "l")

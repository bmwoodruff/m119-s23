data <- read.csv(url("https://byuistats.github.io/M119/logLikelihood_practice.csv"))

x <- data$x
y <- data$y2

c11 <- sum(x)
c12 <- 50
b1 <- sum(y)
c21 <- sum(x^2)
c22 <- sum(x)
b2 <- sum(y*x)


## Create a function to solve a system of equations. 
solvesystem <- function(c11, c12,b1,c21,c22,b2){ 
  c((b1*c22 - c12*b2)/(c11*c22 - c21*c12),
    (c11*b2 - b1*c21)/(c11*c22 - c21*c12))
}


sol <- solvesystem(c11, c12, b1, c21, c22, b2)
sol
best_m <- sol[1] 
best_b <- sol[2] 


h <- function(x, m = best_m, b = best_b){m*x+b}

xvalues <- seq(-5,5,0.1)
plot(x,y)
lines(xvalues,h(xvalues))


#We now test to see that we found a max. 
fxx <- -c12
fyy <- -c21
fxy <- -c11
D <- fxx*fyy-fxy^2
D
#D>0 So we test fxx to see if concave up or down.
fxx
#fxx <0 so we are concave down, hence rainbow, or max. 



#Let's examine the likelihood function
#We did so in Mathematica.  Check the github repo for a .nb file with today's date.
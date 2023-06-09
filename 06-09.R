rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data3_ls.csv"))
x <- data$x
y <- data$y

plot(x,y)

b <- sum(y*exp(-x))
d <- sum(exp(-x)^2)

best_a <- b/d

f <- function(x,a = best_a){a*exp(-x)}
x_values <- seq(-2,4,0.01)
plot(x,y)
lines(x_values,f(x_values))

#is the second derivative posititve?
2*sum(exp(-x)^2)
2*sum(exp(-x)^2)>0
#Yep.  So we have found a minimum of the sum of the squared errors. 




##Model 1
##Data Set 1
rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data1_ls.csv"))
x <- data$x
y <- data$y
plot(x,y)

best_m <- sum(y*x)/sum(x^2)
best_m

f <- function(x,m = best_m){m*x}
xv <- seq(-5,5,1)
plot(x,y)
lines(xv,f(xv))


data <- read.csv(url("https://byuistats.github.io/M119/data2_ls.csv"))
x <- data$x
y <- data$y
best_m <- sum(y*x)/sum(x^2)
f <- function(x,m = best_m){m*x}
plot(x,y)
lines(xv,f(xv))


rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data5_ls.csv"))
x <- data$x
y <- data$y

best_a <- sum(y*log(x))/sum(log(x)^2)

f <- function(x,a = best_a){a * log(x)}
plot(x,y)
xv <- seq(0,5,0.1)
lines(xv, f(xv))


f(4)
f(1)
f(2)
f(1.5)
f(1.538)
solve_me <- function(x){f(x)-5}
uniroot(solve_me, c(1,2))$root

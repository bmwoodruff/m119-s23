p <- function(x,lambda=2){
  # x must be a whole number
  (lambda^x/factorial(x))*exp(-lambda)
}

p(1)
p(1,lambda = 2)
p(1,lambda = 1)
p(1,1)
p(0,lambda = 1)


## P(Y>1)
p(2,1)+p(3,1)+p(4,1)+p(5,1)+p(6,1)+p(7,1)+p(8,1)+p(9,1)+p(10,1)+p(11,1)
p(seq(2,20),lambda = 1)
sum(p(seq(2,1000),lambda = 1))

sum(p(seq(0,1000),lambda = 1))

1-p(0,1)-p(1,1)

#P(Y>=6)
1-sum(p(seq(0,5),1))


# The probability of less than 8 Florida tropical storms this year using lambda = 3
p(seq(0,7),3)
sum(p(seq(0,7),3))
# The probability of less than 8 Florida tropical storms this year using lambda = 6
sum(p(seq(0,7),6))
# The probability of at least 8 Florida tropical storms this year using lambda = 6
1-sum(p(seq(0,7),6))
sum(p(seq(8,100),6))
# The probability of more than 12 Florida tropical storms this year using lambda = 5
sum(p(seq(13,1000),5)) ##UHOH.  NaN is definitely NOT correct.
sum(p(seq(13,100),5))
1-sum(p(seq(0,12),5))


###Stuff well discuss soon. 
# Fix lambda, change x. Create a plot.
x <- seq(0,15,1)
plot(x,p(x,lambda = 5))

# Fix x, change lambda. Create a plot.
lam <- seq(0,10,0.01)
plot(lam,p(x = 7,lam), type = "l")





p.3v1 <- function(x,lambda=2){
  # each element of x must be a whole number
  prod((lambda^x/factorial(x))*exp(-lambda))
}

p.3v2 <- function(x1,x2,x3,lambda=2){
  # x1, x2, and x3 must be whole numbers
  (lambda^(x1+x2+x3)/(factorial(x1)*factorial(x2)*factorial(x3)))*exp(-3*lambda)
}


#The probability of 4 Florida tropical storms this year, 4 Florida tropical storms next year, and 8 Florida tropical storms the year after (using $\lambda = 2$ as assumed).
p.3v1(c(4,4,8))


#The probability of 4 Florida tropical storms this year, 4 Florida tropical storms next year, and 8 Florida tropical storms the year after, assuming the parameter $\lambda = 5$.
p.3v1(c(4,4,8),5)

#Repeat the above, but with lambda = 1, and then with lambda = 10
p.3v1(c(4,4,8),1)
p.3v1(c(4,4,8),10)

p.3v1(c(4,4,8),4)
p.3v1(c(4,4,8),5)
p.3v1(c(4,4,8),6)
p.3v1(c(4,4,8),5.5)
p.3v1(c(4,4,8),5.3333333)


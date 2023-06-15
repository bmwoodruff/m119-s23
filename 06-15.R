#Shades a rug diagram for a probability mass function. 
#Inputs: 
#  x - a vector of data points
#  p - a corresponding vector of probabilities or frequencies
#All widths are 1 unit wide. 
draw_pmf <- function(x,p){
  xs <- c(rbind(x-1/2,x-1/2,x+1/2,x+1/2))
  px <- c(rbind(0,p,p,0))
  par(mar=c(2.5,2.5,0.25,0.25))
  plot.new()
  plot(xs,px,type="l")
  polygon(xs,px,col="gray")
}

x <- c(  1,  2,  3,  4)
p <- c(0.1,0.3,0.3,0.3)
draw_pmf(x,p)

sum(x*p)



#Shades a rug diagram (shades area under) for a function f from a to b. 
#Inputs: 
#  f - a function f(x)
#  a - left end of the rug
#  b - right end of the rug
#  num_points - how many point are sent into f for plotting. 
draw_rug <- function(f,a,b,num_points=100){
  x <- c(a,seq(a,b,(b-a)/num_points),b,a)
  y <- c(0,f(seq(a,b,(b-a)/num_points)),0,0)
  par(mar=c(2.5,2.5,0.25,0.25))
  plot(x,y,type = "l")
  polygon(x,y,col="gray")
}

g <- function(x){5+x*0}
draw_rug(g,0,6)


g <- function(x){2*x}
draw_rug(g,0,6)

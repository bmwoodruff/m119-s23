solve_system <- function(b11,b12,c1,b21,b22,c2){
  x <- (c1*b22-b12*c2)/(b11*b22-b12*b21)
  y <- (b11*c2-c1*b21)/(b11*b22-b12*b21)
  c(x,y)
}

solution <- solve_system(2,3,4,5,6,7)
solution
solution[1]
solution[2]

i <- seq(1,3)
solve_system(
  pi,
  log(2),
  7,
  sum(i^2),
  sum(i-1),
  sum(i-i^2)
)

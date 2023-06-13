x <- seq(2,12)
p <- c(1/36,2/36,3/36,4/36,5/36,6/36,5/36,4/36,3/36,2/36,1/36)
tbl <- data.frame(outcome=x, prob = p)
tbl
knitr::kable(tbl,  align = "c")


sum(x*p)


x <- seq(2,13)
p <- c(1/36,2/36,3/36,4/36,5/36,0,5/36,4/36,3/36,2/36,1/36,6/36)
tbl <- data.frame(outcome=x, prob = p)
tbl
sum(x*p)

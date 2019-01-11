#length comp sample size
d2n <- 1.27359 #Thompson 1987 Table 1, alpha = 0.05
d2n <- 1.00635
n <- d2n/c(.05, .075, .08)^2 # d
n
d <- sqrt(d2n/c(300, 450, 900, 1350, 1800))
d



RR <- function(D, A = 0.5, alpha = 0.05) abs(pnorm(A*sqrt(D)/(1-A)) - pnorm(-A*sqrt(D)/(1+A)) - (1 - alpha))

grid <- expand.grid(list(n = c(300, 500, 700, 900, 1350, 1800), N = c(1000, 1500, 3000, 6000, 9000))); 
grid$D <- round(mapply(function(n, N) n^2*(N-1)/(N-n)^2, grid[, 1], grid[, 2]), 1)
grid$A <- round(sapply(grid$D, function(x) optimize(RR, c(0, 1), D = x, alpha = 0.05)[[1]]), 3)
grid[grid$D < 900, ]

#Total sample size requirements for +/- 10%
optimize(RR, c(2, 500), A = 0.10)
round(seq(1000, 9000, 500)/(sqrt((seq(1000, 9000, 500) - 1)/391.449) + 1), 0)*2

#Total sample size requirements for +/- 15%
optimize(RR, c(2, 500), A = 0.15)
round(seq(1000, 9000, 500)/(sqrt((seq(1000, 9000, 500) - 1)/178.31) + 1), 0)*2

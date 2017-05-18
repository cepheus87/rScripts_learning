# for one dimensional data package 
library("Ckmeans.1d.dp")

x <- rnorm(25, mean = 1, sd = 0.3 )
x <- c(x, rnorm(25, mean = 4, sd = 1))

km_out <- Ckmeans.1d.dp(x, k = 2)

km_out

# uwaga na kolorowanie
plot(x[1:35], col = km_out$cluster)
plot(x[5:40], col = km_out$cluster)

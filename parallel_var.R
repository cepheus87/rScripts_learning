# parallel variance calculation
# https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm

x <- NULL

samp <-1000
no <- 5

for(i in 1:no){
  x <- c(x, rnorm(mean = i, sd = (1+i*0.1), n = samp))
}

var(x)
mean(x)

plot(density(x))

threads <- 8

sampNo <- samp*no/ threads

if(sampNo != as.integer(sampNo)){
  warning("not all data will be included")
}

?sample

li <- list()
vars <- NULL
for(thr in 1:threads){
  li[[thr]] <- sample(x, sampNo)
  vars <- c(vars, var(li[[thr]]))
}

# length(li[[1]])

vars

length(li)

# parallelVar <- function(meanA, countA, varA, meanB, countB, varB){
parallelVar <- function(a, b){
  
  countA <- length(a)
  countB <- length(b)
  delta <- mean(b) - mean(a)
  mA <- var(a) * (countA-1)
  mB <- var(b) * (countB-1)
  M <- mA + mB + (delta^2 * countA * countB) / (countA + countB)
  return(M / (countA + countB - 1))
}

var(c(li[[1]], li[[2]]))

parallelVar(li[[1]], li[[2]])




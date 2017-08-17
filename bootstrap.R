# bootstrap

# http://www.statmethods.net/advstats/bootstrapping.html

A <- rnorm(100)
B <- rnorm(100, mean = 5)

quantile(A, 0.95)


getQuantile <- function(data, index, qProb = 0.95){
  #  print(length(index))
  return(quantile(data[index], qProb))
}

getQuantile(A, 20:100)

library(boot)
?boot

# boot( ) calls the statistic function R times. Each time, it generates a set of random indices, with replacement, from the integers 1:nrow(data). 
# These indices are used within the statistic function to select a sample. The statistics are calculated on the sample and the results are 
# accumulated in the bootobject. The bootobject structure includes
# 
# element 	description
# t0 	The observed values of k statistics applied to the orginal data.
# t 	An R x k matrix where each row is a bootstrap replicate of the k statistics. 


bootObj <- boot(A, getQuantile, R=100, qProb = 0.95)

# with Repetitions bias getting lower

bootObj

bootObj$t0
bootObj$t

bootObj
sd(bootObj$t)

# to examine
plot(bootObj)


# you can use boot.ci( ) function to obtain confidence intervals for the statistic
# boot.ci(bootobject, conf=, type= ) where
# 
# parameter 	description
# bootobject 	The object returned by the boot function
# conf 	The desired confidence interval (default: conf=0.95)
# type 	The type of confidence interval returned. Possible values are "norm", "basic", "stud", "perc", "bca" and "all" (default: type="all")
# 

boot.ci(bootObj)

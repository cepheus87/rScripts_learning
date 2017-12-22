


element	wise mean over list of matrices


 A <- matrix(c(1:9), 3, 3) 
> A
     [,1] [,2] [,3]
[1,]    1    4    7
[2,]    2    5    8
[3,]    3    6    9
> B <- matrix(c(2:10), 3, 3) 
> B
     [,1] [,2] [,3]
[1,]    2    5    8
[2,]    3    6    9
[3,]    4    7   10
> my.list <- list(A, B)

Reduce("+", my.list) / length(my.list)

#legend

x<- rnorm(20)
y <- rnorm(10, mean = 5)



plot(c(x,y), col = c(1,2)  )
legend(1, 6, c("text1", "text2"), lty = c(1,1), col = c(1,2) )

?legend

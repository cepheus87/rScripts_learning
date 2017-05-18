# kMeans  - 2 dim

# for one dimensional data package 
#library("Ckmeans.1d.dp")

set.seed(2)
x<-matrix(rnorm(50*2), ncol = 2)
x[1:25, 1]<- x[1:25, 1] + 3
x[1:25, 2]<- x[1:25, 2] - 4 

plot(x[1:25, ], xlim = c(-5, 10), ylim = c(-10, 5), pch = 20)
points(x[26:50, ], col = "red", pch = 20)

?kmeans 

# centers - jezeli liczba to centa sa losowane przypadkowo, mozna tez podac ich polozenia poczatkowe
# nstart - jezeli centra sa liczba, to ile przypadkowych centrow ma byc sprawdzane, podawany jest najlepszy wynik
km_out <- kmeans(x, centers =  2, nstart = 20 )   

str(km_out)
km_out$cluster
km_out$centers


plot(x, col = (km_out$cluster ), cex = 2)
points(x, col = (km_out$cluster ), cex = 2)


set.seed(4)
km_out3 <- kmeans(x, centers =  3, nstart = 20 )   

km_out3
points(x, col = (km_out3$cluster + 6 ), cex = 1)


# dla roznych nstart
set.seed(3)
km_out_n1 <- kmeans(x, 3, nstart = 1)
km_out_n20 <- kmeans(x, 3, nstart = 20)

#tot.withinss - suma kwadratow dla wszystkich klastrow
#withinss - suma kwadratow dla poszczegolnych klastrow

km_out_n1$withinss
km_out_n1$tot.withinss

km_out_n20$withinss
km_out_n20$tot.withinss

plot(x, col = km_out_n1$cluster, pch = 20)
points(x, col = (km_out_n20$cluster ), cex = 2)



centrX<-c( -1, 0, 3 )
centrY<-c( 0, 0, -4 )
centrX2<-c( -1, 3, 4 )
centrY2<-c( 0, -4, -4 )
centr<-data.frame(centrX, centrY)
centr2<-data.frame(centrX2, centrY2)

km_out_centr1 <- kmeans(x, centers = centr  )
km_out_centr2 <- kmeans(x, centers = centr2  )


plot(x, col = km_out_centr1$cluster, pch = 20)
points(x, col = (km_out_centr2$cluster + 4) , cex = 2)


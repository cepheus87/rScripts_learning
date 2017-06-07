# covariance, correlation and  principal component analysis (PCA) and svd

# https://stats.stackexchange.com/questions/134282/relationship-between-svd-and-pca-how-to-use-svd-to-perform-pca

# wazny pkt to 3 !!!!!!!!!
# 3a przypadek, gdy wiecej obserwabli niz pomiarow n x p ( p > n ) 

#w pakiecie stats
?prcomp


#correlation 
round(cor(heptathlon[,-score]), 2)


X <- as.matrix(heptathlon[,-score])
#### takie rozkminy #####

n <- nrow(X)   # samples no
p <- ncol(X)    # variables no

C <- ( t(X) %*% X ) / (n-1)    # dziala jak X - X_bar

# cov(X,Y) = cor(X, Y) * sigma_x * sigma_y

cov(X)
cov2cor(( cov(X) ) )
cov2cor(C)

#############




# http://www.cs.otago.ac.nz/cosc453/student_tutorials/principal_components.pdf
# troche zamotane pod koniec z tymi transpozycjami macierzy

#X <- rbind( c(.69, .49), c(-1.31, -1.21), c(.39, .99) )

X <- rbind( c(.69, .49), c(-1.31, -1.21), c(.39, .99), c(.09, .29), c(1.29, 1.09), c(.49, .79), c(.19, -.31), c(-.81, -.81), c(-.31, -.31), c(-.71, -1.01) )
axes <- c("xax", "yax")

X_3col <- cbind(X, round(rnorm(10), 1) )
X_3col[,3] <- X_3col[,3] - mean(X_3col[,3])   # centering
X<- X_3col
axes <- c(axes, "zax")

colnames(X) <- axes


X
plot(X)

#XnotCentered <- t( t(X) + c(1.81, 1.91 ))
#XnotCentered



cor(X)

cov(X)
C <- ( t(X) %*% X ) / (nrow(X)-1)   # covariance
C

C <- ( X %*% t(X) ) / (nrow(X)-1)   # covariance ???
C



#to sa eigenvectors
pca <- prcomp(X)   # dla X_3col rot = 3x3
pca

#pca_notCentData <- prcomp(XnotCentered, center = T, scale = F )
#pca_notCentData

evec <- pca$rotation   # to jest V
#ortogonalne -> evec %*% t(evec) = unit

pc1 <- pca$rotation[,1]
pc2 <- pca$rotation[,2]
pc3 <- pca$rotation[,3]




# values that are (proportional to) eigenvalues simply by taking their squares:
#eigenvalues
ev <- pca$sdev^2
ev


# The eigenvalues above are related to usual 'unbiased' variance so that the following results are approximately equal:
sum(ev)
sum(apply(X, 2, var))

# If you want to get eigenvalues related to biased estimate of variance, you can do
#eb <- (1-1/nrow(X))*ev
#eb



#  C = V L t(V)

diagM<- diag(ev)   # L

evec %*% diagM %*% t(evec)  # equals the cov matr

#princip comp values (in PC space)
# PC = X V
X %*% evec
plot(X %*% evec)

PC <-X %*% evec
PC1 <- X %*% pc1
PC1

#without ver
# PCv <- X %*% evec[,-2]
# PCv

# get back to data  X = PC t(V)
# PC %*% t(evec)  

X1 <- PC1 %*% t(pc1)
X1   # data transformed to 1st pc
plot(X1)

#X1v <- PCv %*% t(evec[,-2])
#X1v


PC2 <- X %*% pc2
X2 <- PC2 %*% t(pc2)


plot(x = X[,1], y = PC1)  #  projection of PC1


plot(X1)
plot(X2)



# rzutowanie PC t(PC) na dane 
#X2new <- X - ( PC1 %*% t(PC1) %*% X)   # powinno byc rownozwazne X2 - nie do konca

cov(X) %*% solve(cov(X))


X2new <- X - ( ( PC1 %*% t(PC1) %*% X)  )
#skalowanie przez  ~cov ^-1
X2newScaled <- X - ( ( PC1 %*% t(PC1) %*% X)  %*% solve(t(X) %*% X) )


#https://en.wikipedia.org/wiki/Principal_component_analysis
X2newWiki <- X -  (X %*% pc1 %*% t(pc1) )    # to dziala - z wiki


######## sprawdzenie liczenie korelacji  wzgledem PCs


PC3 <- X %*% pc3


X1
#X2new <- X2new / (X2new[1,1] / X2[1,1])   # przeskalowanie
#X2new <- X2new / (X2new[1,1] / X2[1,1])   # przeskalowanie
X2new

plot(X2)
plot(X2new, main = "paper")
plot(X2newWiki, main = "wiki")


PC # (10x3)
PC2 # (10x1)





##################

# 3a  !!!! rozkminy, gdy wiecej obserwabli niz pomiarow n x p ( p > n ) 

X2 <- rbind( c(.69, .49), c(-1.31, -1.21), c(.39, .99), c(.09, .29), c(1.29, 1.09), c(.49, .79), c(.19, -.31), c(-.81, -.81), c(-.31, -.31), c(-.71, -1.01) )
X_3col <- cbind(X2, round(rnorm(10), 1) )
X_3col[,3] <- X_3col[,3] - mean(X_3col[,3])
X2<- X_3col
X2 <- t(X2)

#centrowanie
X2<- apply(X2, 2, function(X2){X2 -  mean(X2)} )

X2   #( 3 x 10 )


pca2 <- prcomp(X2, center = T)
pca2 <- prcomp(X2, center = F)
pca2


evec2 <- pca2$rotation  

evec2_1col <- pca2$rotation[,1]
evec2_2col <- pca2$rotation[,2]


evec2 %*% t(evec2)  # (5 x 5) - zle
t(evec2) %*% evec2   # (3 x 3) ~ unit

PC_2 <- X2 %*% evec2
PC_2

PC_2_1 <- X2 %*% evec2_1col
PC_2_1


PC_2_2 <- X2 %*% evec2_2col
PC_2_2


X2_1 <- PC_2_1 %*% t(evec2_1col)
X2_2 <- PC_2_2 %*% t(evec2_2col)


X2_recreated <- PC_2 %*% t(evec2)

X2_2new <- X2 - ( ( PC_2_1 %*% t(PC_2_1) %*% X2)  )
#skalowanie przez  ~cov ^-1
#X2_2newScaled <- X2 - ( ( PC_2_1 %*% t(PC_2_1) %*% X2)  %*% solve(t(X2) %*% X2) )


#https://en.wikipedia.org/wiki/Principal_component_analysis
X2_2newWiki <- X2 -  (X2 %*% evec2_1col %*% t(evec2_1col) )    #




plot(X2_2[1,], X2_2[2,])
plot(X2_2new[1,], X2_2new[2,])
plot(X2_2newWiki[1,], X2_2newWiki[2,])






######################
?svd  # based on lapack

### przypadek nxp gdy n > p 

XtoSVD <- X
XtoSVD <- X2

dec <- svd(XtoSVD)
dec

dec[["v"]] # rownowazne  eigenvectors evec  (principal directions)

dec[["u"]] # * sqrt(ncol(dec[["u"]] -1 ))
dec[["d"]]

U <- dec[["u"]]
V <- dec[["v"]]
D <- diag( dec[["d"]] )
D <-  dec[["d"]] 

U
V
D

n<-ncol(XtoSVD)

# zgadza sie 
wartWl <- (D^2 / (n-1))  
wartWl
pca$sdev^2


PC


#loadings
(V %*% D) / sqrt(n-1)


######## wersja dla danych macierzy nxp gdzie n > p

XtoSVD

PC1_svd <- XtoSVD %*% V[,1] 
PC1_svd
 
X1_svd <- PC1_svd %*% t(PC1_svd) %*% XtoSVD    # z dokladnoscia do przeskalowania
X1_svd / (X1_svd[1,1] / X1[1,1])  

X1_svdMy <- XtoSVD %*% V[,1] %*% t(V[,1])

X1_svdMy
X1

### DZIALA


####### wersja dla danych macierzy nxp gdzie p > n


PC_svd <- XtoSVD %*% V

PC1_svd <- XtoSVD %*% V[,1] 
PC1_svd

PC2_svd <- XtoSVD %*% V[,2] 

Xall_svd <- PC_svd %*% t(V) 

X1_svd <- PC1_svd %*% t(PC1_svd) %*% XtoSVD    # z dokladnoscia do przeskalowania
X1_svd / (X1_svd[1,1] / X1[1,1])  

X2_svd <- PC2_svd %*% t(PC2_svd) %*% XtoSVD    # z dokladnoscia do przeskalowania

plot(X2_1[1,], X2_1[2,])
plot(X1_svd[1,], X1_svd[2,])

plot(X2_2[1,], X2_2[2,])
plot(X2_svd[1,], X2_svd[2,])


plot(X2[1,], X2[2,])
plot(X2_recreated[1,], X2_recreated[2,])
plot(Xall_svd[1,], Xall_svd[2,])

### DZIALA


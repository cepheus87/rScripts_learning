# principal component analysis (PCA) and singular value decomposition (SVD)

# https://stats.stackexchange.com/questions/134282/relationship-between-svd-and-pca-how-to-use-svd-to-perform-pca

# https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/

#jak rysowac biploty przez autoplot
# https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html

# wazny pkt to 3 !!!!!!!!!
# 3a przypadek, gdy wiecej obserwabli niz pomiarow n x p ( p > n ) 

#w pakiecie stats
?prcomp

#uzywany data.frame dane fizyczne dla gatunkow kwiatkow
data("iris")
head(iris)

table(iris$Species)


# log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,
                 center = TRUE,  # shifiting to 0 
                 scale. = TRUE)  # if variables should be scaled to have unit variance before the analysis takes place

# The prcomp function returns an object of class prcomp, which have some methods available. The print method returns the standard 
# deviation of each of the four PCs, and their rotation (or loadings), which are the coefficients of the linear combinations 
# of the continuous variables.

# print method
print(ir.pca)


#The plot method returns a plot of the variances (y-axis) associated with the PCs (x-axis). The Figure below is useful to decide how many PCs 
# to retain for further analysis. In this simple case with only 4 PCs this is not a hard task and we can see that the first two PCs explain 
# most of the variability in the data.


# plot method
plot(ir.pca, type = "l")



# The summary method describe the importance of the PCs. The first row describe again the standard deviation associated with each PC. 
# The second row shows the proportion of the variance in the data explained by each component while the third row describe the cumulative 
#proportion of explained variance. We can see there that the first two PCs accounts for more than {95\%} of the variance of the data.

summary(ir.pca)



#We can use the predict function if we observe new data and want to predict their PCs values. Just for illustration pretend the last two rows of 
#the iris data has just arrived and we want to see what is their PCs values:

# Predict PCs
predict(ir.pca, 
        newdata=tail(log.ir, 2))


biplot(ir.pca)




############################3

# same but for different data

library(HSAUR)
data("heptathlon", package = "HSAUR")

head(heptathlon)

#some preprocesing 

heptathlon$hurdles <- max(heptathlon$hurdles) -   heptathlon$hurdles
heptathlon$run200m <- max(heptathlon$run200m) -  heptathlon$run200m
heptathlon$run800m <- max(heptathlon$run800m) -   heptathlon$run800m

score <- which(colnames(heptathlon) == "score")

#correlation 
round(cor(heptathlon[,-score]), 2)


X <- as.matrix(heptathlon[,-score])
#### takie rozkminy #####

n <- nrow(X)   # samples no
p <- ncol(X)    # variables no

C <- ( t(X) %*% X ) / (n-1)    # dziala jak X - X_bar

cov(X)
cov2cor(( cov(X) ) )
cov2cor(C)

#############


heptathlon_pca <- prcomp(heptathlon[, -score], scale. = TRUE)
heptathlon_pca <- prcomp(heptathlon[, -score], scale = TRUE)
# roznica mieczy .scale, scale -> brak roznicy
?prcomp

print(heptathlon_pca)
plot(heptathlon[,-score])

summary(heptathlon_pca)

#Linear combination for the first principal component is 

a1 <- heptathlon_pca$rotation[, 1]
a1

# which is equal to first eigenvector (PC1)


# The center and the scaling used by prcomp internally can be extracted
center<- ir.pca$center
scale <- ir.pca$scale



##### TUTAJ poprawny przyklad ###########
# !!!! 3


# http://www.cs.otago.ac.nz/cosc453/student_tutorials/principal_components.pdf
# troche zamotane pod koniec z tymi transpozycjami macierzy

X <- rbind( c(.69, .49), c(-1.31, -1.21), c(.39, .99), c(.09, .29), c(1.29, 1.09), c(.49, .79), c(.19, -.31), c(-.81, -.81), c(-.31, -.31), c(-.71, -1.01) )
axes <- c("xax", "yax")

X_3col <- cbind(X, round(rnorm(10), 1) )
X_3col[,3] <- X_3col[,3] - mean(X_3col[,3])
X<- X_3col
axes <- c(axes, "zax")

colnames(X) <- axes

X
plot(X)

XnotCentered <- t( t(X) + c(1.81, 1.91 ))
XnotCentered


cor(X)

cov(X)
C <- ( t(X) %*% X ) / (nrow(X)-1)
C


#to sa eigenvectors (przeskalowane przez squaredroot of eigenvalues)
pca <- prcomp(X)   # dla X_3col rot = 3x3
pca

pca_notCentData <- prcomp(XnotCentered, center = T, scale = F )
pca_notCentData

evec <- pca$rotation   # to jest V
#ortogonalne -> evec %*% t(evec) = unit
# V == rotation == loadings 


pc1 <- pca$rotation[,1]
pc2 <- pca$rotation[,2]
pc3 <- pca$rotation[,3]


# X %*% pca$ rotation == scores


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

# it is equal to pca$x


PC <-X %*% evec
PC1 <- X %*% pc1
PC1
#without ver
PCv <- X %*% evec[,-2]
PCv

# get back to data  X = PC t(V)
PC %*% t(evec)  
X1 <- PC1 %*% t(pc1)
X1   # data transformed to 1st pc
plot(X1)

X1v <- PCv %*% t(evec[,-2])
X1v

plot(x = X[,1], y = PC1)  #  projection of PC1

# rzutowanie PC t(PC) na dane 
Xnew <- X - ( PC1 %*% t(PC1) %*% X)

######## sprawdzenie liczenie korelacji  wzgledem PCs

PC2 <- X %*% pc2
PC3 <- X %*% pc3

PC # (10x3)
PC2 # (10x1)

cor( t(PC) %*% PC1 )


####  ponizej wersja zamotana z transpozycjami macierzy

plot(X)
lmobj<-lm( X[,2] ~ X[,1])
abline(lmobj)
summary(lmobj)

pca$rotation[1,1]
abline(0, pca$rotation[2,1] / pca$rotation[1,1], col = 2)   # Ax + By = 0


library(ggfortify)
autoplot(pca, data = X, loadings = TRUE, loadings.colour = 'blue', loadings.label = TRUE, loadings.label.size = 4)

biplot(pca)
pc1 # w kierunku PC1
pc2 # w kierunku PC2

# to choose given principal component we need to create a matrix of eigenvectors as columns
#   finalData = t(choosenEigenVec)   %*%  t(data)

#dane w przestrzeni wybranego wektora (obrocone)   # wersja wyzsza jest prostsza niz te dziwne transpozycje
finalData1 <- t(pc1) %*% t(X)
finalData1
t(finalData1)
X[,"xax"]
plot( x= X[,"xax"], y = t(finalData1))
plot(t(finalData1))

#1PC vs xax
plot(x = t(finalData1), y = X[,"xax"])

#1PC vs yax
plot(x = t(finalData1), y = X[,"yax"])

finalData2 <- t(pc2) %*% t(X)
finalData2
plot(t(finalData2))


#dane w przestrzeni PC1 vs PC2
finalDataAll <- t(pca$rotation) %*% t(X)
t(finalDataAll)
plot(t(finalDataAll))



# to obtain orginal coordinates 
# finalData = t(choosenEigenVec)   %*%  t(data)  => t(data)  = t(choosenEigenVec) ^-1  %*% finalData   # due to orthogonality ^-1 == T
# and restore orginal mean, which was subtracked (here step was ommited - already subtracted data)

# t(data)  = (choosenEigenVec  %*% finalData) + original mean 


orgData1 <- pc1 %*% finalData1 
plot(t(orgData1))

orgData2 <- pc2 %*% finalData2 
plot(t(orgData2))

orgDataAll <- pca$rotation %*% finalDataAll
plot(t(orgDataAll))


##### wiecej obserwabli niz pomiarow #########

# 3a  !!!! rozkminy, gdy wiecej obserwabli niz pomiarow n x p ( p > n ) 

X2 <- rbind( c(.69, .49), c(-1.31, -1.21), c(.39, .99), c(.09, .29), c(1.29, 1.09), c(.49, .79), c(.19, -.31), c(-.81, -.81), c(-.31, -.31), c(-.71, -1.01) )
X_3col <- cbind(X2, round(rnorm(10), 1) )
X_3col[,3] <- X_3col[,3] - mean(X_3col[,3])
X2<- X_3col
X2 <- t(X)
X2   #( 3 x 10 )


pca2 <- prcomp(X2, center = T)
pca2


evec2 <- pca2$rotation  

evec2_1col <- pca2$rotation[,1]


 evec2 %*% t(evec2)  # (5 x 5) - zle
t(evec2) %*% evec2   # (3 x 3) ~ unit

PC_2 <- X2 %*% evec2
PC_2

PC_2_1 <- X2 %*% evec2_1col
PC_2_1

c_ct <- PC_2_1 %*% t(PC_2_1)
c_ct

c_ct %*% X2

X2new <- X2 - (c_ct %*% X2)

plot(X2[1,], X2[2,])
plot(X2new[1,], X2new[2,])

# nie da sie odzyskac danych oryginalnych

######
# plyr/dplyr
# lubridate   # do praca na datach
# knitr # do raportow
# stringr # praca na stringach
# tidyr  # praca na tagowanych danych
# 
# #do wczytywania danych
# readr   #  do wczytywania tabelarycznych
# readxl  # do wczytywania danych excellowskich
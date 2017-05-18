library(ISLR)
str(Smarket)


library(class)  # knn

train<-(Smarket$Year<2005)
train_X <- cbind(Smarket$Lag1, Smarket$Lag2)[train, ]
test_X <- cbind(Smarket$Lag1, Smarket$Lag2)[!train, ]
trainDirection<-Smarket$Direction[train]
direction <- Smarket$Direction[!train]

# if in knn() obs are tied, tie will be break randomly (depend on set seed)

set.seed(1)



length(train_X)
length(trainDirection)


length(test_X)
length(direction)


knn_pred <- knn(train_X, test_X, trainDirection, k = 1)  # zwraca predykcje dla danych test

knn_pred

table(knn_pred, direction)
mean(knn_pred == direction)    # 50%


knn_pred <- knn(train_X, test_X, trainDirection, k = 3)
mean(knn_pred == direction)    # 53% - zwiekszenie k zwiekszylo szanse dopasowania 

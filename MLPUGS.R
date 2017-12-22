# https://cran.r-project.org/web/packages/MLPUGS/vignettes/tutorial.html


#### package mlr
# https://mlr-org.github.io/mlr-tutorial/devel/html/multilabel/index.html

require(MLPUGS)
require(randomForest)

data("movies")
head(movies)

size(movies)

data("movies_train"); 
data("movies_test")

size(movies_train) # 60%
size(movies_test) # 40%

#### Training an Ensemble of Classifier Chains (ECC) ####
ecc()
fit <- ecc(movies_train[, -(1:3)], movies_train[1:3], 3, randomForest::randomForest, replace = TRUE)  # 3 models, 95% of train data


#### Prediction Using Gibbs Sampling (PUGS) #### 

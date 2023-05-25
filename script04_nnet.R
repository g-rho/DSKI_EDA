## iris data
data(iris)
head(iris)

summary(iris)

plot(iris)
plot(iris[,1:4], col = iris$Species)

# https://archive.ics.uci.edu/ml/datasets/iris
# https://en.wikipedia.org/wiki/Iris_flower_data_set


## avoid overfitting: split data in to trainign and test set 
set.seed(42)
nrow(iris) # number of observations in data
trainsize <- round(0.7 * nrow(iris)) # size of the trainig set

sample(nrow(iris)) # samp,e ids for training set 
sample(nrow(iris), trainsize)
train.ids <- sample(nrow(iris), trainsize) 

iris.train <- iris[train.ids,]# create training and validation data
iris.test  <- iris[-train.ids,] 
rm(iris) #for big data: remove entire data set from workspace 


## train neural network
library(nnet)
set.seed(42) # ensure reproducible random initialization of the network weights
nni <- nnet(Species ~ Petal.Length + Petal.Width, data = iris.train, size = 4) 
  # train neural network with 1 hidden layer of four neurons

summary(nni)

library(NeuralNetTools)
plotnet(nni)


# making predictions
predict(nni, iris.train)
head(predict(nni, iris.train), 3)
# Question: What class are the first three flowers of the training data assigned to?

predict(nni, iris.train, type = "class")

# confusion matrix
         table(predict(nni, iris.train, type = "class"), iris.train$Species)

# compute accuaracy ( = 1 - misclassification error)         
    diag(table(predict(nni, iris.train, type = "class"), iris.train$Species))
sum(diag(table(predict(nni, iris.train, type = "class"), iris.train$Species)))
sum(diag(table(predict(nni, iris.train, type = "class"), iris.train$Species))) / nrow(iris.train) # ...for training data

# ... and for test data
table(predict(nni, iris.test, type = "class"), iris.test$Species)
sum(diag(table(predict(nni, iris.test, type = "class"), iris.test$Species))) / nrow(iris.test) 



# References:
# - Schmuller, J. (2018). R in Projekten anwenden fuer Dummies, Wiley (Kap. 11).
# - Rashid, T. (2017). Neuronale Netze selbst Trainieren. O'Reily.
# - James, G., Witten, D., hastie, T., Tibshirani, R. (2021). An Introduction to Statistical Learning. 2nd Ed. Springer (Kap. 10: Deep Learning). online: https://www.statlearning.com/
#Example for the kaggle forums.
library(stats)
library("e1071")
library("devtools")
library("ggbiplot")

trainingSet <- read.csv("data/train.csv")
validationSet <- read.csv("data/test.csv")
#combinedSet <- read.csv("data/combined.csv")
set.seed(20140202)
#trainingSet <- train[sample(1:nrow(train), N), ]
#validationSet <- train[sample(25000:nrow(train), M), ]
#validationSet <- train[!(rownames(train) %in% rownames(trainingSet)), ]

trainingLabel <- trainingSet[, 1]
trainingSet <- trainingSet[, -1]
#validationLabel <- validationSet[, 1]
#validationSet <- validationSet[, -1]

prc <- proc.time()
principalComps <- prcomp( ~. , data = trainingSet)
screeplot(principalComps, type="line", main="Principle Components")
num.of.comp.train = 50
pca.train.rotation <- principalComps$rotation
pca.train.rotation <- pca.train.rotation[,1:num.of.comp.train]
trainingPRC <- as.matrix(trainingSet) %*% pca.train.rotation
validationPRC <- as.matrix(validationSet) %*% pca.train.rotation

train.model <- svm(trainingPRC, trainingLabel,  type = "C-classification", kernel = "radial", gamma=0.01, cost = 10)

svm.pred <- predict(train.model, validationPRC)
write.csv(as.matrix(svm.pred), file = "calculateddata/results.csv")
print(proc.time() - prc)
#sum(svm.pred == validationLabel)/length(validationLabel)


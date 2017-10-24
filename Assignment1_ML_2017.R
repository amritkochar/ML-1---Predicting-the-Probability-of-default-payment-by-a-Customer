#caret library for easy pre processing of data
library(caret)

cc_data <- read.csv("D:/pilani/fourth year/4-1/ML/Assignment 1/data.csv")
#View(cc_data)

#saving the last column to append it after applying PCA on the other columns
lastcol <- cc_data[,25]

#deleting the last column to normalize and apply PCA
cc_data <- cc_data[,1:24]

#storing difference of bill amt and paid amt as a new feature 
for (i in 1:nrow(cc_data)){

	cc_data[i,13] = cc_data[i,13]-cc_data[i,19]
	cc_data[i,14] = cc_data[i,14]-cc_data[i,20]
	cc_data[i,15] = cc_data[i,15]-cc_data[i,21]
	cc_data[i,16] = cc_data[i,16]-cc_data[i,22]
	cc_data[i,17] = cc_data[i,17]-cc_data[i,23]
	cc_data[i,18] = cc_data[i,18]-cc_data[i,24]
}

#deleting individual bill amt and paid amt columns
cc_data <- subset(cc_data, select = -c(19:24))

#normalize
preprocessParams <- preProcess(cc_data, method=c("range"))
cc_data<-predict(preprocessParams,cc_data)

#PCA
preprocessParams <- preProcess(cc_data, method=c("center", "scale", "pca"))
cc_data <- predict(preprocessParams, cc_data)

#appending last column
cc_data$defaultpaymentnextmonth <- lastcol

set.seed(1234)

training_data_size <- floor(0.6 * nrow(cc_data))
#training_data_size
cc_train_index <- sample(1:nrow(cc_data), training_data_size)
#Training data
cc_train <- cc_data[cc_train_index, ]
# head(cc_train)
#Testing data
cc_test <- cc_data[-cc_train_index, ]
# head(cc_test)

# Decision Tree

library(rpart)

# grow tree

fit <-  rpart(defaultpaymentnextmonth ~ ., data = cc_train, method = "class")
#print(fit)
#summary(fit)

#fit$cptable

plotcp(fit)

fit.pruned <- prune(fit,cp=0.01)

library(rpart.plot)

#plotting the D tree
prp(fit.pruned, type = 1, extra = 104,
    fallen.leaves = TRUE, main="Decision Tree")

#testing of the model
fit.pred <- predict(fit.pruned, cc_test, type="class")
fit.perf <- table(cc_test$defaultpaymentnextmonth, fit.pred,
                    dnn=c("Actual", "Predicted"))

#matrix of performance
fit.perf

(fit.perf[1,1]+fit.perf[2,2])/(fit.perf[1,1]+fit.perf[1,2]+fit.perf[2,2]+fit.perf[2,1])
# % prediction accuracy = 0.80875
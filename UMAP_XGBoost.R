library(umap)
library(caret)
library(xgboost)

# UMAP plot
data <-  # Import data in “data frame” format.This data is the peak table derived from KPIC2
labels <-  # The label corresponding to the data is imported, format as “factor” type
umap = umap(data)
plot(umap[["layout"]][,1],.umap[["layout"]][,2], col = labels)
# Divide training set and test set
set.seed(1234)
index<-createDataPartition(labels, p=.67, list = FALSE)
train<-data[index,]
test<-data[-index,] 
# After dividing the training set and the test set according to the labels,
# export the training data in csv format, the test data and their corresponding labels 
# (train_data, test_data, train_label, test_label). Then, these csv files are re-imported.
# Data preprocessing
train_label <- as.numeric(train_label)
test_label<-as.numeric(test_label)
dtrain <- xgb.DMatrix(data = as.matrix(train_data), label=train_label)
dtest <- xgb.DMatrix(data = as.matrix(test_data), label=test_label)
# Training model
model <- xgboost(data = dtrain, max.depth = 2, eta = 1, nthread = 2, nrounds = 2)
pred <- predict(model, dtest)
print(model)
# Confusion matrix
pre_xgb = round(predict(model,newdata = dtest))
table(test_label,pre_xgb, dnn=c("true","pre"))
# Sort the importance of features.
importance_matrix <- xgb.importance(model = model)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

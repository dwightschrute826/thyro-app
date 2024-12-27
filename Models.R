library(caret)
data <- read.csv("rfovunsamplenew.csv")
data <- subset(data, select = -c(referral_source))
# char_cols <- c("sex","on_thyroxine","query_on_thyroxine","on_antithyroid_medication","sick","pregnant","thyroid_surgery","I131_treatment","query_hypothyroid","query_hyperthyroid","lithium","goitre","tumor","hypopituitary","psych","referral_source","Class")
char_cols <- c("sex","on_thyroxine","query_on_thyroxine","on_antithyroid_medication","sick","pregnant","thyroid_surgery","I131_treatment","query_hypothyroid","query_hyperthyroid","lithium","goitre","tumor","hypopituitary","psych","Class")
data[char_cols] <- lapply(data[char_cols], as.factor)
data$Class <- factor(make.names(data$Class))
# data <- subset(data, select = -c(hypopituitary))
# data <- data[ ,c("T3", "FTI", "referral_source", "age", "TSH", "Class")]
#data <- data[, c("T3", "T4U", "age", "TT4", "FTI", "TSH", "Class")]
data <- data[, c("T3", "FTI", "age", "TSH", "Class")]
#data <- data[, c("T3", "FTI", "TSH", "Class")]

set.seed(123)
train_index <- createDataPartition(data$Class, p = 0.65, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]
actual_classes <- test_data$Class
test_data <- subset(test_data, select = -Class)
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE)

# SVM
library(e1071)
svm_model <- train(Class ~ ., data = train_data, method = "svmRadial",trControl = ctrl)
predictions1 <- predict(svm_model, test_data)
c1 <- confusionMatrix(predictions1, actual_classes)
accuracy <- round(c1$overall['Accuracy'], 3)
precision <- round(c1$byClass['Pos Pred Value'], 3)
recall <- round(c1$byClass['Sensitivity'], 3)
f1_score <- round(c1$byClass['F1'], 3)
specificity <- round(c1$byClass['Specificity'], 3)
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")
cat("Specificity:", specificity, "\n")
library(ROCR)
pred1 <- prediction(as.numeric(predictions1), actual_classes)
perf1 <- performance(pred1, "tpr", "fpr")
plot(perf1, main="ROC for SVM", col="blue", lwd=2)
abline(0, 1, col = "grey")
auc1 <- performance(pred1, "auc")
legend("bottomright", paste(round(as.numeric(auc1@y.values), digits = 4)), col = c("green"), pch = c(3))
auc1 <- auc1@y.values[[1]]
cat("AUC: ",auc1, "\n")

# LR
library(glmnet)
set.seed(123)
lr_model <- train(Class ~ ., data = train_data, method = "glmnet", family = "binomial", trControl = ctrl)
predictions2 <- predict(lr_model, test_data)
c2 <- confusionMatrix(predictions2, actual_classes)
accuracy <- round(c2$overall['Accuracy'], 3)
precision <- round(c2$byClass['Pos Pred Value'], 3)
recall <- round(c2$byClass['Sensitivity'], 3)
f1_score <- round(c2$byClass['F1'], 3)
specificity <- round(c2$byClass['Specificity'], 3)
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")
cat("Specificity:", specificity, "\n")
library(ROCR)
pred2 <- prediction(as.numeric(predictions2), actual_classes)
perf2 <- performance(pred2, "tpr", "fpr")
plot(perf2, main="ROC for LR", col="red", lwd=2)
abline(0, 1, col = "grey")
auc2 <- performance(pred2, "auc")
legend("bottomright", paste(round(as.numeric(auc2@y.values), digits = 4)), col = c("green"), pch = c(3))
auc2 <- auc2@y.values[[1]]
cat("AUC: ",auc2, "\n")




# RF

library(randomForest)
set.seed(123)
rf_model <- train(Class ~ ., data = train_data, method = "rf", trControl = ctrl, ntree = 100, verbose=TRUE)
predictions3 <- predict(rf_model, test_data)
c3 <- confusionMatrix(predictions3, actual_classes)
accuracy <- round(c3$overall['Accuracy'], 3)
precision <- round(c3$byClass['Pos Pred Value'], 3)
recall <- round(c3$byClass['Sensitivity'], 3)
f1_score <- round(c3$byClass['F1'], 3)
specificity <- round(c3$byClass['Specificity'], 3)
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")
cat("Specificity:", specificity, "\n")
library(ROCR)
pred3 <- prediction(as.numeric(predictions3), actual_classes)
perf3 <- performance(pred3, "tpr", "fpr")
plot(perf3, main="ROC for RF", col="black", lwd=2)
abline(0, 1, col = "grey")
auc3 <- performance(pred3, "auc")
legend("bottomright", paste(round(as.numeric(auc3@y.values), digits = 4)), col = c("black"), pch = c(3))
auc3 <- auc3@y.values[[1]]
cat("AUC: ",auc3, "\n")



# XGBoost
set.seed(123)
library(xgboost)
tuning_grid <- expand.grid(
  nrounds = c(100, 200),           
  max_depth = c(3, 6),            
  eta = c(0.1),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)
xgb_model <- train(Class ~ ., data = train_data, method = "xgbTree", trControl = ctrl,  tuneGrid = tuning_grid, verbose = FALSE, verbosity=0)
predictions4 <- predict(xgb_model, test_data)
c4 <- confusionMatrix(predictions4, actual_classes)
accuracy <- round(c4$overall['Accuracy'], 3)
precision <- round(c4$byClass['Pos Pred Value'], 3)
recall <- round(c4$byClass['Sensitivity'], 3)
f1_score <- round(c4$byClass['F1'], 3)
specificity <- round(c4$byClass['Specificity'], 3)
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")
cat("Specificity:", specificity, "\n")
pred4 <- prediction(as.numeric(predictions4), actual_classes)
perf4 <- performance(pred4, "tpr", "fpr")
plot(perf4, main="ROC for XGBoost", col="purple", lwd=2)
abline(0, 1, col = "grey")
auc4 <- performance(pred4, "auc")
legend("bottomright", paste(round(as.numeric(auc4@y.values), digits = 4)), col = c("green"), pch = c(3))
auc4 <- auc4@y.values[[1]]
cat("AUC: ",auc4, "\n")


# Naive Bayes
library(e1071)
set.seed(123)
nb_model <- naiveBayes(Class ~ ., data = train_data, trControl = ctrl)
predictions5 <- predict(nb_model, test_data)
c5 <- confusionMatrix(predictions5, actual_classes)
accuracy <- round(c5$overall['Accuracy'], 3)
precision <- round(c5$byClass['Pos Pred Value'], 3)
recall <- round(c5$byClass['Sensitivity'], 3)
f1_score <- round(c5$byClass['F1'], 3)
specificity <- round(c5$byClass['Specificity'], 3)
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")
cat("Specificity:", specificity, "\n")
pred5 <- prediction(as.numeric(predictions5), actual_classes)
perf5 <- performance(pred5, "tpr", "fpr")
plot(perf5, main="ROC for Naive Bayes", col="black", lwd=2)
abline(0, 1, col = "grey")
auc5 <- performance(pred5, "auc")
legend("bottomright", paste(round(as.numeric(auc5@y.values), digits = 4)), col = c("green"), pch = c(3))
auc5 <- auc5@y.values[[1]]
cat("AUC: ",auc5, "\n")



# Decision Tree
library(rpart)
set.seed(123)
tree_model <- train(Class ~ ., data = train_data, method = "rpart", trControl = ctrl)
predictions6 <- predict(tree_model, test_data)
c6 <- confusionMatrix(predictions6, actual_classes)
accuracy <- round(c6$overall['Accuracy'], 3)
precision <- round(c6$byClass['Pos Pred Value'], 3)
recall <- round(c6$byClass['Sensitivity'], 3)
f1_score <- round(c6$byClass['F1'], 3)
specificity <- round(c6$byClass['Specificity'], 3)
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")
cat("Specificity:", specificity, "\n")
pred6 <- prediction(as.numeric(predictions6), actual_classes)
perf6 <- performance(pred6, "tpr", "fpr")
plot(perf6, main="ROC for Decision Tree", col="orange", lwd=2)
abline(0, 1, col = "grey")
auc6 <- performance(pred6, "auc")
legend("bottomright", paste(round(as.numeric(auc6@y.values), digits = 4)), col = c("green"), pch = c(3))
auc6 <- auc6@y.values[[1]]
cat("AUC: ",auc6, "\n")

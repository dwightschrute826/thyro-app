# RF importance
data <- read.csv("rfovunsamplenew.csv")
data <- subset(data, select = -c(referral_source))
# char_cols <- c("sex","on_thyroxine","query_on_thyroxine","on_antithyroid_medication","sick","pregnant","thyroid_surgery","I131_treatment","query_hypothyroid","query_hyperthyroid","lithium","goitre","tumor","hypopituitary","psych","referral_source","Class")
char_cols <- c("sex","on_thyroxine","query_on_thyroxine","on_antithyroid_medication","sick","pregnant","thyroid_surgery","I131_treatment","query_hypothyroid","query_hyperthyroid","lithium","goitre","tumor","hypopituitary","psych","Class")

data[char_cols] <- lapply(data[char_cols], as.factor)
library(randomForest)
set.seed(123)
X <- subset(data, select = -Class)
Y <- data$Class
rfmodel <- randomForest(X,Y)
importance_scores <- importance(rfmodel)
print(importance_scores)
mean_decrease_gini <- rfmodel$importance[,"MeanDecreaseGini"]
column_nos <- 1:length(mean_decrease_gini)
feature_importance_df <- data.frame(Column_Number = column_nos, MeanDecreaseGini = mean_decrease_gini)
sorted_df <- feature_importance_df[order(-feature_importance_df$MeanDecreaseGini), ]
print(sorted_df)
final_df <- data.frame(Features = rownames(importance_scores), Importances = importance_scores[, 1])

library(ggplot2)
ggplot(final_df, aes(x = Features, y = Importances)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Feature Importances")


# chi-square test
library(stats)
hypothyroid <- data
char_columns <- names(hypothyroid)[sapply(hypothyroid, is.factor)]
chi_square_results <- sapply(char_columns, function(feature) {
  chi_square <- chisq.test(table(hypothyroid$Class, hypothyroid[[feature]]))
  c(statistic = chi_square$statistic, p_value = chi_square$p.value)
})
chi_square_results_df <- as.data.frame(t(chi_square_results))
row.names(chi_square_results_df) <- char_columns
print(chi_square_results_df)



# RFE
# char_cols <- c("sex","on_thyroxine","query_on_thyroxine","on_antithyroid_medication","sick","pregnant","thyroid_surgery","I131_treatment","query_hypothyroid","query_hyperthyroid","lithium","goitre","tumor","hypopituitary","psych","referral_source","Class")
char_cols <- c("sex","on_thyroxine","query_on_thyroxine","on_antithyroid_medication","sick","pregnant","thyroid_surgery","I131_treatment","query_hypothyroid","query_hyperthyroid","lithium","goitre","tumor","hypopituitary","psych","Class")

data[char_cols] <- lapply(data[char_cols], as.factor)
library(caret)
subsets <- c(1:5, 8, 10, 12)
set.seed(335)
rfeCtrl <- rfeControl(functions = rfFuncs, method = "cv", verbose = FALSE)
result <- rfe(x = X, y = Y, sizes = subsets, rfeControl = rfeCtrl)
print(result)



# Correlation matrix
library(dplyr)
numericdata <- select_if(data, is.numeric)
data$Class_numeric <- as.numeric(factor(data$Class))
numericdata <- cbind(numericdata, class = data$Class_numeric)
cr1 <- cor(numericdata)
print(cr1)
# install.packages("corrplot")
library(corrplot)
corrplot(cr1, method = "color")

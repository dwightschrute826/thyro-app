# Removing unwanted features
data <- read.csv("hypothyroid.csv", na.strings = c("?"))
data <- subset(data, select = -c(TSH_measured, T3_measured, TT4_measured, T4U_measured, FTI_measured, TBG_measured, TBG))
data <- data[!is.na(data$age), ]
data <- data[!is.na(data$sex), ]
write.csv(data, "hypothyroid_subset.csv", row.names = FALSE)

# Imputing missing values with Mean
mean_tsh <- mean(data$TSH, na.rm = TRUE)
data[is.na(data$TSH), "TSH"] <- mean_tsh
mean_t3 <- mean(data$T3, na.rm = TRUE)
data[is.na(data$T3), "T3"] <- mean_t3
mean_tt4 <- mean(data$TT4, na.rm = TRUE)
data[is.na(data$TT4), "TT4"] <- mean_tt4
mean_t4u <- mean(data$T4U, na.rm = TRUE)
data[is.na(data$T4U), "T4U"] <- mean_t4u
mean_fti <- mean(data$FTI, na.rm = TRUE)
data[is.na(data$FTI), "FTI"] <- mean_fti

# Normalizing
num_cols <- c("age","TSH","T3","TT4","T4U","FTI")
library(dplyr)
char_cols1 <- c("sex","on_thyroxine","query_on_thyroxine","on_antithyroid_medication","sick","pregnant","thyroid_surgery","I131_treatment","query_hypothyroid","query_hyperthyroid","lithium","goitre","tumor","hypopituitary","psych")
char_cols2 <- c("referral_source","Class")
# char_data <- data.frame(imputed_data[char_cols1])
# char_data2 <- data.frame(imputed_data[char_cols2])
char_data <- data.frame(data[char_cols1])
char_data2 <- data.frame(data[char_cols2])
num_data <- data[,num_cols]

n <- function(b){
  (b-min(b))/(max(b)-min(b))
}
norm_data <- as.data.frame(lapply(num_data, n))
age <- norm_data$age
a_df <- data.frame(age)
f1 <- subset(norm_data, select = -c(age))
data <- cbind(a_df, char_data)
data <- cbind(data, f1)
data <- cbind(data, char_data2)
data[data=='f'] = 0
data[data=='t'] = 1
data[data=='M'] = 0
data[data=='F'] = 1
data[data=='negative'] = 0
data[data=='sick'] = 1
unique(data$referral_source)
data[data=='SVHC'] = 0
data[data=='other'] = 1
data[data=='SVI'] = 2
data[data=='STMW'] = 3
data[data=='SVHD'] = 4
char_cols <- c(char_cols1,char_cols2)
data[char_cols] <- lapply(data[char_cols], as.factor)
write.csv(data, "newrfimpute.csv", row.names = FALSE)
write.csv(data, "mean.csv", row.names = FALSE)


# KNN
library(VIM)
data <- read.csv("hypothyroid_subset.csv")
hypothyroid_knn <- kNN(data, variable=num_cols, k=6)
data <- subset(hypothyroid_knn, select = age:Class)
write.csv(data, "knnwithoutnorm.csv", row.names = FALSE)
# Normalize numeric cols
write.csv(data, "knn.csv", row.names = FALSE)


# rf impute
set.seed(123)
data <- read.csv("hypothyroid_subset.csv")
data[data=='f'] = 0
data[data=='t'] = 1
data[data=='M'] = 0
data[data=='F'] = 1
data[data=='negative'] = 0
data[data=='sick'] = 1
unique(data$referral_source)
data[data=='SVHC'] = 0
data[data=='other'] = 1
data[data=='SVI'] = 2
data[data=='STMW'] = 3
data[data=='SVHD'] = 4
data[char_cols] <- lapply(data[char_cols], as.factor)
data$age <- as.numeric(as.integer(data$age))
predictors <- subset(data, select = -Class)
y <- data$Class
library(randomForest)
imputed_data <- rfImpute(x = predictors, y = y)
imputed_data <- rename(imputed_data, Class = y)
imputed_data <- select(imputed_data, -Class, everything())
num_data <- imputed_data[,num_cols]
# Normalize
write.csv(data, "rfimpute.csv", row.names = FALSE)


# Shuffling and balancing
# SMOTE
data <- read.csv("mean.csv")
data <- read.csv("knn.csv")
data <- read.csv("rfimpute.csv")
data <- read.csv("newrfimpute.csv")
data[char_cols] <- lapply(data[char_cols], as.factor)
set.seed(2347723)
library(DMwR)
balanced_data <- SMOTE(Class ~ ., data, perc.over = 200, perc.under = 100)
shuffled_indices <- sample(nrow(balanced_data))
balanced_data <- balanced_data[shuffled_indices, ]
write.csv(balanced_data, "meansmotenew.csv", row.names = FALSE)
write.csv(balanced_data, "knnsmotenew.csv", row.names = FALSE)
write.csv(balanced_data, "rfsmotenew.csv", row.names = FALSE)
write.csv(balanced_data, "newrfsmote.csv", row.names = FALSE)
write.csv(balanced_data, "rfsmote.csv", row.names = FALSE)

# ROSE
library(ROSE)
rosedata <- ROSE(Class ~ ., N=1000, data = data, seed = 42, p = 0.5)$data
shuffled_indices <- sample(nrow(rosedata))
rosedata <- rosedata[shuffled_indices, ]
write.csv(rosedata, "meanrosenew.csv", row.names = FALSE)
write.csv(rosedata, "knnrosenew.csv", row.names = FALSE)
write.csv(rosedata, "rfrosenew.csv", row.names = FALSE)
write.csv(rosedata, "newrfrose.csv", row.names = FALSE)
write.csv(rosedata, "rfrose.csv", row.names = FALSE)


# ovun sample
both <- ovun.sample(Class ~ ., data = data, method = "both", N=1000)
both <- both$data
shuffled_indices <- sample(nrow(both))
both <- both[shuffled_indices, ]
write.csv(both, "meanovunsamplenew.csv", row.names = FALSE)
write.csv(both, "knnovunsamplenew.csv", row.names = FALSE)
write.csv(both, "rfovunsamplenew.csv", row.names = FALSE)
write.csv(both, "newrfovunsample.csv", row.names = FALSE)
write.csv(both, "rfovunsample.csv", row.names = FALSE)  

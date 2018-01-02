library(caret)

heart_df <- read.csv("D:/R-files/Learning SVM/heart_tidy.csv", sep=",", header=F)

str(heart_df)

head(heart_df)

set.seed(3033)

intrain <- createDataPartition(y=heart_df$V14, p=0.7, list=F)

training <- heart_df[intrain,]
training

testing <- heart_df[-intrain,]

summary(testing)

dim(training)

dim(testing)

anyNA(heart_df)

summary(heart_df)

training["V14"] = factor(training[["V14"]])
training["V14"]

trctrl <- trainControl(method = "repeatedcv", number=10, repeats = 3)

set.seed(3233)

# install.packages("e1071")
library(e1071)

svm_linear <- train(V14~. , data=training, method = "svmLinear",
                    trControl = trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength=10)

svm_linear

test_pred <- predict(svm_linear, newdata= testing)

summary(test_pred)
training[""]

confusionMatrix(test_pred, testing$V14)

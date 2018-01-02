library(e1071)
data <- read.csv("D:/R-files/regression.csv", header = T)

model <- svm(Y ~ X , data)

predictedY <- predict(model, data)

predictedY
plot(data$X, predictedY, col = "red", pch=4)

error <- data$Y - predictedY
svrPredictionRMSE <- rmse(error)


# perform a grid search
tuneResult <- tune(svm, Y ~ X,  data = data,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)
print(tuneResult)
# Draw the tuning graph
plot(tuneResult)


tuneResult <- tune(svm, Y ~ X,  data = data,
                   ranges = list(epsilon = seq(0,0.2,0.01), cost = 2^(2:9))
) 

print(tuneResult)
plot(tuneResult)


tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, data) 

tunedModelY
error <- data$Y - tunedModelY  

# this value can be different on your computer
# because the tune method  randomly shuffles the data
tunedModelRMSE <- rmse(error)
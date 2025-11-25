#loading datasets
data <- read.csv("german_credit_data.csv", stringsAsFactors = TRUE)
head(data)

#decision tree model
library(rpart.plot)
model <- rpart(Risk ~ ., data = data, method = "anova")

#plot the tree
library(rpart)
rpart.plot(model)

#data splitting to training and test set
set.seed(42)
n <- nrow(data)
train_index <- sample(1:n, 0.8 * n)

train <- data[train_index, ]
test <- data[-train_index, ]

model <- rpart(Risk ~ ., data = train, method = "class")
pred <- predict(model, test, type = "class")

table(pred, test$Risk)



library(rpart)
library(rpart.plot)

data <- read.csv("german_credit_data.csv", stringsAsFactors = FALSE)


risk_raw <- tolower(as.character(data$Risk))

if (all(risk_raw %in% c("good", "bad"))) {
  data$Risk_num <- ifelse(risk_raw == "good", 0, 1)
} else {

  data$Risk_num <- as.numeric(as.factor(data$Risk)) - 1
}

# Removing original Risk column
predictors <- setdiff(names(data), c("Risk", "Risk_num"))


for (col in predictors) {
  if (is.character(data[[col]])) {
    data[[col]] <- as.factor(data[[col]])
  }
}

df <- data[, c(predictors, "Risk_num")]


depths <- 1:10
n_iter <- 25
n <- nrow(df)

mse_results <- matrix(NA, nrow = n_iter, ncol = length(depths))

set.seed(123)


for (iter in 1:n_iter) {

  train_idx <- sample.int(n, size = round(0.7 * n))
  train <- df[train_idx, ]
  test  <- df[-train_idx, ]

  for (i in seq_along(depths)) {

    model <- rpart(
      Risk_num ~ .,
      data = train,
      method = "anova",
      control = rpart.control(maxdepth = depths[i])
    )

    pred <- predict(model, test)
    mse_results[iter, i] <- mean((pred - test$Risk_num)^2)
  }
}


mean_mse <- colMeans(mse_results)
sd_mse <- apply(mse_results, 2, sd)

print(data.frame(depth = depths, mean_mse, sd_mse))


plot(
  depths, mean_mse, type = "l", lwd = 2,
  xlab = "Tree Depth", ylab = "MSE",
  main = "CART Regression: MSE vs Tree Depth"
)

polygon(
  c(depths, rev(depths)),
  c(mean_mse - sd_mse, rev(mean_mse + sd_mse)),
  col = adjustcolor("blue", alpha.f = 0.3),
  border = NA
)

lines(depths, mean_mse, lwd = 2)
points(depths, mean_mse, pch = 19)


best_depth <- depths[which.min(mean_mse)]
cat("Best depth =", best_depth, "\n")

# Regression tree

final_model <- rpart(
  Risk_num ~ .,
  data = df,
  method = "anova",
  control = rpart.control(maxdepth = best_depth)
)


cat("\nVariable Importance (Normalized):\n")
var_imp <- final_model$variable.importance
print(round(var_imp / sum(var_imp), 3))

# print the plot

rpart.plot(final_model, main = paste("Final CART Regression Tree (Depth =", best_depth, ")"))

# 3.1.1.

data <- read.csv("BostonHousing.csv")

# For reproducibility
set.seed(123)

# Number of observations
n <- nrow(data)

# Training set 60%
train_index <- sample(1:n, size = 0.6 * n)

# Columns 1–12 (CRIM through LSTAT)
# Used as inputs to the model
train_data <- data[train_index, ]

# Valid data
valid_data <- data[-train_index, ]

# Used as inputs to the model
# Columns 1–12 (CRIM through LSTAT)
x_train <- train_data[, 1:12]

# MEDV (median house value)
# What we are trying to predict
y_train <- train_data$MEDV

# Same separation for the validation set
x_valid <- valid_data[, 1:12]
y_valid <- valid_data$MEDV

# Check the split proportions
nrow(train_data) / nrow(data)  # ~0.6
nrow(valid_data) / nrow(data)  # ~0.4

# 3.1.1.1.

# Declaring a new functions for data normalizing
normalize_train <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

normalize_test <- function(x, train_col) {
  (x - min(train_col)) / (max(train_col) - min(train_col))
}

# Normalize training data
x_train_norm <- as.data.frame(lapply(x_train, normalize_train))

# Normalize validation data using training min/max
x_valid_norm <- as.data.frame(
  Map(normalize_test, x_valid, x_train)
)


library(class)

rmse <- numeric(5)

#k = 1 to 5
for (k in 1:5) {
  pred <- class::knn(
    train = x_train_norm,
    test  = x_valid_norm,
    cl    = y_train,
    k     = k
  )
  
  pred <- as.numeric(as.character(pred))
  rmse[k] <- sqrt(mean((pred - y_valid)^2))
}

# The best k is the one with the smallest RMSE.
# Returned 1 for me
best_k <- which.min(rmse)

# 3.1.1.2.

new_tract <- data.frame(
  CRIM = 0.2,
  ZN = 0,
  INDUS = 7,
  CHAS = 0,
  NOX = 0.538,
  RM = 6,
  AGE = 62,
  DIS = 4.7,
  RAD = 4,
  TAX = 307,
  PTRATIO = 21,
  LSTAT = 10
)

# Normalize the new tract using training data
normalize_new <- function(x, train_col) {
  (x - min(train_col)) / (max(train_col) - min(train_col))
}

new_tract_norm <- as.data.frame(
  Map(normalize_new, new_tract, x_train)
)

# Predict MEDV using k-NN
predicted_MEDV <- class::knn(
  train = x_train_norm,
  test  = new_tract_norm,
  cl    = y_train,
  k     = best_k
)

predicted_MEDV <- as.numeric(as.character(predicted_MEDV))
# Predicted MEDV is 20.4
predicted_MEDV






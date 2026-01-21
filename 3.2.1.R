library(caret)
library(FNN)
library(ggplot2)
library(dplyr)

# Reading data set into variable from csv file
firewall.rules.df <- read.csv("Firewall_Rule_Classification.csv")

# Check how data set looks
View(firewall.rules.df)

# Remove first column of data set
# This column only contains row numbering and does not add predictive value
firewall.rules.df <- firewall.rules.df[ , -1]

# Check for NA values and remove them
colSums(is.na(firewall.rules.df))
firewall.rules.df <- na.omit(firewall.rules.df)

# Convert Class to factor
# KNN classification requires categorical class labels
firewall.rules.df$Class <- as.factor(firewall.rules.df$Class)

# Setting seed for random variables reuse later on, required for consistency of results
set.seed(0)

# Separation in training set (60%) and validation set (40%)
# In class we used dim that returns vector c(number of rows, number of columns) and picked first value
# nrow essentially does the same, but looks cleaner, therefore we used this option
train.index <- sample(row.names(firewall.rules.df), 0.6*nrow(firewall.rules.df))
valid.index <- setdiff(row.names(firewall.rules.df), train.index)  

# Creation of training and validation data sets
train.df <- firewall.rules.df[train.index, ]
valid.df <- firewall.rules.df[valid.index, ]

# Check for training data set structure
str(train.df)
# Simple summary statistics of all variables in data set
summary(train.df)

# As for the visualization, it was hard to pick one, 
# because data set has a lot of different columns and we wanted to create very clear and understandable graphics
# Scatter plot used in class example would not match that, therefore we picked a simple barplot visualization
barplot(table(train.df$Class),
        main = "Firewall Rule Class distribution",
        xlab = "Class",
        ylab = "Count")

# Additional histogram dedicated to packets
hist(train.df$Packets,
     breaks = 50,
     main = "Distribution of Packets",
     xlab = "Packets")

# Gggplot for destination port per class, because we thought it would be interesting to see which ones are blocked, allowed, etc.
# Used x as Destination.Port and Class as Fill
# position = "dodge" - to ensure that bars do not overlap
# bins = 50 - how precisely is divided into intervals destination.port
# alpha = 0.7 - transparency of bars (to look good)
# labs - aids with titling
# theme_minimal() - applied minimal theme for histogram to look good
ggplot(train.df, aes(x = Destination.Port, fill = Class)) +
  geom_histogram(position = "dodge", bins = 50, alpha = 0.7) +
  labs(title = "Distribution of destination ports by firewall rules class",
       x = "Destination Port",
       y = "Count") +
  theme_minimal()

# Save unique classes values in variable, show in table how many are of those
unique.classes <- unique(train.df$Class)
X <- length(unique.classes)
table(train.df$Class)

# We need row id particularly in this case to filter out records
# We know we had X before and removed it, but this is the only point where it might be useful
train.df <- train.df %>%
  mutate(row_id = row_number())

# Pick one record for each Class type
examples.df <- train.df %>%
  group_by(Class) %>%
  slice(1) %>%
  ungroup()

# Remove retrieved records from training data set
train.df <- train.df %>% 
  filter(!row_id %in% examples.df$row_id) %>% 
  select(-row_id)

# Check if operation was successful
View(examples.df)

# Identify numeric columns
# Only numeric features are used in distance-based KNN
num.cols <- names(firewall.rules.df)[sapply(firewall.rules.df, is.numeric)]

# Initialize normalized data sets
train.norm.df <- train.df
valid.norm.df <- valid.df

# Normalize numeric columns (applied only to training data)
norm.values <- caret::preProcess(train.df[, num.cols, drop=FALSE], method=c("center", "scale"))

# Apply normalization to training and validation data
train.norm.df[, num.cols] <- predict(norm.values, train.df[, num.cols, drop=FALSE])
valid.norm.df[, num.cols] <- predict(norm.values, valid.df[, num.cols, drop=FALSE])

# Prepare variable to store accuracy results for different k values
accuracy.df <- data.frame(k = seq(1, 15, 1), accuracy = rep(0, 15))

# Ensure factor levels are consistent between train and validation sets
# Otherwise KNN breaks
all_levels <- levels(factor(firewall.rules.df$Class))
train_class <- factor(train.norm.df$Class, levels = all_levels)
valid_class <- factor(valid.norm.df$Class, levels = all_levels)

# Loop through k = 1 to 15
# Train KNN model and calculate accuracy for each k
for(i in 1:15) {
  knn.pred <- FNN::knn(
    train = train.norm.df[, num.cols, drop=FALSE],
    test = valid.norm.df[, num.cols, drop=FALSE],
    cl = train_class,
    k = i
  )
  # Convert prediction to factor with all class levels
  knn.pred.fct <- factor(knn.pred, levels = all_levels)
  
  # Calculate accuracy using confusion matrix
  accuracy.df$accuracy[i] <- confusionMatrix(knn.pred.fct, valid_class)$overall["Accuracy"]
}

# Find index of best-performing k
best_index <- which.max(accuracy.df$accuracy)

# best k value
best_k <- accuracy.df$k[best_index]

# precision for best k
best_accuracy <- accuracy.df$accuracy[best_index]

best_k
best_accuracy

# Similarly take only numeric columns
examples.num <- examples.df[, num.cols, drop=FALSE]

# In the same manner normalize them
examples.norm <- predict(norm.values, examples.num)

# Predict class labels for example records using best k
example_preds <- FNN::knn(
  train = train.norm.df[, num.cols, drop=FALSE],
  test = examples.norm,
  cl = train_class,
  k = best_k
)

# Convert predictions to factor with correct levels
example_preds <- factor(example_preds, levels = levels(train_class))

# Ensure actual Class labels use same factor levels
examples.df$Class <- factor(examples.df$Class, levels = levels(train_class))

# Table with results, combined predictions with original examples
examples_with_preds <- examples.df %>%
  mutate(
    Predicted = example_preds,
    Correct = Predicted == Class
  )

# View results
View(examples_with_preds)

# Calculate accuracy on example records
example_accuracy <- mean(examples_with_preds$Correct)
print(example_accuracy)

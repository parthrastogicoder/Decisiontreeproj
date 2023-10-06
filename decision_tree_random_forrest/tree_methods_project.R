# Load Libraries
library(ISLR)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)

# Check the head of College data
head(College)

# Assign College data to a variable
df <- College

# Exploratory Data Analysis (EDA)
# Scatterplot of Grad.Rate vs Room.Board
ggplot(df, aes(Room.Board, Grad.Rate, color = Private)) +
  geom_point() +
  labs(title = "Scatterplot of Grad.Rate vs Room.Board",
       x = "Room and Board Costs",
       y = "Graduation Rate",
       color = "Private Status")

# Histogram of full-time undergrad students
ggplot(df, aes(F.Undergrad, fill = Private)) +
  geom_histogram(color = 'black', bins = 50) +
  labs(title = "Histogram of Full-time Undergrad Students",
       x = "Number of Full-time Undergraduates",
       y = "Frequency",
       fill = "Private Status") +
  theme_minimal()

# Histogram of Grad.Rate
ggplot(df, aes(Grad.Rate, fill = Private)) +
  geom_histogram(color = 'black', bins = 50) +
  labs(title = "Histogram of Graduation Rates",
       x = "Graduation Rate",
       y = "Frequency",
       fill = "Private Status") +
  theme_minimal()

# Identify and correct erroneous entries in Grad.Rate
df[df$Grad.Rate > 100, 'Grad.Rate'] <- 100

# Train-Test Split
set.seed(101)
sample <- sample.split(df$Private, SplitRatio = 0.70)
train <- subset(df, sample == TRUE)
test <- subset(df, sample == FALSE)

# Decision Tree
tree <- rpart(Private ~ ., method = 'class', data = train)
tree.preds <- predict(tree, test, type = 'prob')
tree.preds <- as.data.frame(tree.preds)
tree.preds$Private <- ifelse(tree.preds$Yes >= 0.5, 'Yes', 'No')

# Confusion matrix for the decision tree model
confusion_tree <- table(tree.preds$Private, test$Private)
print("Confusion Matrix - Decision Tree:")
print(confusion_tree)

# Plot the decision tree
prp(tree, main = "Decision Tree for Private/Non-Private Classification")

# Random Forest
rf.model <- randomForest(Private ~ ., data = train, importance = TRUE)

# Confusion matrix for the random forest model
confusion_rf <- rf.model$confusion
print("Confusion Matrix - Random Forest:")
print(confusion_rf)

# Feature importance
importance <- rf.model$importance
print("Feature Importance:")
print(importance)

# Predictions using the random forest model
rf_preds <- predict(rf.model, test)
confusion_rf_preds <- table(rf_preds, test$Private)
print("Confusion Matrix - Random Forest Predictions:")
print(confusion_rf_preds)

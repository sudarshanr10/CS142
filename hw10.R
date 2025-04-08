install.packages(c("e1071", "caret"))
library(e1071)
library(caret)

train_data <- read.csv('training_email_advertising.csv', header = TRUE)
test_data <- read.csv('test_email_advertising.csv', header = TRUE)

train_data$none_open_buy <- as.factor(train_data$none_open_buy)
test_data$none_open_buy <- as.factor(test_data$none_open_buy)

nb_model = naiveBayes(none_open_buy ~., data = train_data)

train_predictions <-predict(nb_model, newdata = train_data)
test_predictions <- predict(nb_model, newdata = test_data)
train_conf_matrix <- confusionMatrix(train_predictions, train_data$none_open_buy)
test_conf_matrix <- confusionMatrix(test_predictions, test_data$none_open_buy)

cat("\n--- Confusion Matrix: Training Data --- \n")
print(train_conf_matrix)

cat("\n--- Confusion Matrix: Test Data --- \n")
print(test_conf_matrix)


library(faraway)
data(pima)

test0data <- subset(pima, test == 0)
mean_diabetes_test0 <- mean(test0data$diabetes)

test1data <- subset(pima, test == 1)
mean_diabetes_test1 <- mean(test1data$diabetes)

cat("Mean of diabetes variable for test = 0:", mean_diabetes_test0)
cat("Mean of diabetes variable for test = 1:", mean_diabetes_test1)

library(faraway)
data(pima)

bmi_summary <- summary(pima$bmi)
bmi_summary

q1_threshold <- bmi_summary[2]
q4_threshold <- bmi_summary[5]

q1Subset <- subset(pima, bmi <= q1_threshold)
q4Subset <- subset(pima, bmi > q4_threshold)

mean_diabetes_q1 <- mean(q1Subset$diabetes)
mean_diabetes_q4 <- mean(q4Subset$diabetes)

cat("Mean diabetes for Q1:", mean_diabetes_q1)
cat("Mean diabetes for Q4:", mean_diabetes_q4)

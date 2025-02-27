library(readr)
data <- read_csv('hw5_data.csv')

sample_mean <- mean(data$diaBP)
sample_sd <- sd(data$diaBP)
n <- length(data$diaBP)

error <- qt(0.975, df = n-1) * sample_sd / sqrt(n)
c_lower <- sample_mean - error
c_upper <- sample_mean + error

margin <- 5
z_value <- qnorm(0.975)
sample_size <- (z_value * sample_sd / margin) ^ 2

cat("95% Confidence Interval for the Mean: (", c_lower, ", ", c_upper, ")\n")
cat("Margin of Error: ", error, "\n")
cat("Sample Size Needed For Margin of Error Less than 5 Units: ", ceiling(sample_size) ,"\n")

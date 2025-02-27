library(readr)
data <- read_csv('hw6_data.csv')

phat <- mean(data$isFemale)
n <- nrow(data)

std_error <- sqrt(phat * (1-phat) / n)
z_value <- qnorm(0.975)
c_lower <- phat - (z_value*std_error)
c_upper <- phat + (z_value*std_error)
margin_of_error <- z_value * std_error

desired_error <- 0.05
sample_size <- (z_value * sqrt(phat * (1-phat)) / desired_error) ^ 2

cat("95% Confidence Interval for the Proportion of Females: (", c_lower, ", ", c_upper, ")\n")
cat("Margin of Error: ", margin_of_error, "\n")
cat("Sample Size Required For Margin of Error Less than 5%: ", ceiling(sample_size), "\n")



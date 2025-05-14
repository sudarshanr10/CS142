
isEvent <- function(numDice, numSides, target, numTrials){
  apply(
    matrix(sample(1:numSides, numDice * numTrials, replace = TRUE),
           nrow = numDice),
    2, sum
  ) >= target
}

trial_sizes <- c(5000, 25000, 100000)

cat("---- Part 1: P(sum>=4) ----\n")
set.seed(12345)
for (n in trial_sizes){
  out <- isEvent(2, 6, 4, n)
  estimate <- mean(out)
  cat(sprintf("Trials = %6d \t Estimate = %.4f\n", n, estimate))
}

cat("\n---- Part2: P(sum>=11) ----\n")
set.seed(13245)
for (n in trial_sizes){
  out <- isEvent(2, 6, 11, n)
  estimate <- mean(out)
  cat(sprintf("Trials = %6d \t Estimate = %.4f\n", n, estimate))
}

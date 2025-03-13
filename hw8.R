library(ggplot2)
library(EnvStats)

set.seed(12345)

sample1 <- runif(9,0,1)
sample2 <- runif(9,0,1.4)

qqnorm(sample1, main = "Q-Q Plot for Sample 1", datax = TRUE)
qqline(sample1, col = "steelblue", datax = TRUE)

qqnorm(sample2, main = "Q-Q Plot for Sample 2", datax = TRUE)
qqline(sample2, col = "steelblue", datax = TRUE)

t_test_result <- t.test(sample1, sample2, var.equal = TRUE)
cat("p-value from t-test:", t_test_result$p.value)

n_permutations <- choose(18, 9)
permutation_test_result <- twoSamplePermutationTestLocation(x = sample1, y = sample2, 
                                                           fcn = "mean", alternative = "two.sided",
                                                           mu1.minus.mu2 = 0, paired = FALSE, 
                                                           exact = FALSE, n.permutations = n_permutations,
                                                           seed = 123)
cat("p-value from permutation Test:",permutation_test_result$p.value)
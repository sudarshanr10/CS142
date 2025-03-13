library(ggplot2)
library(EnvStats)

set.seed(12345)

sample1 <- rnorm(10, mean = 4, sd =1)
sample2 <- rnorm(10, mean = 4.4, sd = 1)

qqnorm(sample1, main = "Q-Q Plot for Sample 1")
qqline(sample1, col = 'steelblue', lwd =2)

qqnorm(sample2, main = "Q-Q Plot For Sample 2")
qqline(sample2, col = 'steelblue', lwd = 2)

t_test_result <- t.test(sample1, sample2, alternative = 'two.sided')
cat("p-value from t-test:", t_test_result$p.value)


n_permutations <- choose(20,10)
permutation_test_result <- twoSamplePermutationTestLocation(x = sample1, y = sample2, 
                                                            fcn = "mean", alternative = "two.sided",
                                                            mu1.minus.mu2 = 0, paired = FALSE, 
                                                            exact = FALSE, n.permutations = n_permutations,
                                                            seed = 123, tol = sqrt(.Machine$double.eps))
cat("p-value from permutation test:", permutation_test_result$p.value)

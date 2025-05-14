
tab <- matrix(
  c(200688, 24, 33,
    201087, 27, 115),
  nrow=2,
  byrow = TRUE,
  dimnames = list(
    Vaccination = c("Vaccinated", "Placebo"),
    Status = c("NoPolio", "NonParalytic", "Paralytic")
  )
)

chi <- chisq.test(tab, correct = FALSE)
obs_stat <- chi$statistic

cat("Chi-Square Stat: ", obs_stat, "\n")
cat("P-Value (Chi-Square): ", chi$p.value, "\n\n")

set.seed(12345)
B <- 5000
perm_stats <- replicate(B, {
  reshuffled <- sample(c(tab))
  chisq.test(
    matrix(reshuffled, nrow =2, byrow = TRUE), 
    correct = FALSE
  )$statistic
})

perm_p <- mean(perm_stats >= obs_stat)
cat("P-Value (5000 Permutations) :", perm_p, "\n")

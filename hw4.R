if (!require(rpart)) install.packages('rpart', dependencies=TRUE)
library(rpart)

data(stagec)
help(stagec)

#One-way table for the variable pgstat
one_way_pgstat <- table(stagec$pgstat)
cat("One-way table for the variable pgstat:\n")
print(one_way_pgstat)

#Two-way table for grade and pgstat
two_way_grade_pgstat <- table(stagec$grade, stagec$pgstat)
cat("\nTwo-way table for grade and pgstat:\n")
print(two_way_grade_pgstat)

#Subset of stagec where eet is 1 (for patients without early endocrinen therapy)
subset_stagec <- stagec[stagec$eet == 1, ]

#Summary of pgtime in the subset
cat("\nSummary of pgtime in the subset:\n")
print(summary(subset_stagec$pgtime))

#One-way table for pgstat in the subset
one_way_pgstat_subset <- table(subset_stagec$pgstat)
cat("\nOne-way table for pgstat in the subset:\n")
print(one_way_pgstat_subset)

#Two-Way table for grade and pgstat in the subset
two_way_grade_pgstat_subset <- table(subset_stagec$grade, subset_stagec$pgstat)
cat("\nTwo-way table for grade and pgstat in the subset:\n")
print(two_way_grade_pgstat_subset)
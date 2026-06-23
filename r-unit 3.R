#unit-3

# 1. Mean of Votes
mean(data$Votes, na.rm=TRUE)


# 2. Median of Votes
median(data$Votes, na.rm=TRUE)


# 3. Mode function (R me built-in mode nahi hota)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
get_mode(data$Votes)


# 4. Count of candidates per Party
table(data$Party.Name)


# 5. Proportion of candidates per Party
prop.table(table(data$Party.Name)) * 100


# 6. Summary Statistics (Min, Max, Mean, Median)
summary(data$Votes)


# 7. Quantiles / Percentiles
quantile(data$Votes, probs=c(0.25,0.5,0.75), na.rm=TRUE)


# 8. Variance of Votes
var(data$Votes, na.rm=TRUE)


# 9. Standard Deviation of Votes
sd(data$Votes, na.rm=TRUE)


# 10. Interquartile Range (IQR)
IQR(data$Votes, na.rm=TRUE)


# 11. Covariance (Votes vs Total Valid Votes)
cov(data$Votes, data$Total.Valid.Votes, use="complete.obs")


# 12. Correlation (Votes vs Total Valid Votes)
cor(data$Votes, data$Total.Valid.Votes, use="complete.obs")




# 15. Aggregate: Total Votes per State
aggregate(Votes ~ State.Name, data, sum)

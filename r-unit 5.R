#data visualisation
# UNIT 5 – DATA VISUALIZATION
# CLOSE ANY OLD GRAPHIC DEVICE (ignore warning if any)
par(mar = c(5, 5, 4, 2) + 0.1)
par(pin = c(5, 5))
x11()



# 1. Histogram of Votes


hist(
  data$Votes,
  main="Distribution of Votes",
  xlab="Votes",
  ylab="Frequency",
  col="lightblue",
  border="black"
)






# 3. Barplot: Count of Candidates per Party

party_counts <- table(data$Party.Name)
barplot(
  party_counts,
  main="Number of Candidates per Party",
  xlab="Party Name",
  ylab="Count",
  col="lightgreen",
  las=2
)


# 4. Pie Chart: Party Distribution
party_counts <- table(data$Party.Name)

pct <- round(party_counts / sum(party_counts) * 100, 1)
labels <- paste(names(party_counts), "(", pct, "%)", sep="")

pie(
  party_counts,
  labels = labels,
  main = "Party-wise Candidate Distribution",
  col = colorRampPalette(c("skyblue", "dodgerblue4"))(length(party_counts)),
  cex = 0.7
)





plot(
  data$Total.Valid.Votes,
  data$Votes,
  main="Scatter Plot: Votes vs Total Valid Votes",
  xlab="Total Valid Votes",
  ylab="Votes",
  pch=19,
  col="purple"
)


# 6. Line Plot: Votes of First 20 Candidates

plot(
  1:20,
  data$Votes[1:20],
  type="o",
  main="Votes of First 20 Candidates",
  xlab="Candidate Index",
  ylab="Votes",
  col="blue"
)


# 7. Heatmap (Correlation Heatmap)
numeric_cols <- sapply(data, is.numeric)
corr_matrix <- cor(data[, numeric_cols], use="complete.obs")

heatmap(
  corr_matrix,
  main="Correlation Heatmap",
  Colv=NA,
  Rowv=NA,
  col=heat.colors(256),
  scale="none"
)


# 8. Barplot: Total Votes per State

state_votes <- aggregate(Votes ~ State.Name, data, sum)
barplot(
  state_votes$Votes,
  names.arg=state_votes$State.Name,
  main="Total Votes per State",
  xlab="State",
  ylab="Total Votes",
  col="pink",
  las=2
)


# 9. Boxplot: Votes per Party

boxplot(
  Votes ~ Party.Name,
  data=data,
  main="Votes by Party",
  xlab="Party",
  ylab="Votes",
  col="lightcyan",
  las=2
)


# 10. Pie Chart: Status (Won / Lost) Distribution

status_counts <- table(data$Status)
pie(
  status_counts,
  main="Status Distribution (Won vs Lost)",
  col=c("lightgreen", "red")
)


#ggplot
library(ggplot2)

ggplot(data, aes(x=Total.Valid.Votes, y=Votes)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Votes vs Total Valid Votes (ggplot2)")




#ggplot-barplot top -10 parties

party_top <- sort(table(data$Party.Name), decreasing = TRUE)[1:10]
party_top_df <- data.frame(
  Party = names(party_top),
  Count = as.numeric(party_top)
)

ggplot(party_top_df, aes(x=Party, y=Count)) +
  geom_col() +
  ggtitle("Top 10 Parties by Candidate Count") +
  theme(axis.text.x = element_text(angle=90))





## scatter plot 
ggplot(data, aes(Total.Valid.Votes, Votes, color=Status)) +
  geom_point() +
  ggtitle("Votes vs Total Valid Votes by Status")


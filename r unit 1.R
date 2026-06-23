#read data
data<-read.csv("~/r dataset.csv")

#view data 
View(data)

#rows in data
nrow(data)

#col
ncol(data)

#dimensions
dim(data)

#column names
colnames(data)


#structure 
str(data)

#summary
summary(data)

#missing value per column
colSums(is.na(data))

#total missing value
sum(is.na(data))

#duplicate rows
sum(duplicated(data))

#first 10 rows
head(data, 10)
# Last 10 Rows
tail(data, 10)

# Unique Values of Key Columns
unique(data$State.Name)
unique(data$Party.Name)
unique(data$Status)

# Check Data Types of All Columns
sapply(data, class)

# Convert Column to Factor
data$Party.Name <- as.factor(data$Party.Name)

# Convert Column to Numeric
data$Votes <- as.numeric(data$Votes)

# Filter Rows by Condition
data[data$Votes > 50000, ]

# Sort Dataset by Column
data[order(data$Votes, decreasing = TRUE), ]

# Add a New Column
data$Vote_Percentage <- (data$Votes / sum(data$Votes)) * 100

# Remove Column
data$Vote_Percentage <- NULL

# Remove Duplicate Rows
data <- data[!duplicated(data), ]

# Remove Rows with NA Values
data <- na.omit(data)

# Creating a Vector (Candidate Names)
candidate_names <- data$Candidate.Name

# Sequence (Row numbers)
seq_rows <- seq(1, nrow(data), by=2)


# Repetition (First 3 Parties repeated 3 times)
rep_parties <- rep(data$Party.Name[1:3], times=3)

# Sorting Vector (Votes of first 10 candidates)
sort_votes <- sort(data$Votes[1:10], decreasing=TRUE)

# Length of Vector
length(candidate_names)

# Matrix Creation (Votes of first 6 candidates)
votes_matrix <- matrix(data$Votes[1:6], nrow=2)

# Matrix Transpose
t(votes_matrix)

# Matrix Multiplication (Votes matrix)
votes_matrix %*% t(votes_matrix)


# Correlation and Aggregation (Votes by Party)
aggregate(Votes ~ Party.Name, data, sum)
cor(data$Votes, data$Votes)




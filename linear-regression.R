#unit-4
#algo



library(caret)
library(ggplot2)
library(reshape2)  # for bar chart

# ===============================
## 1️⃣ Ensure numeric columns
# ===============================
data$Votes <- as.numeric(data$Votes)
data$Total.Valid.Votes <- as.numeric(data$Total.Valid.Votes)
data$Total.Electors.In.Constituency <- as.numeric(data$Total.Electors.In.Constituency)

# ===============================
## 2️⃣ Build Linear Regression model
# ===============================
lr_model <- lm(
  Votes ~ Total.Valid.Votes + Total.Electors.In.Constituency,
  data = data
)

# Model summary
summary(lr_model)

# ===============================
## 3️⃣ Make predictions on original dataset
# ===============================
data$Predicted_Votes <- predict(lr_model, data)

# ===============================
## 4️⃣ Evaluate model performance
# ===============================
rmse <- sqrt(mean((data$Votes - data$Predicted_Votes)^2))
rsq <- summary(lr_model)$r.squared

cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R-squared:", rsq, "\n")

# ===============================
## 5️⃣ Predict for new candidates
# ===============================
new_candidates <- data.frame(
  Total.Valid.Votes = c(2000000, 1500000, 2500000),
  Total.Electors.In.Constituency = c(2500000, 1800000, 3000000)
)

new_predicted_votes <- predict(lr_model, new_candidates)
print("Predicted Votes for new candidates:")
print(new_predicted_votes)


# Ensure predictions exist
data$Predicted_Votes <- predict(lm(Votes ~ Total.Valid.Votes + Total.Electors.In.Constituency, data = data), data)

# Scatter plot: Actual vs Predicted
ggplot(data, aes(x = Predicted_Votes, y = Votes)) +
  geom_point(color = "blue", alpha = 0.6) +                     # predicted vs actual points
  geom_abline(intercept = 0, slope = 1, color = "red",          # perfect prediction line
              linetype = "dashed", linewidth = 1) +
  labs(
    title = "Linear Regression: Actual vs Predicted Votes",
    x = "Predicted Votes",
    y = "Actual Votes"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )

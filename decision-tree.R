library(rpart)
library(rpart.plot)
library(caret)

# ===============================
# 2️⃣ Prepare data
# ===============================
# Convert target variable to factor
data$Status <- as.factor(data$Status)  # Replace 'Status' with your target column name

# ===============================
# 3️⃣ Build Decision Tree
# ===============================
dt_model <- rpart(
  Status ~ Votes + Total.Valid.Votes,  # Features for prediction
  data = data,
  method = "class"
)

# ===============================
# 4️⃣ Visualize the tree
# ===============================
rpart.plot(
  dt_model,
  type = 4,           # Fancy tree
  extra = 104,        # Display class & probability
  fallen.leaves = TRUE,
  faclen = 0,         # Full names of factors
  cex = 0.75          # Font size
)

# ===============================
# 5️⃣ Make predictions
# ===============================
data$DT_Predicted_Status <- predict(dt_model, data, type = "class")

# ===============================
# 6️⃣ Evaluate model
# ===============================
dt_conf_mat <- confusionMatrix(data$DT_Predicted_Status, data$Status)
print("Decision Tree Confusion Matrix & Accuracy:")
print(dt_conf_mat)

# ===============================
# 7️⃣ Predict new candidates (optional)
# ===============================
new_candidates <- data.frame(
  Votes = c(500000, 150000, 700000),
  Total.Valid.Votes = c(2000000, 1500000, 2500000)
)

dt_pred_new <- predict(dt_model, new_candidates, type = "class")
dt_prob_new <- predict(dt_model, new_candidates, type = "prob")

print("Decision Tree predictions for new candidates:")
print(dt_pred_new)
print(dt_prob_new)
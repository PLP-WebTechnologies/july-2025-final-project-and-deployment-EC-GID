library(minpack.lm)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(Metrics)
library(reshape2)

tree_data <- read.csv("E:/My_website/Data/lysvolume.csv")
head(tree_data)

# Scatter plots
# Volume vs Total Height
p1 <- ggplot(tree_data, aes(x = H, y = V)) +
  geom_point() +
  labs(title = "Volume vs Total Height", x = "Total Height (H)", y = "Volume (V)")

# Volume vs Diameter
p2 <- ggplot(tree_data, aes(x = D, y = V)) +
  geom_point() +
  labs(title = "Volume vs Diameter", x = "Diameter at Breast Height (D)", y = "Volume (V)")

# Total Height vs Diameter
p3 <- ggplot(tree_data, aes(x = D, y = H)) +
  geom_point() +
  labs(title = "Total Height vs Diameter", x = "Diameter at Breast Height (D)", y = "Total Height (H)")

grid.arrange(p1, p2, p3, nrow = 2)


# Extract variables
H <- tree_data$H
D <- tree_data$D

# Define models
models <- list()

# 1. Exponential Function
models$exp <- nlsLM(H ~ a * exp(-b / (D + c)),
                    start = list(a = max(H), b = 1, c = 1),
                    control = nls.lm.control(maxiter = 100))

# 2. Lundqvist Function
models$lundqvist <- nlsLM(H ~ a * exp(-b * D^(-c)),
                          start = list(a = max(H), b = 0.1, c = 0.5))

# 3. Modified Logistic Function
models$logistic <- nlsLM(H ~ a / (1 + (1 / b) * D^(-c)),
                         start = list(a = max(H), b = 1, c = 0.5))

# 4. Richards Function
models$richards <- nlsLM(H ~ a * ((1 - exp(-b * D))^c),
                         start = list(a = max(H), b = 0.01, c = 1))

# 5. Weibull Function
models$weibull <- nlsLM(H ~ a * (1 - exp(-b * D^c)),
                        start = list(a = max(H), b = 0.01, c = 1))

# Function to calculate goodness-of-fit statistics
gof_stats <- function(model, data) {
  pred <- predict(model)
  obs <- tree_data$H
  n <- length(obs)
  mean_H <- mean(obs)
  
  ME <- mean(obs - pred)
  MAE <- mean(abs(obs - pred))
  MPE <- mean((obs - pred) / obs) * 100
  MAPE <- mean(abs((obs - pred) / obs)) * 100
  RMSE <- sqrt(mean((obs - pred)^2))
  RMSPE <- sqrt(mean(((obs - pred) / obs)^2)) * 100
  CE <- 1 - sum((obs - pred)^2) / sum((obs - mean_H)^2)
  
  return(data.frame(ME, MAE, MPE, MAPE, RMSE, RMSPE, CE))
}

# Evaluate all models
results <- lapply(models, gof_stats, data = tree_data)
gof_table <- do.call(rbind, results)
rownames(gof_table) <- names(models)

# Print parameter estimates and significance
cat("=== Parameter Estimates and Significance Tests ===\n")
for (name in names(models)) {
  cat("\nModel:", name, "\n")
  print(summary(models[[name]]))
}

# Print goodness-of-fit statistics
cat("\n=== Goodness-of-Fit Summary ===\n")
print(round(gof_table, 4))


# Fit the five height-diameter models
models <- list()

models$Exponential <- nlsLM(H ~ a * exp(-b / (D + c)),
                            data = tree_data,
                            start = list(a = max(tree_data$H), b = 1, c = 1))

models$Lundqvist <- nlsLM(H ~ a * exp(-b * D^(-c)),
                          data = tree_data,
                          start = list(a = max(tree_data$H), b = 0.1, c = 0.5))

models$Logistic <- nlsLM(H ~ a / (1 + (1 / b) * D^(-c)),
                         data = tree_data,
                         start = list(a = max(tree_data$H), b = 1, c = 0.5))

models$Richards <- nlsLM(H ~ a * (1 - exp(-b * D^c)),
                         data = tree_data,
                         start = list(a = max(tree_data$H), b = 0.01, c = 1))

models$Weibull <- nlsLM(H ~ a * (1 - exp(-b * D^c)),
                        data = tree_data,
                        start = list(a = max(tree_data$H), b = 0.01, c = 1))

# Create a sequence of DBH values for predictions
dbh_seq <- seq(min(tree_data$D), max(tree_data$D), length.out = 200)
prediction_df <- data.frame(D = dbh_seq)

# Generate predictions from each model
for (name in names(models)) {
  prediction_df[[name]] <- predict(models[[name]], newdata = data.frame(D = dbh_seq))
}

# Reshape for ggplot
long_pred_df <- melt(prediction_df, id.vars = "D", variable.name = "Model", value.name = "H")

# Plot: Observed data + all model curves
ggplot() +
  geom_point(data = tree_data, aes(x = D, y = H), color = "black", alpha = 0.6, size = 2) +
  geom_line(data = long_pred_df, aes(x = D, y = H, color = Model), size = 1.2) +
  scale_color_manual(values = c("Exponential" = "red",
                                "Lundqvist" = "blue",
                                "Logistic" = "green",
                                "Richards" = "orange",
                                "Weibull" = "purple")) +
  labs(title = "Observed Tree Heights with Fitted Height-Diameter Models",
       x = "DBH (cm)", y = "Tree Height (m)", color = "Model") +
  theme_minimal(base_size = 14)


# Check for outliers (e.g., very high or low values for D and V)
boxplot(tree_data$D, main = "Boxplot for Diameter (D)")
boxplot(tree_data$V, main = "Boxplot for Volume (V)")



# 1. Power model for Volume
model_v1 <- nlsLM(V ~ a * D^b, data = tree_data, start = list(a = 1, b = 0.5), control = nls.lm.control(maxiter = 500))

# 2. Exponential model for Volume
model_v2 <- nlsLM(V ~ a * (1 - exp(-b * D)), data = tree_data, start = list(a = 100, b = 0.1), control = nls.lm.control(maxiter = 500))
# 3. Logarithmic model for Volume
model_v3 <- nlsLM(V ~ a + b * log(D), data = tree_data, start = list(a = 1, b = 1), control = nls.lm.control(maxiter = 500))

# 4. Weibull model for Volume
model_v4 <- nlsLM(V ~ a * (1 - exp(-(D/b)^c)), data = tree_data, start = list(a = 100, b = 10, c = 1), control = nls.lm.control(maxiter = 500))

# 5. Sigmoid (Chapman-Richards like) model for Volume
model_v5 <- nlsLM(V ~ a * (1 - exp(-b * D))^c, data = tree_data, start = list(a = 100, b = 0.1, c = 1), control = nls.lm.control(maxiter = 500))

# View model summaries
summary(model_v1)
summary(model_v2)
summary(model_v3)
summary(model_v4)
summary(model_v5)

calculate_metrics <- function(observed, predicted) {
  ME <- mean(predicted - observed)
  MAE <- mean(abs(predicted - observed))
  MPE <- mean((predicted - observed) / observed) * 100
  MAPE <- mean(abs((predicted - observed) / observed)) * 100
  RMSE <- sqrt(mean((predicted - observed)^2))
  RMSPE <- sqrt(mean(((predicted - observed) / observed)^2)) * 100
  CE <- 1 - sum((observed - predicted)^2) / sum((observed - mean(observed))^2)
  return(c(ME = ME, MAE = MAE, MPE = MPE, MAPE = MAPE, RMSE = RMSE, RMSPE = RMSPE, CE = CE))
}
# Ensure that observed and predicted values are numeric
observed_values <- as.numeric(tree_data$V)
# Predicted values for each model
predicted_values_v1 <- as.numeric(predict(model_v1, newdata = tree_data))
predicted_values_v2 <- as.numeric(predict(model_v2, newdata = tree_data))
predicted_values_v3 <- as.numeric(predict(model_v3, newdata = tree_data))
predicted_values_v4 <- as.numeric(predict(model_v4, newdata = tree_data))
predicted_values_v5 <- as.numeric(predict(model_v5, newdata = tree_data))

# Calculate metrics for each model
metrics_v1 <- calculate_metrics(observed_values, predicted_values_v1)
metrics_v2 <- calculate_metrics(observed_values, predicted_values_v2)
metrics_v3 <- calculate_metrics(observed_values, predicted_values_v3)
metrics_v4 <- calculate_metrics(observed_values, predicted_values_v4)
metrics_v5 <- calculate_metrics(observed_values, predicted_values_v5)

# Create a table of the metrics
Metrics_table <- rbind(
  Power_model = unlist(metrics_v1),
  Exponential_model = unlist(metrics_v2),
  Logarithmic_model = unlist(metrics_v3),
  Weibull_model = unlist(metrics_v4),
  Sigmoid_model = unlist(metrics_v5)
)

# View the table
print(Metrics_table)
View(Metrics_table)

# Create transformations and combinations
tree_data <- tree_data %>%
  mutate(
    D_squared = D^2,
    H_squared = H^2,
    DH = D * H,
    D2H = D^2 * H,
    log_D = log(D),
    log_H = log(H)
  )

# View the first few rows again
head(tree_data)

# Fit the Multiple Linear Regression model
mlr_model <- lm(V ~ D + H + D_squared + H_squared + DH + D2H + log_D + log_H, data = tree_data)

# View the summary
summary(mlr_model)
# Predicted Volume values from MLR model
predicted_mlr <- predict(mlr_model, newdata = tree_data)
# Use the same calculate_metrics() function we defined earlier
metrics_mlr <- calculate_metrics(observed = tree_data$V, predicted = predicted_mlr)

# Print MLR model metrics
print(metrics_mlr)

# Combine the metrics
Comparison_table <- rbind(
  Best_Nonlinear_Model = unlist(metrics_v4),  # Assuming Weibull model was best
  New_MLR_Model = unlist(metrics_mlr)
)

# View the comparison
print(Comparison_table)

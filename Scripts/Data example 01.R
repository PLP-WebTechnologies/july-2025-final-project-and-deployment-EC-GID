# Load required packages
library(readxl)
library(ggplot2)
library(minpack.lm)
library(patchwork)

# 1. Data Preparation -----------------------------------------------------
data <- read_excel("E:/R/EVANS.xls", sheet = "Sheet1")
names(data) <- make.names(names(data))

# Data quality check
cat("\n=== DATA SUMMARY ===\n")
print(summary(data[, c("PARi", "Ci", "Photo")]))

# 2. Enhanced Light Response Curve Analysis -------------------------------
light_model <- nlsLM(
  Photo ~ (alpha * PARi * Pmax) / (alpha * PARi + Pmax) - Rd,
  data = data,
  start = list(alpha = 0.05, Pmax = max(data$Photo), Rd = 1),
  control = nls.control(maxiter = 500, warnOnly = TRUE)
)

cat("\n=== LIGHT RESPONSE MODEL ===\n")
print(summary(light_model))

light_plot <- ggplot(data, aes(PARi, Photo)) +
  geom_point(color = "darkgreen", alpha = 0.7, size = 3) +
  geom_smooth(method = "nlsLM", 
              formula = y ~ (alpha * x * Pmax) / (alpha * x + Pmax) - Rd,
              method.args = list(start = coef(light_model)),
              se = FALSE, color = "red", linewidth = 1.2) +
  labs(title = "Light Response Curve",
       subtitle = paste("α =", round(coef(light_model)["alpha"], 3)),
       x = "PARi (μmol photons/m²/s)",
       y = "Photosynthesis (μmol CO₂/m²/s)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# 3. Enhanced CO2 Response Curve Analysis --------------------------------
co2_model <- tryCatch({
  nlsLM(
    Photo ~ (alpha * Ci * Pmax) / (alpha * Ci + Pmax) - Rd,
    data = data,
    start = list(alpha = 0.1, Pmax = max(data$Photo), Rd = 0.1),
    control = nls.control(maxiter = 1000, warnOnly = TRUE)
  )
}, error = function(e) {
  message("Model fitting failed: ", e$message)
  NULL
})
summary(co2_model)
if (!is.null(co2_model)) {
  cat("\n=== CO2 RESPONSE MODEL ===\n")
  print(summary(co2_model))
  
  pred_data <- data.frame(Ci = seq(min(data$Ci), max(data$Ci), length.out = 100))
  pred_data$Photo <- predict(co2_model, newdata = pred_data)
  
  co2_plot <- ggplot(data, aes(Ci, Photo)) +
    geom_point(color = "darkblue", alpha = 0.7, size = 3) +
    labs(title = "CO2 Response Curve",
         subtitle = paste("Pmax =", round(coef(co2_model)["Pmax"], 2)),
         x = "Ci (μmol CO₂/mol air)",
         y = "Photosynthesis (μmol CO₂/m²/s)") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
} else {
  co2_plot <- ggplot(data, aes(Ci, Photo)) +
    geom_point(color = "darkblue") +
    labs(title = "CO2 Response Curve (Model Failed to Fit)",
         x = "Ci (μmol CO₂/mol air)",
         y = "Photosynthesis (μmol CO₂/m²/s)") +
    theme_minimal()
}
# 4. Combined Output -----------------------------------------------------
combined_plots <- light_plot + co2_plot + 
  plot_annotation(tag_levels = 'A')
print(combined_plots)

# Save outputs
ggsave("photosynthesis_curves.png", combined_plots, width = 12, height = 5, dpi = 300)

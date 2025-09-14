library(readxl)
library(car)
miracle<-read_excel("E:/R/Copy of Salary.xlsx")
head(miracle)
summary(miracle)
plot(miracle$X,miracle$Y,main="X vs Y",pch=19,col="yellow",xlab="X",ylab="Y")
cor.test(miracle$Y,miracle$X,method = "pearson")
cor.test(miracle$X, miracle$Y, method = "spearman")
Immah_model<-lm(Y~X,data=miracle)
summary(Immah_model)
new_data <- data.frame(X = c(2, 5, 10, 20))
predict(Immah_model,newdata = new_data)
abline(Immah_model, col = "blue", lwd = 2)

library(psych)
Tinna_data<-read.table("E:/R/YP(1).txt",header = TRUE, sep = "\t")
head(Tinna_data)
describe(Tinna_data)
cor(Tinna_data)
plot(Tinna_data$VOLUME,Tinna_data$DBH+Tinna_data$THT+
       Tinna_data$AGE+Tinna_data$CL+Tinna_data$CR,
     main="Volume Vs Impact Factors",
     xlab="Impact factor",ylab="Volume",pch=22,col="gold")
Tinat_Model<-lm(VOLUME~DBH+THT+AGE+CL+CR,data=Tinna_data)
par(mfrow=c(2,2))
plot(Tinat_Model)
summary(Tinat_Model)
plot(fitted(Tinat_Model), residuals(Tinat_Model),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residual Plot", col = "blue", pch = 16)
abline(h = 0, lty = 2, col = "red")

student_res <- rstudent(Tinat_Model)
plot(fitted(Tinat_Model), student_res,
     xlab = "Fitted values", ylab = "Studentized Residuals",
     main = "Studentized Residual Plot", col = "darkblue", pch = 16)
abline(h = c(-2, 0, 2), lty = 2, col = "red")
hist(residuals(Tinat_Model), col = "skyblue", main = "Histogram of Residuals")
qqnorm(residuals(Tinat_Model)); qqline(residuals(Tinat_Model), col = "darkgreen")
shapiro.test(residuals(Tinat_Model))

plot(fitted(Tinat_Model), residuals(Tinat_Model),
     main = "Homoscedasticity Check",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
plot( student_res,main = "Studentized Residual Plot", col = "darkblue")
abline(h = c(-2, 0, 2), lty = 2, col = "red")
abline(h = c(-2, 2), col = "red", lty = 2)
install.packages("car")
library(car)

# 2. Load it every time you use it
library(lmtest)

# 3. Now run the Durbin-Watson test
dwtest(Tinat_Model)
# 1. Install the 'car' package if not already

# 2. Load it
library(car)

# 3. Run VIF diagnostics
vif(Tinat_Model)

#VIF Value | Interpretation
#1 | No correlation
#1–5 | Moderate correlation (okay)
#> 5 | Problematic multicollinearity ❌
#> 10 | Severe multicollinearity 🚨
Best_model<-step(Tinat_Model,direction = "both")
summary(Best_model)

# Full model with all predictors
full_model <- lm(VOLUME ~ DBH + THT + AGE + CL + CR, data = R_data)

# Best model using stepwise selection
install.packages("MASS")       # If not installed
library(MASS)

Singo_model <- stepAIC(Tinat_Model, direction = "both", trace = FALSE)

summary(Singo_model)   # Stats for the full model
summary(Tinat_Model)   # Stats for the best model

anova(Best_model, Tinat_Model)

nums <- 1:10
for (n in nums) {
  if (n %% 2 == 0) {
    print(paste(n, "is EVEN"))
  } else {
    print(paste(n, "is ODD"))
  }
}


# Sample data
tree_data <- data.frame(
  DBH = c(10, 15, 20, 25, 30, 35, 40, 45),
  Height = c(8, 12, 16, 20, 22, 24, 25, 26),
  Volume = c(0.3, 1.1, 2.4, 4.5, 6.7, 9.0, 11.5, 14.0)
)
tree_data
#Let’s say we expect Volume to follow a power function of DBH:
#Volume=a⋅DBH^b
# Fit a non-linear model
model_nl <- nls(Volume ~ a * DBH^b,
                data = tree_data,
                start = list(a = 0.01, b = 2))

# Summary of the model
summary(model_nl)
#for remote sensing select multiple linear regression model and machine learning model
#clasical or deep learning model
#when you have data plot scatter plot to understand the nature of your model
#The least squares principle is being used to find the estimates of the parameters.
#minimum squre error best parameter
#Obviously if the starting value is relatively close to the least squares 
#estimates, convergence difficulties will be minimal. starting value is very important
#without starting value you can not get the model
#how to get initial starting value?
#Similar procedures can be used for obtaining the starting 
#values in the case of other nonlinear models.
#For model fitting use SAS only for model fitting to get parameters
#At times the complexity of the nonlinear structure can cause difficulty. 
#There are computer software algorithms that use grid search routines, 
#which compute starting values from crude ranges suggested by the user. 
#from remote sensing data we must build model for large studies in the future
install.packages("lattice")
library(lattice)
library(readxl)
Immah_non<-read_xls("E:/R/HS.xls")
LinearM<-lm(D~H,data=Immah_non)
summary(LinearM)
plot(LinearM)
head(Immah_non)
xyplot(D~H|PLOT,data=Immah_non)
boxplot(D~PLOT,data=Immah_non)
plot(D~H,data=Immah_non)
hist(Immah_non$H)

#Power Function
nls1=nls(D~a*H**b,start=list(a=3,b=0.15),data=Immah_non)
summary(nls1)
plot(residuals(nls1)~fitted(nls1))
plot(Immah_non$H,Immah_non$D,xlab="HEIGHT",ylab="DIAMETER",main="Fitted model") 
plot(Immah_non$A,Immah_non$D,xlab="HEIGHT",ylab="DIAMETER",main="Fitted model1")
H<-seq(0,40,length=100)
lines(A,col="red",predict(nls1,list(H=H)))

#Dummy_variables
#If you use a factor variable in your model, R automatically
#creates dummy variables behind the scenes.

Immah_non$PLOT <- as.factor(Immah_non$PLOT)
model <- lm(D ~ H + PLOT, data = Immah_non)
summary(model)

#dummy_variable in my data is segment, plots

# Predicted values
Immah_non$predicted <- predict(nls1)

# Plot
plot(tree_data$DBH, tree_data$Volume,
     main = "Non-linear Fit: Volume vs DBH",
     xlab = "DBH", ylab = "Volume",
     pch = 19, col = "forestgreen")

lines(tree_data$DBH, tree_data$predicted,
      col = "blue", lwd = 2)

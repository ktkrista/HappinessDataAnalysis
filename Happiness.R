
install.packages("nloptr") 
install.packages("faraway")
library(faraway)
library(car)
library(MASS)
library(leaps)

# Read in the data
data(happy)

# Fit the full model
y <- happy[,1] # hapiness
x1 <- happy[,2] # money
x2 <- happy[,3] # sex
x3 <- happy[,4] # love
x4 <- happy[,5] # work

# Fit the model
fit <- lm(y~ x1 + x2 + x3 + x4, data=happy)
summary(fit)

# (1) QQ plot and a studentized residual plot
par(mfrow=c(1,2))
hist(studres(fit), breaks=10, freq=F, col="cornflowerblue", 
     cex.axis=1.5, cex.lab=1.5, cex.main=2)
qqPlot(fit)

# (1) Studententized residual plot 
par(mfrow=c(1,1))
residualPlot(fit, type="rstudent", quadratic=F, col = "dodgerblue",pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5)

# Standardized residual values
stdres(fit)
# Studentized residual values
studres(fit)
# Rstudent residual values
RStudent <- rstudent(fit)
RStudent

# Standardized residual plot
barplot(height = stdres(fit), names.arg = 1:39, 
        main = "Standardized Residuals", xlab = "Index", 
        ylab = "Standardized Resid", ylim=c(-3,3))
abline(h=2, col = "Red", lwd=2)
abline(h=-2, col = "Red", lwd=2)
# Studentized  residual plot
range(studres(fit))
barplot(height = studres(fit), names.arg = 1:39, 
        main = "Studentized Residuals", xlab = "Index", 
        ylab = "Studentized Resid", ylim=c(-3,3))
abline(h=2, col = "Red", lwd=2)
abline(h=-2, col = "Red", lwd=2)
# Rstudent residual plot
range(RStudent)
barplot(height = RStudent, names.arg = 1:39, 
        main = "R Student Residuals", xlab = "Index", 
        ylab = "R Student Resid", ylim=c(-5,5))
abline(h=qt(0.05/(2*39), 21, lower.tail=F) , col = "Red", lwd=2)
abline(h=-qt(0.05/(2*39), 21, lower.tail=F) , col = "Red", lwd=2)

# Measures of influence 
myInf <- influence.measures(fit) 
myInf
summary (myInf)
# Diagnostic Plots
influenceIndexPlot(fit, vars=c("Cook", "Studentized", "hat"))
# DFBETA Plots
dfbetasPlots(fit, intercept = T)

# Forward Selection with regsubset command #################################################
fwd <- regsubsets(y ~ x1 + x2 + x3 + x4 , data=happy, method="forward")
summary(fwd)
fwd.mse <- summary(fwd)$rss/(n-(2:5))
fwd.adjr2 <- summary(fwd)$adjr2
fwd.cp <- summary(fwd)$cp
fwd.bic <- summary(fwd)$bic
fwd.criteria <- cbind(fwd.mse, fwd.adjr2, fwd.cp, fwd.bic)
colnames(fwd.criteria) <- c("MSE", "Adj R2", "Cp", "BIC")
rownames(fwd.criteria) <- 2:5
fwd.criteria

par(mfrow=c(1,2))
plot(fwd, scale="r2", main = "Forward: R2")
plot(fwd, scale="adjr2", main = "Forward: adjusted R2")
plot(2:5, fwd.mse, col = "blue", type = "l", xlab = "p", ylab = "MSE")
plot(2:5, fwd.adjr2, col = "blue", type = "l", xlab = "p", ylab = "Adj R2")

par(mfrow=c(1,2))
plot(fwd, scale="Cp", main = "Forward: Cp")
plot(fwd, main="Forward: BIC")
par(mfrow=c(1,2))
plot(2:5, fwd.cp, col = "blue", xlab = "p", ylab = "Cp", pch=16, cex=1, xlim=c(0,6), ylim=c(0,15))
abline(a=0,b=1, col = "red")
plot(2:5, fwd.bic, col = "blue", type = "l", xlab = "p", ylab = "BIC")

# Fit the new regression model 
final.model <- lm(y ~ x1 + x3 + x4, data=happy)
summary(final.model)

# Plot y against all the chosen predictor variables x1, x3 and x4
# scatter plot and a regression line
happyFrame <- data.frame(x=happy[,2:5], y = happy[,1])
library(reshape2)
library(ggplot2)
happyPlot <- melt(happyFrame[c(1,3:5)], id.vars='y')
ggplot(happyPlot) + geom_jitter(aes(value, y, colour=variable)) + 
  geom_smooth(aes(value, y, colour=variable), method=lm, se=FALSE) +
  facet_wrap(~variable, scales="free_x") 

# QQ plot and a studentized residual plot of the new model
par(mfrow=c(1,2))
hist(studres(final.model), breaks=10, freq=F, col="cornflowerblue", 
     cex.axis=1.5, cex.lab=1.5, cex.main=2)
qqPlot(final.model)

# Studententized residual plot of the new model
par(mfrow=c(1,1))
residualPlot(final.model, type="rstudent", quadratic=F, col = "dodgerblue",pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5)

# Measures of influence of the new model
myNEWInf <- influence.measures(final.model) 
summary (myNEWInf)
# Diagnostic Plots of the new model
influenceIndexPlot(final.model, vars=c("Cook", "Studentized", "hat"))
# DFBETA Plots of the new model
dfbetasPlots(final.model, intercept = T)




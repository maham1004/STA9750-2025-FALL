# 1. Load your CSV
CH01PR21 <- read.csv("~/Downloads/CH01PR21.csv")

# 2. Quick look at data
head(CH01PR21)
summary(CH01PR21)

# 3. Scatterplot
plot(Y ~ X, data=CH01PR21, xlab="X", ylab="Y", main="Scatterplot of Y vs X")

# 4. Fit linear model
m <- lm(Y ~ X, data=CH01PR21)
summary(m)

# 5. t critical value for 95% CI (df = n-2 = 8)
qt(0.975, df = 8)

# 6. 95% confidence intervals for coefficients
confint(m, level = 0.95)

# 7. p-value for slope t-test
t_value <- summary(m)$coefficients["X", "t value"]
pt(abs(t_value), df = 8, lower.tail = FALSE) * 2

# 8. CI for mean Y at X = 55
predict(m, data.frame(X = 55), level = 0.90, interval = "confidence")

# 9. Prediction interval for new observation at X = 55
predict(m, data.frame(X = 55), level = 0.90, interval = "prediction")

# 10. Superimpose regression line, CI, and PI
plot(Y ~ X, data=CH01PR21, xlab="X", ylab="Y")
abline(m, col = "red")

newx <- seq(min(CH01PR21$X), max(CH01PR21$X), by = 5)

ci90 <- predict(m, newdata = data.frame(X = newx), level = 0.90, interval = "confidence")
lines(newx, ci90[,2], col="blue")
lines(newx, ci90[,3], col="blue")

pi90 <- predict(m, newdata = data.frame(X = newx), level = 0.90, interval = "prediction")
lines(newx, pi90[,2], lty = 2)
lines(newx, pi90[,3], lty = 2)

# 11. ANOVA
anova(m)

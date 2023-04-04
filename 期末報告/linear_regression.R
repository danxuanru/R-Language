#import
#getwd()
#setwd("~/CCU/R語言應用與分析方法/final_proj")
#value = read.csv("insurance.csv", header = TRUE, sep = ",")
#value

#Linear regression
library(ggplot2)
valueLM <- lm(charges ~ age + bmi + children, data = value)
lm(charges ~ age + bmi + children, data = value)
ggplot(value, aes(x = age + bmi + children, y = charges)) + geom_point(shape = 11, size = 3)
ggplot(value, aes(x = age + bmi + children, y = charges)) + geom_point(shape = 11, size = 3) + geom_smooth(method = lm) + labs(x = "age + bmi + children", y = "charges")
summary(valueLM)

#Prediction
new <- data.frame(age = 30, bmi = 22, children = 2)
result <- predict(valueLM, newdata = new)
result
ggplot(value, aes(x = age + bmi + children, y = charges)) + geom_point(shape = 11, size = 3) + geom_point(x = new$age + new$bmi + new$children, y = result, size = 10, shape = 18, color = "red") +  geom_smooth(method = lm) + labs(x = "age + bmi + children", y = "charges")

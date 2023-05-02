library(readxl)
kb <- read_excel("C:\\Users\\turtl\\Documents\\Classes\\Spring 2023\\STAT 4204\\project\\stat4204_project\\keyboard.xlsx")

kb$Operator = as.factor(kb$Operator)
kb$Order = as.factor(kb$Order)
kb$`Keyboard Type` = as.factor(kb$`Keyboard Type`)

par(mfrow=c(1,3))
boxplot(`Typing Speed` ~ Operator, data = kb)
boxplot(`Typing Speed` ~ Order, data = kb)
boxplot(`Typing Speed` ~ `Keyboard Type`, data = kb)

# Not necessary, this gets done on line 18 anyways
#myfit = aov(`Typing Speed` ~ Operator + Order + `Keyboard Type`, data = kb)
#summary(myfit)

myfit = lm(`Typing Speed` ~ Operator + Order + `Keyboard Type`, data = kb)
anova(myfit)
par(mfrow=c(1,1))
plot(myfit)

# Testing the three assumptions
# 1. Equal Variance - Fitted vs Residuals plot and the Levene test
plot(myfit, 1)
library(car)
leveneTest(myfit$residuals ~ factor(kb$`Keyboard Type`))

# 2. Normality - Q-Q Plot and Normality test
# This isn't satisfied with a single replication
plot(myfit, 2)
shapiro.test(myfit$residuals)

# 3. Independent - Residuals vs orders plot, and the Durbin-Watson test    
plot(myfit$residuals, type="o", xlab="Residual orders", ylab="Residual value")
abline(0,0, lty=2)
durbinWatsonTest(myfit) 

# Residuals vs operator, order and method
plot(myfit$residuals,factor(kb$`Keyboard Type`), type="l", xlab="Residual orders", ylab="Residual value")
abline(0,0, lty=2)

res=myfit$residuals; Type=kb$`Keyboard Type`; Operator=kb$Operator; Order=kb$Order
new.data = data.frame(res, Type, Operator, Order)

library("ggpubr")
ggline(new.data, x = "Type", y = "res", 
       add = c("mean_se", "jitter"), 
       order = c("Mechanical","Membrane","Rubber Dome"),
       ylab = "Residuals", xlab = "Keyboard Type")

ggline(new.data, x = "Operator", y = "res", 
       add = c("mean_se", "jitter"), 
       order = c("1", "2", "3"),
       ylab = "Residuals", xlab = "Operator")

ggline(new.data, x = "Order", y = "res", 
       add = c("mean_se", "jitter"), 
       order = c("1", "2", "3"),
       ylab = "Residuals", xlab = "Order")


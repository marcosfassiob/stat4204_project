library(readxl)
kb <- read_excel("C:\\Users\\turtl\\Documents\\Classes\\Spring 2023\\STAT 4204\\project\\stat4204_project\\keyboard.xlsx")

# Setting factors
kb$Operator = as.factor(kb$Operator)
kb$Order = as.factor(kb$Order)
kb$`Keyboard Type` = as.factor(kb$`Keyboard Type`)

# Boxplots of our parameters with the response variable Typing Speed
par(mfrow=c(1,3))
boxplot(`Typing Speed` ~ Operator, data = kb)
boxplot(`Typing Speed` ~ Order, data = kb)
boxplot(`Typing Speed` ~ `Keyboard Type`, data = kb)

# Not necessary, this gets done on line 20 anyways
#myfit = aov(`Typing Speed` ~ Operator + Order + `Keyboard Type`, data = kb)
#summary(myfit)

myfit = lm(`Typing Speed` ~ Operator + Order + `Keyboard Type`, data = kb)
anova(myfit)        # This shows that Operator is the only significant factor in
                    # Influencing typing speed. 
par(mfrow=c(1,1))
#plot(myfit)

# Testing the three assumptions
# 1. Equal Variance - Fitted vs Residuals plot and the Levene test
plot(myfit, 1)
library(car)
leveneTest(myfit$residuals ~ factor(kb$`Keyboard Type`))
# Equal variance is satisfied

# 2. Normality - Q-Q Plot and Normality test
# This isn't satisfied with a single replication
plot(myfit, 2)
shapiro.test(myfit$residuals)
# Normality is satisfied

# 3. Independent - Residuals vs orders plot, and the Durbin-Watson test    
plot(myfit$residuals, type="o", xlab="Residual orders", ylab="Residual value")
abline(0,0, lty=2)
durbinWatsonTest(myfit) 
# Independence is satisfied

# Residuals vs operator, order and method
plot(myfit$residuals,factor(kb$`Keyboard Type`), type="l", xlab="Residual orders", ylab="Residual value")
abline(0,0, lty=2)
# Probably shouldn't include this bc I don't really know what it's doing or means

res=myfit$residuals; Type=kb$`Keyboard Type`; Operator=kb$Operator; Order=kb$Order
new.data = data.frame(res, Type, Operator, Order)

# Plots for the various factors in the experiment against our residuals.
# Probably not necessary to include in the report but good to have
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


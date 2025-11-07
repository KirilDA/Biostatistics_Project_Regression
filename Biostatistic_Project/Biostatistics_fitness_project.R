### Biostatistics project fitness
list.files()
install.packages("sas7bdat")
library("sas7bdat")
data=read.sas7bdat("C:\\Users\\kiril\\Downloads\\fitness.sas7bdat")
summary(data)
data
par(mfrow = c(1, 2))  # 1 ред, 2 колони

hist(data$Oxygen)
plot(density(data$Oxygen))
hist(data$Age)
plot(density(data$Age))
hist(data$Weight)
plot(density(data$Weight))
hist(data$RunTime)
plot(density(data$RunTime))
hist(data$RestPulse)
plot(density(data$RestPulse))
hist(data$RunPulse)
plot(density(data$RunPulse))
hist(data$MaxPulse)
plot(density(data$MaxPulse))
install.packages("GGally")
library(GGally)
ggpairs(data)
plot(data$RunTime, data$Oxygen)
abline(lm(Oxygen ~ RunTime, data = data))
plot(data$Oxygen,data$Weight)
first_model=lm(Oxygen~RunTime,data=data)
summary(first_model)
plot(residuals(first_model))
abline(h=0,col="red",lty=2)

sapply(data, sd)
summary(data)
shapiro.test(data$MaxPulse)
cor(data)
m=lm(Oxygen~.,data=data)
summary(m)
plot(residuals(m))
abline(h=0,col="red",lty=2)
qqnorm(residuals(m))

library(car)
vif(m)
stepwise_reg=step(m, direction = "backward")
summary(stepwise_reg)
m_reduced=lm(Oxygen~Age+RunTime+RunPulse+MaxPulse,data=data)
summary(m_reduced)
qqnorm(residuals(m_reduced))


m_final=lm(Oxygen~Age+RunTime+RunPulse,data=data)
summary(m_final)
plot(m_final)
plot(rstandard(m_final))
qqnorm(residuals(m_final))
abline(h=0, col="red",lty=2)
?which
max(abs(residuals(m_final)))
sd(residuals(m_final))
which(abs(rstandard(m_final)) > 2*sd(rstandard(m_final)))
###Имаме 3 аутлаяри 8,9,21 нека проверим
shapiro.test(residuals(m_final)) 
###Остатъците са нормално разпределени 
hist(residuals(m_final))
anova(stepwise_reg,m_final)### Резултатът е 0.06 като п-стойост което е на границата, но 
##не можем да кажем, че по-комплекния модел е по-добър 
length(data$Oxygen)
data_cleaned=data[-c(8,9,21),]
m_reduced_cleaned=lm(Oxygen~Age+RunTime+RunPulse+MaxPulse,data=data_cleaned)
summary(m_reduced_cleaned)
qqnorm(residuals(m_reduced_cleaned))
m_final_cleaned=lm(Oxygen~Age+RunTime+RunPulse,data=data_cleaned)
summary(m_final_cleaned)
plot(m_final_cleaned)
plot(rstandard(m_final_cleaned))
qqnorm(residuals(m_final_cleaned))
abline(h=0, col="red",lty=2)
length(data_cleaned$Weight)

vif(m_final)
if (!require(boot)) install.packages("boot")
data
library(boot)

loocv_error <- function(data, formula) {
  glm_fit <- glm(formula, data = data)
  cv_result <- cv.glm(data, glm_fit, K = nrow(data))
  return(cv_result$delta[1])  # чиста крос валидация за пресмятане на грешката
}

formula_full <- Oxygen ~ Age + RunTime + RunPulse + MaxPulse
formula_reduced <- Oxygen ~ Age + RunTime+ RunPulse

error_full <- loocv_error(data, formula_full)
error_reduced <- loocv_error(data, formula_reduced)


## тука ползвам cat вместо принт просто, за да излезе и надписа concatenate and print
cat("LOOCV предикшън грешка на пълния модел:", error_full)
cat("LOOCV предикшън грепка на редуцирания модел:", error_reduced)

plot(m_reduced_cleaned)
plot(residuals(m_reduced_cleaned))
abline(h=0, col="green",lty=2 )


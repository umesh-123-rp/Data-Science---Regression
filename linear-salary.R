# Q4 : Build s prediction model for salary hike
salary_Data<-read.csv("D:\\Dataset-Linear Regression\\Salary_Data.csv")
View(salary_Data)
summary(salary_Data)
Exp<-salary_Data$YearsExperience
Salary<-salary_Data$Salary
var(Exp)
sd(Exp)
var(Salary)
sd(Salary)
qqplot(Salary,Exp)
install.packages("moments")
library(moments)
skewness(Exp)
skewness(Salary)
kurtosis(Exp)
kurtosis(Salary)
plot(Exp,Salary)
cor(Exp,Salary)
install.packages("lattice")
library(lattice)
dotplot(Exp)
dotplot(Salary)
boxplot(Exp,col="blue")
boxplot(Salary,col="yellow")
library(car)
qqPlot(salary_Data)
hist(Exp)
hist(Salary)
model<-lm(Salary~Exp, data = salary_Data)
summary(model)
plot(model)
# R-Square value is 96% which is an excellent model
# Checking of model at experience of 12 years
confint(model,level=0.95)
predict(model,interval="predict")
model1<-lm(Salary~log(Exp), data = salary_Data)
summary(model1)
model2<-lm((log(Salary)~Exp), data = salary_Data)
summary(model2)
salary_Data1<-salary_Data[-c(9,20,24),]
Exp1<-salary_Data1$YearsExperience
Salary1<-salary_Data1$Salary
model3<-lm(Salary1~Exp1, data=salary_Data1)
summary(model3)
confint(model3,level=0.95)
predict(model3,interval="predict")
plot(model3)


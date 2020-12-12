#Q2 : Predict delivery time using sorting time
delivery_time<-read.csv("C:\\Users\\IN102385\\OneDrive - Super-Max Personal Care Pvt. Ltd\\Assignment - Data Science -UL\\Linear Regression - R\\delivery_time.csv")
View(delivery_time)
str(delivery_time)
summary(delivery_time)
DT<-delivery_time$Delivery.Time
ST<-delivery_time$Sorting.Time
var(DT)
sd(DT)
var(ST)
sd(ST)
install.packages("moments")
library(moments)
skewness(DT)
skewness(ST)
kurtosis(DT)
kurtosis(ST)
hist(DT)
hist(ST)
install.packages("lattice")
library(lattice)
dotplot(DT)
dotplot(ST)
boxplot(ST, col="Blue")
boxplot(DT, col="red")
plot(ST,DT)
cor(ST,DT)
model<-lm(DT ~ST,data = delivery_time)
summary(model)
n=length(DT)
k=length(model$coefficients)-1 #Subtract one to ignore intercept
SSE=sum(model$residuals**2)
MSE=SSE/(n-(k+1))
SSST=sum((ST-mean(ST))**2)
SSST
SE_ST=sqrt(MSE/SSST)
SE_ST
SE_IN=(1/n+((mean(ST)**2)/SSST))*MSE
1/21
SE_IN
sqrt(SE_IN)
SSDT=sum((DT-mean(DT))**2)
MSE=SSE/(n-(k+1))
sqrt(MSE)
1-(SSE/SSDT)*(n-1)/(n-(k+1))
VAR=var(model$residuals)
mean(ST)
SSST=sum(ST-mean(ST))**2
SSST

VAR = SSE/(n-(k+1))
VAR

sqrt(VAR/SSST)
plot(model)
confint(model,level=0.95)
predict(model,interval="predict")
sqrt(sum(model$residuals^2)/nrow(delivery_time))

model1<-lm(DT~log(ST),data = delivery_time)
summary(model1)
plot(model1)
confint(model1,level=0.95)
predict(model1,interval="predict")
sqrt(sum(model1$residuals^2)/nrow(delivery_time))

model2<-lm(log(DT)~ST,data=delivery_time)
summary(model2)
confint(model2,level=0.95)
predict(model2,interval="predict")
sqrt(sum(model2$residuals^2)/nrow(delivery_time))


delivery_time1<-delivery_time[-c(5,9,21),]
DT1<-delivery_time1$Delivery.Time
ST1<-delivery_time1$Sorting.Time
View(delivery_time1)
model3<-lm(DT1~ST1,data = delivery_time1)
summary(model3)
plot(model3)
confint(model3,level=0.95)
predict(model3,interval="predict")
sqrt(sum(model3$residuals^2)/nrow(delivery_time1))
# There is an improvement in model R-Squared value from 0.68 to 0.83


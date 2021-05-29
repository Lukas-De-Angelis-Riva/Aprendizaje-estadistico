library(readxl)
library(zoo)
library(lmtest)
library(MASS)

data_cemento <- read.table("cemento.txt", header=TRUE)
reg <- lm(y~x2+x3, data=data_cemento)
boxcox(data_cemento$y~data_cemento$x2+data_cemento$x3, lambda = seq(0,4,0.01))

data_cemento$y2 <- ((data_cemento$y)^2 - 1)/2
data_cemento$y2


reg <- lm(y2~x2+x3, data = data_cemento)
plot(reg$fitted.values,rstandard(reg), pch=20,col="darkblue",xlab="valor ajustado", 
     ylab="residuo estandarizado")
abline(h=0)

nuevo <- data.frame(reg$fitted.values, reg$fitted.values^2, rstandard(reg))
colnames(nuevo) <- c("y_som", "y_som2", "res")
reg <- lm(res~y_som+y_som2, data = nuevo)
lines(sort(nuevo$y_som), fitted(reg)[order(nuevo$y_som)],col='red')


mean((y - sqrt(reg$fitted.values*2 + 1))^2)
Y = sqrt((a + bX2 + cX3)*2 + 1)



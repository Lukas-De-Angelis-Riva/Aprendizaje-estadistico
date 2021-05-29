data_cemento <- read.table("cemento.txt", header=TRUE)

#modelo 1
regM1 <- lm(y~x1+x2+x3+x4+x5, data=data_cemento)

#(regM1$residuals - mean(regM1$residuals))/sqrt((1-diag(P))*(S^2))

plot(regM1$fitted.values,rstandard(regM1), pch=20,col="darkblue",xlab="valor ajustado", 
     ylab="residuo estandarizado")
abline(h=0)



nuevo <- data.frame(regM2$fitted.values, regM2$fitted.values^2, rstandard(regM2))
colnames(nuevo) <- c("y_som", "y_som2", "res")
reg <- lm(res~y_som+y_som2, data = nuevo)
lines(sort(nuevo$y_som), fitted(reg)[order(nuevo$y_som)],col='red')



# modelo 2 -> el q nos interesa
regM2 <- lm(y~x1+x2+x3+x4+x5+0, data=data_cemento)
plot(regM2$fitted.values,rstandard(regM2), pch=20,col="darkblue",xlab="valor ajustado", 
     ylab="residuo estandarizado")
abline(h=0)

qqnorm(y = rstandard(regM2))
abline(0,1)


nuevo <- data.frame(regM1$fitted.values, regM1$fitted.values^2, rstandard(regM1))
colnames(nuevo) <- c("y_som", "y_som2", "res")
reg <- lm(res~y_som+y_som2, data = nuevo)
lines(sort(nuevo$y_som), fitted(reg)[order(nuevo$y_som)],col='red')


# modelo 3 -> meh
regM3 <- lm(y~x1+x2+x3+x5, data=data_cemento)
plot(regM3$fitted.values,rstandard(regM3), pch=20,col="darkblue",xlab="valor ajustado", 
     ylab="residuo estandarizado")
abline(h=0)

nuevo <- data.frame(regM3$fitted.values, regM3$fitted.values^2, rstandard(regM3))
colnames(nuevo) <- c("y_som", "y_som2", "res")
reg <- lm(res~y_som+y_som2, data = nuevo)
lines(sort(nuevo$y_som), fitted(reg)[order(nuevo$y_som)],col='red')


# modelo 4 -> el q más interesa
regM4 <- lm(y~x2+x3, data=data_cemento)
plot(regM4$fitted.values,rstandard(regM4), pch=20,col="darkblue",xlab="valor ajustado", 
     ylab="residuo estandarizado")
abline(h=0)

nuevo <- data.frame(regM4$fitted.values, regM4$fitted.values^2, rstandard(regM4))
colnames(nuevo) <- c("y_som", "y_som2", "res")
reg <- lm(res~y_som+y_som2, data = nuevo)
lines(sort(nuevo$y_som), fitted(reg)[order(nuevo$y_som)],col='red')

qqnorm(y = rstandard(regM4))
abline(0,1)

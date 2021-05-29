library(gridExtra)
library(leaps)
library(ggplot2)

data_cemento <- read.table("cemento.txt", header=TRUE)

forw<-regsubsets(y~x1+x2+x3+x4+x5,data = data_cemento, method = "forward")

par(mfrow=c(2,1))
par(mar=c(4,6,1,2))
plot(summary(forw)$adjr2,pch=20,xlab="Modelo", ylab= "R^2 aj")
plot(1:5,summary(forw)$cp,pch=20,ylim=c(0,8),xlab="Modelo", ylab= "CP")
abline(0,1)


adjr2 <- data.frame(c(1, 2, 3, 4, 5), summary(forw)$adjr2)
names(adjr2) <- c("Modelo", "R2adj")
p1 <- ggplot(adjr2, aes(Modelo, R2adj)) + geom_point(size=2, shape=23) + labs(x = "Modelo", y = "R² ajustado")

cp <- data.frame(c(1, 2, 3, 4, 5), summary(forw)$cp)
names(cp) <- c("Modelo", "CP") 

p2 <- ggplot(cp, aes(Modelo, CP)) + 
  geom_point(size=2, shape=23) + 
  labs(x = "Modelo", y = "CP") + 
  ylim(0, 10) +
  geom_abline(intercept = 0, slope = 1)

grid.arrange(p1, p2, ncol=1)



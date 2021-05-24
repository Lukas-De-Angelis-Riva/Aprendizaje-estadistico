library(corrplot)
library(RColorBrewer)
library(GGally)
library(gridExtra)


data_cemento <- read.table("cemento.txt", header=TRUE)
#data_cemento <- data_cemento[data_cemento$x5<3,]

data_cemento <- data_cemento[c('x1', 'x2', 'x3', 'x4', 'x5','y')]
corr_matrix <- cor(data_cemento, method = "pearson")
corr_matrix
p.mat <- cor.mtest(data_cemento)

col <- colorRampPalette(c("#4477AA", "#77AADD", "#BB4444", "#EE9988", "#FFFFFF"))
corrplot(corr_matrix, method="color", col=rev(brewer.pal(11, 'RdBu')),
         type = "lower",
         addCoef.col = "black",
         tl.col="black", tl.srt=0,tl.offset = 1,
         diag=TRUE
)

plot(x = data_cemento$x3, y = data_cemento$x4)

ggpairs(data_cemento)

g3<-ggplot(mtcars, aes(x=factor(cyl), y=qsec, fill=cyl)) +
  geom_boxplot() +
  theme(legend.position="none")

g1 <- ggplot(data_cemento, aes(y=x5)) +
  geom_boxplot()
g2 <- ggplot(data_cemento, aes(y=x2)) +
  geom_boxplot()

grid.arrange(g1,g2, ncol=2)



reg <- lm(y~x1+x2+x3+x4+x5, data = data_cemento)


summary(reg)


Y <- data_cemento$y
X <- cbind(rep(1, nrow(data_cemento)), data_cemento$x1, data_cemento$x2, data_cemento$x3, data_cemento$x4, data_cemento$x5)
D <- solve(t(X)%*%X)
S <- sqrt(t(Y-X%*%reg$coefficients)%*%(Y-X%*%reg$coefficients)/(14-6))

1 - pt(1.219043, df=8) + pt(-1.219043, df=8)
b1_sombrero = reg$coefficients['x1']

Ts <- b1_sombrero / (S*sqrt(D[2,2]))
Ts


forw<-regsubsets(y~x1+x2+x3+x4+x5,data = data_cemento, method = "forward")
summary(forw)

par(mfrow=c(2,2))
plot(summary(forw)$rss,pch=20,xlab="Modelo", ylab= "RSS")
plot(summary(forw)$rsq,pch=20,xlab="Modelo", ylab= "R^2")
plot(summary(forw)$adjr2,pch=20,xlab="Modelo", ylab= "R^2 aj")
plot(1:5,summary(forw)$cp,pch=20,ylim=c(0,8),xlab="Modelo", ylab= "CP")
abline(0,1)

regFinal <- lm(y~x1+x2+x3+x5, data = data_cemento)



regM1 <- lm(y~x1+x2+x3+x4+x5, data=data_cemento)
sum((regM1$residuals)^2)

#R² = 1 - |Y-Ŷ|²/|Y-Y*|²
#1 - sum((data_cemento$y - regM1$fitted.values)^2)/sum((data_cemento$y - mean(data_cemento$y))^2)

regM2 <- lm(y~x1+x2+x3+x4+x5-1, data=data_cemento)
sum((regM2$residuals)^2)
1 - sum((data_cemento$y - regM2$fitted.values)^2)/sum((data_cemento$y - mean(data_cemento$y))^2)

regM3 <- lm(y~x1+x2+x3+x5, data=data_cemento)
sum((regM3$residuals)^2)
1 - sum((data_cemento$y - regM3$fitted.values)^2)/sum((data_cemento$y - mean(data_cemento$y))^2)

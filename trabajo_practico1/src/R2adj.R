library(leaps)
data_cemento <- read.table("cemento.txt", header=TRUE)

Y <- data_cemento$y
Y_raya <- mean(Y)

regM1 <- lm(y~x1+x2+x3+x4+x5, data=data_cemento)
Y_somM1 <- regM1$fitted.values
n <- 14
pM1 <- 6
R2adj_M1 <- 1 - (n-1)/(n-pM1) * (sum((Y - Y_somM1)^2))/(sum((Y-Y_raya)^2))

regM2 <- lm(y~x1+x2+x3+x4, data=data_cemento)
Y_somM2 <- regM2$fitted.values
n <- 14
pM2 <- 5
R2adj_M2 <- 1 - (n-1)/(n-pM2) * (sum((Y - Y_somM2)^2))/(sum((Y-Y_raya)^2))

regM3 <- lm(y~x1+x2+x3+x4+x5+0, data=data_cemento)
Y_somM3 <- regM3$fitted.values
n <- 14
pM3 <- 5
R2adj_M3 <- 1 - (n-1)/(n-pM3) * (sum((Y - Y_somM3)^2))/(sum((Y-Y_raya)^2))

regM4 <- lm(y~x1+x2+x3+x4+0, data=data_cemento)
Y_somM4 <- regM4$fitted.values
n <- 14
pM4 <- 4
R2adj_M4 <- 1 - (n-1)/(n-pM4) * (sum((Y - Y_somM4)^2))/(sum((Y-Y_raya)^2))
R2adj_M4


# R2adj = 1 - (n-1)/(n-p) (Y-Y_som)^2/(Y-Y_raya)^2


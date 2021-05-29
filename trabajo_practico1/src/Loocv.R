data_cemento <- read.table("cemento.txt", header=TRUE)


X <- cbind(rep(1, nrow(data_cemento)), data_cemento$x1, data_cemento$x2, data_cemento$x3, data_cemento$x4, data_cemento$x5)
P <- X %*% solve(t(X)%*%X) %*% t(X)
regM1 <- lm(y~x1+x2+x3+x4+x5, data=data_cemento)
r_LOOCV_M1 <- regM1$residuals/(1-diag(P))
mean(r_LOOCV_M1^2)
mean(rstandard(regM1)^2)

X <- cbind(data_cemento$x1, data_cemento$x2, data_cemento$x3, data_cemento$x4, data_cemento$x5)
P <- X %*% solve(t(X)%*%X) %*% t(X)
regM2 <- lm(y~x1+x2+x3+x4+x5+0, data=data_cemento)
r_LOOCV_M2 <- regM2$residuals/(1-diag(P))
mean(r_LOOCV_M2^2)


X <- cbind(rep(1, nrow(data_cemento)), data_cemento$x1, data_cemento$x2, data_cemento$x3, data_cemento$x5)
P <- X %*% solve(t(X)%*%X) %*% t(X)
regM3 <- lm(y~x1+x2+x3+x5, data=data_cemento)
r_LOOCV_M3 <- regM3$residuals/(1-diag(P))
mean(r_LOOCV_M3^2)

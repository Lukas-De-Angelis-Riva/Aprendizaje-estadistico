library(ggplot2)

data_cemento <- read.table("cemento.txt", header=TRUE)

square_r_LOOCV <- function(reg){
  X <- model.matrix(reg)
  P <- X %*% solve(t(X)%*%X) %*% t(X)
  r <- reg$residuals/(1-diag(P))
  return(r^2)
}

regM1 <- lm(y~x1+x2+x3+x4+x5, data=data_cemento)
srLOOCV_m1 <- square_r_LOOCV(reg = regM1)

regM2 <- lm(y~x1+x2+x3+x4+x5+0, data=data_cemento)
srLOOCV_m2 <- square_r_LOOCV(reg = regM2)

regM3 <- lm(y~x1+x2+x3+x5, data=data_cemento)
srLOOCV_m3 <- square_r_LOOCV(reg = regM3)

srLOOCV <- data.frame(srLOOCV_m1, srLOOCV_m2, srLOOCV_m3)
names(srLOOCV) <- c("1", "2", "3")

ggplot(stack(srLOOCV), aes(x=ind, y=values)) +
  geom_boxplot() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Modelo") + ylab("ECM_CV") + labs(fill='Modelo') + 
  ggtitle("Error cuadrático medio de validación cruzada para cada modelo") + 
  scale_fill_manual(values=c("#D5F7EF", "#C9EAE2", "#B4D1CA"))



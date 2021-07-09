library(MASS)
library(GGally)

iris <- read.table("iris.data", sep = ",")
colnames(iris) <- c("SL", "SW", "PL", "PW", "Y")

ggpairs(iris, aes(colour = Y, alpha = 0.4))

ajuste_lda<-lda(Y~.,data=iris)
proyeccion = data.frame(Y = iris$Y, LD1 = predict(ajuste_lda)$x[,1], LD2 = predict(ajuste_lda)$x[,2])

# Centroides
c_setosa = c(mean(proyeccion[proyeccion$Y == "Iris-setosa", "LD1"]),
             mean(proyeccion[proyeccion$Y == "Iris-setosa", "LD2"]))
c_versicolor = c(mean(proyeccion[proyeccion$Y == "Iris-versicolor", "LD1"]),
             mean(proyeccion[proyeccion$Y == "Iris-versicolor", "LD2"]))
c_virginica = c(mean(proyeccion[proyeccion$Y == "Iris-virginica", "LD1"]),
             mean(proyeccion[proyeccion$Y == "Iris-virginica", "LD2"]))

ggplot(proyeccion) +
  geom_point(aes(LD1, LD2, colour = Y), size = 2.5) +
  geom_point(aes(x = c_setosa[1], y = c_setosa[2])) +
  geom_point(aes(x = c_versicolor[1], y = c_versicolor[2])) +
  geom_point(aes(x = c_virginica[1], y = c_virginica[2]))


table(iris$Y, predict(ajuste_lda)$class, dnn = c("Clase real", "Clase predicha"))

errores <- c()
for(i in 1:nrow(iris)){
  iris_loocv = iris[-i, ]
  ajuste <- lda(Y~.,data=iris_loocv)
  resultado <- predict(ajuste, newdata=iris[i, ])$class
  errores <- c(errores, resultado)
}

table(iris$Y, errores, dnn = c("Clase real", "Clase predicha"))





ajuste_qda<-qda(Y~., data=iris)
table(iris$Y, predict(ajuste_qda)$class, dnn = c("Clase real", "Clase predicha"))

errores <- c()
for(i in 1:nrow(iris)){
  iris_loocv = iris[-i, ]
  ajuste <- qda(Y~.,data=iris_loocv)
  resultado <- predict(ajuste, newdata=iris[i, ])$class
  errores <- c(errores, resultado)
}

table(iris$Y, errores, dnn = c("Clase real", "Clase predicha"))


# Ejercicio 2



library(ggplot2)
library(gridExtra)


# a

ggplot(iris) +
  geom_point(aes(SW, PW, colour = Y), size = 2.5)

ggQQ <- function (vec, titulo) # argument: vector of numbers
{
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  d <- data.frame(resids = vec)
  
  ggplot(d, aes(sample = resids)) +
    stat_qq() +
    geom_abline(slope = slope, intercept = int) +
    ggtitle(titulo)
} # Robado de internet.

g1 <- ggQQ(iris[iris$Y == "Iris-setosa",]$SW, "Ancho de sépalos - Setosa")
g2 <- ggQQ(iris[iris$Y == "Iris-setosa",]$PW, "Ancho de pétalos - Setosa")

g3 <- ggQQ(iris[iris$Y == "Iris-versicolor",]$SW, "Ancho de sépalos - Versicolor")
g4 <- ggQQ(iris[iris$Y == "Iris-versicolor",]$PW, "Ancho de pétalos - Versicolor")

g5 <- ggQQ(iris[iris$Y == "Iris-virginica",]$SW, "Ancho de sépalos - Virginica")
g6 <- ggQQ(iris[iris$Y == "Iris-virginica",]$PW, "Ancho de pétalos - Virginica")

grid.arrange(g1,g2,g3,g4,g5,g6, ncol=2)

# b

iris2 <- iris[, c("SW", "PW", "Y")]

mu1 <- colMeans(iris2[iris2$Y=="Iris-setosa", c("SW", "PW")])
mu2 <- colMeans(iris2[iris2$Y=="Iris-versicolor", c("SW", "PW")])
mu3 <- colMeans(iris2[iris2$Y=="Iris-virginica", c("SW", "PW")])
sigma1 <- cov(iris2[iris2$Y=="Iris-setosa", c("SW", "PW")])
sigma2 <- cov(iris2[iris2$Y=="Iris-versicolor", c("SW", "PW")])
sigma3 <- cov(iris2[iris2$Y=="Iris-virginica", c("SW", "PW")])


# Asume que las probas son iguales
# AX² + BY² + CXY + DX + EY + G
regla_clasif <- function(mu_i, mu_l, sigma_i, sigma_l){
  
  dif_sigma <- solve(sigma_i) - solve(sigma_l)
  A <- -0.5*dif_sigma[1, 1]
  B <- -0.5*dif_sigma[2, 2]
  C <- -1*dif_sigma[1,2]
  
  dif_sigma_pond <- solve(sigma_i) %*% mu_i - solve(sigma_l) %*% mu_l
  D <- dif_sigma_pond[1]
  E <- dif_sigma_pond[2]

  G <- 0.5*log(det(sigma_l)/det(sigma_i))+ 
      -0.5*(t(mu_i) %*% solve(sigma_i) %*% mu_i - t(mu_l) %*% solve(sigma_l) %*% mu_l)
  return(c(A, B, C, D, E, G))
}

regla_clasif_lineal <- function(mu1, mu2, sigma){
  m <- t(mu1 - mu2) %*% solve(sigma)
  u <- (mu1+mu2)/2
  D <- m[1]
  E <- m[2]
  G <- -(m[1]*u[1]+m[2]*u[2])
  return(c(D, E, G))
}

# LDA

sigma <- (sigma1+sigma2+sigma3)/3 # Sale de Sigma_W = Sum(pi_i Sigma_i)

regla_clasif_lineal(mu1, mu2, sigma)
regla_clasif_lineal(mu1, mu3, sigma)
regla_clasif_lineal(mu2, mu3, sigma)

# QDA
regla_clasif(mu1, mu2, sigma1, sigma2)
regla_clasif(mu1, mu3, sigma1, sigma3)
regla_clasif(mu2, mu3, sigma2, sigma3)



# Ejercicio E.


# Coef: c(A, B, C, D, E, G), tq: Ax²+By²+Cxy+Dx+Ey+G
# Punto: c(x, y)
# threshold: en general log(proba_l / proba_i)
esta_en_region <- function(coefs, punto, threshold){
  x <- punto[1]
  y <- punto[2]
  A <- coefs[1]
  B <- coefs[2]
  C <- coefs[3]
  D <- coefs[4]
  E <- coefs[5]
  G <- coefs[6]
  return(A*(x**2) + B*(y**2) + C*x*y + D*x + E*y + G > threshold)
}

# LDA

# Error aparente
predicciones <- c()
mu1 <- colMeans(iris2[iris2$Y=="Iris-setosa", c("SW", "PW")])
mu2 <- colMeans(iris2[iris2$Y=="Iris-versicolor", c("SW", "PW")])
mu3 <- colMeans(iris2[iris2$Y=="Iris-virginica", c("SW", "PW")])
sigma1 <- cov(iris2[iris2$Y=="Iris-setosa", c("SW", "PW")])
sigma2 <- cov(iris2[iris2$Y=="Iris-versicolor", c("SW", "PW")])
sigma3 <- cov(iris2[iris2$Y=="Iris-virginica", c("SW", "PW")])
sigma <- (sigma1+sigma2+sigma3)/3
reg1 <- regla_clasif(mu1, mu2, sigma, sigma)
reg2 <- regla_clasif(mu1, mu3, sigma, sigma)
reg3 <- regla_clasif(mu2, mu3, sigma, sigma)
for(i in 1:nrow(iris2)){
  punto <- iris2[i, c("SW", "PW")]  
  inA <- esta_en_region(reg1, punto, 0)
  inB <- esta_en_region(reg2, punto, 0)
  inC <- esta_en_region(reg3, punto, 0)
  
  if(inA && inB){
    predicciones <- c(predicciones, "Iris-setosa")
  }else if(!inA && inC){
    predicciones <- c(predicciones, "Iris-versicolor")
  }else{
    predicciones <- c(predicciones, "Iris-virginica")
  }
}
table(iris2$Y, predicciones, dnn = c("Clase real", "Clase predicha"))
1-mean(predicciones==iris2$Y)


# Error por crossvalidation
predicciones <- c()
for(i in 1:nrow(iris2)){
  iris_loocv = iris2[-i, ]
  
  punto <- iris2[i, c("SW", "PW")]
  
  n1 <- length(iris_loocv[iris_loocv$Y=="Iris-setosa", ])
  n2 <- length(iris_loocv[iris_loocv$Y=="Iris-versicolor", ])
  n3 <- length(iris_loocv[iris_loocv$Y=="Iris-virginica", ])
  
  mu1 <- colMeans(iris_loocv[iris_loocv$Y=="Iris-setosa", c("SW", "PW")])
  mu2 <- colMeans(iris_loocv[iris_loocv$Y=="Iris-versicolor", c("SW", "PW")])
  mu3 <- colMeans(iris_loocv[iris_loocv$Y=="Iris-virginica", c("SW", "PW")])
  sigma1 <- cov(iris_loocv[iris_loocv$Y=="Iris-setosa", c("SW", "PW")])
  sigma2 <- cov(iris_loocv[iris_loocv$Y=="Iris-versicolor", c("SW", "PW")])
  sigma3 <- cov(iris_loocv[iris_loocv$Y=="Iris-virginica", c("SW", "PW")])

  sigma = (n1/149)*sigma1 + (n2/149)*sigma2 + (n3/149)*sigma3
    
  reg1 <- regla_clasif(mu1, mu2, sigma, sigma)
  reg2 <- regla_clasif(mu1, mu3, sigma, sigma)
  reg3 <- regla_clasif(mu2, mu3, sigma, sigma)
  
  inA <- esta_en_region(reg1, punto, log(n2/n1))
  inB <- esta_en_region(reg2, punto, log(n3/n1))
  inC <- esta_en_region(reg3, punto, log(n3/n2))
  
  if(inA && inB){
    predicciones <- c(predicciones, "Iris-setosa")
  }else if(!inA && inC){
    predicciones <- c(predicciones, "Iris-versicolor")
  }else{
    predicciones <- c(predicciones, "Iris-virginica")
  }
}
1-mean(predicciones==iris2$Y)

# QDA

# Error aparente
predicciones <- c()
mu1 <- colMeans(iris2[iris2$Y=="Iris-setosa", c("SW", "PW")])
mu2 <- colMeans(iris2[iris2$Y=="Iris-versicolor", c("SW", "PW")])
mu3 <- colMeans(iris2[iris2$Y=="Iris-virginica", c("SW", "PW")])
sigma1 <- cov(iris2[iris2$Y=="Iris-setosa", c("SW", "PW")])
sigma2 <- cov(iris2[iris2$Y=="Iris-versicolor", c("SW", "PW")])
sigma3 <- cov(iris2[iris2$Y=="Iris-virginica", c("SW", "PW")])
reg1 <- regla_clasif(mu1, mu2, sigma1, sigma2)
reg2 <- regla_clasif(mu1, mu3, sigma1, sigma3)
reg3 <- regla_clasif(mu2, mu3, sigma2, sigma3)
for(i in 1:nrow(iris2)){
  punto <- iris2[i, c("SW", "PW")]  
  inA <- esta_en_region(reg1, punto, 0)
  inB <- esta_en_region(reg2, punto, 0)
  inC <- esta_en_region(reg3, punto, 0)
  
  if(inA && inB){
    predicciones <- c(predicciones, "Iris-setosa")
  }else if(!inA && inC){
    predicciones <- c(predicciones, "Iris-versicolor")
  }else{
    predicciones <- c(predicciones, "Iris-virginica")
  }
}
table(iris2$Y, predicciones, dnn = c("Clase real", "Clase predicha"))
1-mean(predicciones==iris2$Y)


# Error de cross validation
predicciones <- c()
for(i in 1:nrow(iris2)){
  iris_loocv = iris2[-i, ]

  punto <- iris2[i, c("SW", "PW")]

  n1 <- length(iris_loocv[iris_loocv$Y=="Iris-setosa", ])
  n2 <- length(iris_loocv[iris_loocv$Y=="Iris-versicolor", ])
  n3 <- length(iris_loocv[iris_loocv$Y=="Iris-virginica", ])
  
  mu1 <- colMeans(iris_loocv[iris_loocv$Y=="Iris-setosa", c("SW", "PW")])
  mu2 <- colMeans(iris_loocv[iris_loocv$Y=="Iris-versicolor", c("SW", "PW")])
  mu3 <- colMeans(iris_loocv[iris_loocv$Y=="Iris-virginica", c("SW", "PW")])
  sigma1 <- cov(iris_loocv[iris_loocv$Y=="Iris-setosa", c("SW", "PW")])
  sigma2 <- cov(iris_loocv[iris_loocv$Y=="Iris-versicolor", c("SW", "PW")])
  sigma3 <- cov(iris_loocv[iris_loocv$Y=="Iris-virginica", c("SW", "PW")])
  
  reg1 <- regla_clasif(mu1, mu2, sigma1, sigma2)
  reg2 <- regla_clasif(mu1, mu3, sigma1, sigma3)
  reg3 <- regla_clasif(mu2, mu3, sigma2, sigma3)
  
  inA <- esta_en_region(reg1, punto, log(n2/n1))
  inB <- esta_en_region(reg2, punto, log(n3/n1))
  inC <- esta_en_region(reg3, punto, log(n3/n2))
  
  if(inA && inB){
    predicciones <- c(predicciones, "Iris-setosa")
  }else if(!inA && inC){
    predicciones <- c(predicciones, "Iris-versicolor")
  }else{
    predicciones <- c(predicciones, "Iris-virginica")
  }
}

1-mean(predicciones==iris2$Y)




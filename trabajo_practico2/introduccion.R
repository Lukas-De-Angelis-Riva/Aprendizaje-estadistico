library(MASS)
library(GGally)

iris <- read.table("iris.data", sep = ",")
colnames(iris) <- c("SL", "SW", "PL", "PW", "Y")

ggpairs(iris, aes(colour = Y, alpha = 0.4))

ajuste_lda<-lda(Y~.,data=iris)
proyeccion = data.frame(Y = iris$Y, LD1 = predict(ajuste_lda)$x[,1], LD2 = predict(ajuste_lda)$x[,2])

# Devuelve un vector (m, b), tal que la recta es y = mx+b
mediatriz <- function(p1, p2){
  x1 = p1[1]
  x2 = p2[1]
  y1 = p1[2]
  y2 = p2[2]
  
  m = - (x2-x1)/(y2-y1)
  x3 = 1/2 * (x1+x2)
  y3 = 1/2 * (y1+y2)

  return(c(m, m*(-x3)+y3))
}

ggplot(proyeccion) +
  geom_point(aes(LD1, LD2, colour = Y), size = 2.5)

# centroides
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
  geom_point(aes(x = c_virginica[1], y = c_virginica[2]))+
  geom_abline(intercept = mediatriz(c_setosa, c_versicolor)[2], slope = mediatriz(c_setosa, c_versicolor)[1]) + 
  geom_abline(intercept = mediatriz(c_setosa, c_virginica)[2], slope = mediatriz(c_setosa, c_versicolor)[1])


a <- predict(ajuste_lda, newdata = iris)
a$class

errores <- c()
for(i in 1:nrow(iris)){
  iris_loocv = iris[-i, ]
  ajuste <- lda(Y~.,data=iris_loocv)
  resultado <- predict(ajuste, newdata=iris[i, ])$class == iris[i, "Y"]
  errores <- c(errores, resultado)
}

1-mean(errores)
table(iris$Y, predict(ajuste_lda)$class, dnn = c("Clase real", "Clase predicha"))

ajuste_qda<-qda(Y~., data=iris)
table(iris$Y, predict(ajuste_qda)$class, dnn = c("Clase real", "Clase predicha"))

errores <- c()
for(i in 1:nrow(iris)){
  iris_loocv = iris[-i, ]
  ajuste <- qda(Y~.,data=iris_loocv)
  resultado <- predict(ajuste, newdata=iris[i, ])$class == iris[i, "Y"]
  errores <- c(errores, resultado)
}

1-mean(errores)


# Ejercicio 2 



library(ggplot2)
library(gridExtra)


# a

ggplot(iris) +
  geom_point(aes(SW, PW, colour = Y), size = 2.5)

g1 <- ggQQ(iris[iris$Y == "Iris-setosa",]$SW, "Ancho de sépalos - Setosa")
g2 <- ggQQ(iris[iris$Y == "Iris-setosa",]$PW, "Ancho de pétalos - Setosa")

g3 <- ggQQ(iris[iris$Y == "Iris-versicolor",]$SW, "Ancho de sépalos - Versicolor")
g4 <- ggQQ(iris[iris$Y == "Iris-versicolor",]$PW, "Ancho de pétalos - Versicolor")

g5 <- ggQQ(iris[iris$Y == "Iris-virginica",]$SW, "Ancho de sépalos - Virginica")
g6 <- ggQQ(iris[iris$Y == "Iris-virginica",]$PW, "Ancho de pétalos - Virginica")

grid.arrange(g1,g2,g3,g4,g5,g6, ncol=2)

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
}

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

regla_clasif(mu1, mu2, sigma1, sigma2)
regla_clasif(mu1, mu3, sigma1, sigma3)
regla_clasif(mu2, mu3, sigma2, sigma3)


sigma <- (sigma1+sigma2+sigma3)/3 # Sale de Sigma_W = Sum(pi_i Sigma_i)

regla_clasif_lineal(mu1, mu2, sigma)
regla_clasif_lineal(mu1, mu3, sigma)
regla_clasif_lineal(mu2, mu3, sigma)


ajuste_lda<-lda(Y~.,data=iris2)
table(iris$Y, predict(ajuste_lda)$class, dnn = c("Clase real", "Clase predicha"))
# Coinciden. :D.

# Ejercicio E.


errores <- c()
for(i in 1:nrow(iris2)){
  iris_loocv = iris2[-i, ]
  ajuste <- lda(Y~.,data=iris_loocv)
  resultado <- predict(ajuste, newdata=iris2[i, ])$class == iris2[i, "Y"]
  errores <- c(errores, resultado)
}

1-mean(errores)
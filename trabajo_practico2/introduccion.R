library(MASS)
library(GGally)

iris <- read.table("iris.data", sep = ",")
colnames(iris) <- c("SL", "SW", "PL", "PW", "Y")

ggpairs(iris, aes(colour = Y, alpha = 0.4))

ajuste_lda<-lda(Y~.,data=iris)
proyeccion = data.frame(Y = iris$Y, LD1 = predict(ajuste_lda)$x[,1], LD2 = predict(ajuste_lda)$x[,2])

aplicarF <- function(a){
  if(a == "Iris-setosa"){
    return(1)
  }else if(a== "Iris-versicolor"){
    return(2)
  }else{
    return(3)
  }
}

proyeccion$Y <- sapply(proyeccion$Y, aplicarF)

ggplot(proyeccion) +
  geom_point(aes(LD1, LD2, colour = Y), size = 2.5) +
  geom_contour(data=proyeccion, aes(x=LD1, y=LD2, z=Y), colour="red2", alpha=0.5, breaks=c(1.5,2.5))

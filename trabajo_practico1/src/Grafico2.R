library(ggplot2)

data_cemento <- read.table("cemento.txt", header=TRUE)
data_cemento <- data_cemento[2:6]


ggplot(data_cemento, aes(x=x3, y=x4)) + geom_point()

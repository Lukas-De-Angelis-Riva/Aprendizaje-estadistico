library(ggplot2)
library(gridExtra)

data_cemento <- read.table("cemento.txt", header=TRUE)
data_cemento <- data_cemento[2:6]

g1 <- ggplot(data_cemento, aes(y=x2)) +
  geom_boxplot() + xlab("X2") + ylab("") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
g2 <- ggplot(data_cemento, aes(y=x5)) +
  geom_boxplot() + xlab("X5") + ylab("") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

grid.arrange(g1,g2, ncol=2)



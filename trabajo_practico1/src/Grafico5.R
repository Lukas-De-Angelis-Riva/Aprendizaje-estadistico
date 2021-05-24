library(ggplot2)
library(gridExtra)

data_cemento <- read.table("cemento.txt", header=TRUE)
data_cemento <- data_cemento[2:7]
corr_matrix <- cor(data_cemento, method = "pearson")

g1 <- ggplot(data_cemento, aes(x=x1, y=y)) + geom_point() +
  theme(axis.text.y = element_text(angle = 0))
g2 <- ggplot(data_cemento, aes(x=x2, y=y)) + geom_point() +
  theme(axis.text.y = element_text(angle = 0))
g3 <- ggplot(data_cemento, aes(x=x3, y=y)) + geom_point() +
  theme(axis.text.y = element_text(angle = 0))
g4 <- ggplot(data_cemento, aes(x=x4, y=y)) + geom_point() +
  theme(axis.text.y = element_text(angle = 0))
g5 <- ggplot(data_cemento, aes(x=x5, y=y)) + geom_point() +
  theme(axis.text.y = element_text(angle = 0))

corr1 <- ggplot() + 
  annotate("text", x = 4, y = 25, size=8, label = round(corr_matrix['x1', 'y'], digits = 3)) +
  theme(
    axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x = element_blank(),
    axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
corr2 <- ggplot() + annotate("text", x = 4, y = 25, size=8, label = round(corr_matrix['x2', 'y'], digits = 3)) +
  theme(
    axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x = element_blank(),
    axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

corr3 <- ggplot() + annotate("text", x = 4, y = 25, size=8, label = round(corr_matrix['x3', 'y'], digits = 3)) +
  theme(
    axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x = element_blank(),
    axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
corr4 <- ggplot() + annotate("text", x = 4, y = 25, size=8, label = round(corr_matrix['x4', 'y'], digits = 3)) +
  theme(
    axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x = element_blank(),
    axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
corr5 <- ggplot() + annotate("text", x = 4, y = 25, size=8, label = round(corr_matrix['x5', 'y'], digits = 3)) +  theme(
  axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x = element_blank(),
  axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank())


grid.arrange(corr1, corr2, corr3, corr4, corr5, g1, g2, g3, g4, g5, ncol=5)


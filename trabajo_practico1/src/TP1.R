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


summary(reg)$sigma


Y <- data_cemento$y
X <- cbind(rep(1, nrow(data_cemento)), data_cemento$x1, data_cemento$x2, data_cemento$x3, data_cemento$x4, data_cemento$x5)
D <- solve(t(X)%*%X)
S <- sqrt(t(Y-X%*%reg$coefficients)%*%(Y-X%*%reg$coefficients)/(14-6))

1 - pt(1.219043, df=8) + pt(-1.219043, df=8)



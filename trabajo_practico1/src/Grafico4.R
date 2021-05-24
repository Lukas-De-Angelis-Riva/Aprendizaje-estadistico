library(ggplot2)
library(corrplot)
library(RColorBrewer)


data_cemento <- read.table("cemento.txt", header=TRUE)
data_cemento <- data_cemento[2:6]

corr_matrix <- cor(data_cemento, method = "pearson")
corr_matrix

#
colores = c()
for(corr in corr_matrix){
  colores = c(colores, ifelse(corr >= 0, "white", "black"))
}
color_ggplot2 = "#ebebeb"
corrplot(corr_matrix, addCoef.col=colores, 
         tl.col="black", tl.srt=0,tl.offset = 0.5, tl.cex = 0.9,
         col=whiteblack, number.cex=0.75, bg=color_ggplot2)




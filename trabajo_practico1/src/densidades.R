library(ggplot2)
set.seed(1492)

ggplot() + 
  xlim(-3.5, 3.5) + 
  stat_function(fun = dt, args = list(df = 30)) + 
  stat_function(fun = dt, args = list(df = 30), geom="area", xlim = c(-4,-2), fill = "darkgrey") + 
  stat_function(fun = dt, args = list(df = 30), geom="area", xlim = c(2,4), fill = "darkgrey") + 
  ylab(expression(f["T"](t))) + xlab("t") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.title=element_text(size=20,face="bold"))


ggplot() + 
  xlim(0, 5) + 
  stat_function(fun = df, args = list(df1=3, df2=5)) + 
  stat_function(fun = df, args = list(df1=3, df2=5), geom="area", xlim = c(3.5,6), fill = "darkgrey") + 
  ylab(expression(f["F"](x))) + xlab("x") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.title=element_text(size=20,face="bold"))


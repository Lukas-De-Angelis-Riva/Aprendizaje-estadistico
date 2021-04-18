# 1.7
# R has a built-in vector rivers which contains the lengths of major North American rivers.
#   a) Use ?rivers to learn about the data set.
#   b) Find the mean and standard deviation of the rivers data using the base R functions mean and sd.
#   c) Make a histogram (hist) of the rivers data.
#   d) Get the five number summary (summary) of rivers data.
#   e) Find the longest and shortest lengths of rivers in the set.
#   f) Make a list of all (the lengths of the) rivers longer than 1000 miles.

# a)
?rivers

# b)
mean(rivers)
sd(rivers)

# c)
hist(rivers, xlab="Length", col="chocolate")

# d)
summary(rivers)

# e)
min(rivers)
max(rivers)

# f)
rivers[rivers>1000]
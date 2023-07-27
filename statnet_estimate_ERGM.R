#Coursera course from Stanford University - Social and Economic Networks: Models and Analysis - Lecture 3 exercise

install.packages("statnet")
library(statnet)
data("florentine")
summary(flomarriage)

model1 <- ergm(flomarriage ~ edges)

summary(model1)

model2 <- ergm(flomarriage ~ edges + triangles)

summary(model2)

model3 <- ergm(flomarriage ~ edges + triangles + kstar(2))

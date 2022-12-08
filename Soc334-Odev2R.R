library(knitr)
install.packages('ggplot2', repos = "http://cran.us.r-project.org")
library(ggplot2)


head(edusavings)

summary(edusavings)

hist(edusavings$Education.expenditure.Percent.of.GNI.2019)
hist(edusavings$Unemployment.with.advanced.education.Percent.of.total.labor.force.2019)

ggplot(edusavings,aes(x=Education.expenditure.Percent.of.GNI.2019,y=Unemployment.with.advanced.education.Percent.of.total.labor.force.2019))+
  geom_point()+
  geom_smooth(method = "lm")

lm(formula=Education.expenditure.Percent.of.GNI.2019~Unemployment.with.advanced.education.Percent.of.total.labor.force.2019,data=edusavings)

fit <- lm(formula=Education.expenditure.Percent.of.GNI.2019~Unemployment.with.advanced.education.Percent.of.total.labor.force.2019,data=edusavings)
summary(fit)

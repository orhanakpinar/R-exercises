#install.packages("knitr", repos = "http://cran.us.r-project.org")
library(knitr)
# install.packages('ggplot2', repos = "http://cran.us.r-project.org")
library(ggplot2)

# Import Dataset "edusavings.tsv"

head(edusavings)

summary(edusavings)

sd(edusavings$Education.expenditure.Percent.of.GNI.2019)
sd(edusavings$Unemployment.with.advanced.education.Percent.of.total.labor.force.2019)
cor(edusavings$Unemployment.with.advanced.education.Percent.of.total.labor.force.2019, edusavings$Education.expenditure.Percent.of.GNI.2019)

hist(edusavings$Education.expenditure.Percent.of.GNI.2019)
hist(edusavings$Unemployment.with.advanced.education.Percent.of.total.labor.force.2019)

ggplot(edusavings,aes(x=Unemployment.with.advanced.education.Percent.of.total.labor.force.2019,y=Education.expenditure.Percent.of.GNI.2019))+
  geom_point()+
  geom_smooth(method = "lm")

lm(formula=Unemployment.with.advanced.education.Percent.of.total.labor.force.2019~Education.expenditure.Percent.of.GNI.2019,data=edusavings)

fit <- lm(formula=Unemployment.with.advanced.education.Percent.of.total.labor.force.2019~Education.expenditure.Percent.of.GNI.2019,data=edusavings)
summary(fit)

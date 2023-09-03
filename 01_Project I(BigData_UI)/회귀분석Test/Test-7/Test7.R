setwd("C:/Temp/R_data")
grape <- read.csv("data7.csv", header=T)
head(grape)
rownames(grape) <- c('2009','2010','2011','2012','2013','2014','2015','2016','2017','2018','2019','2020','2021')
grape
mod <- lm(revenue~Pcost+Hprice+Dcost+rain+sun+insolation+temp+gdp+area+yield, data=grape)
mod
summary(mod)

library(MASS)
mod2 <- stepAIC(mod, direction="both")
mod2
summary(mod2)

cor(grape, method="pearson")

install.packages("corrgram")
library(corrgram)
corrgram(grape, upper.panel = panel.conf)


mod3 <- lm(revenue~Pcost+Dcost+rain+sun+area, data=grape)
mod3
summary(mod3)
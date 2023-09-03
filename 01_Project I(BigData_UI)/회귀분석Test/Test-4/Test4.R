setwd("C:/Temp/R_data")
rice <- read.csv("data4.csv", header=T)
head(rice)
rownames(rice) <- c('2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016','2017','2018','2019','2020','2021','2022')
rice
library(MASS)
mod <- lm(revenue~temp+rain+wind+hpa+rh+sun+insolation+Pcost+area+yield+Hprice+Mprice+Dcost+gdp+population, data=rice)
mod
summary(mod)

plot(rice, pch=16, col="blue", main="¸ÅÃâ")

mod2 <- stepAIC(mod, direction="both")

mod3 <- lm(revenue~rain+hpa+rh+sun+insolation+area+yield+Hprice+Mprice+Dcost+gdp+population, data=rice)
mod3
summary(mod3)

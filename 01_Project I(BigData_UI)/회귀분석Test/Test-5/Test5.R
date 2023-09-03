setwd("C:/Temp/R_data")
rice <- read.csv("data5.csv", header=T)
head(rice)
rownames(rice) <- c('2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016','2017','2018','2019','2020','2021','2022')
rice

mod <- lm(revenue~temp+rain+sun+insolation+Pcost+area+yield+Hprice+Dcost+gdp, data=rice)
mod
summary(mod)

library(MASS)
mod2 <- stepAIC(mod, direction="both")
mod2
summary(mod2)

cor(rice, method="pearson")

install.packages("corrgram")
library(corrgram)
corrgram(rice, upper.panel=panel.conf)
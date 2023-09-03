setwd("C:/Temp/R_data")
spotato <- read.csv("spotato_matrix.csv", header=T)
head(spotato)
rownames(spotato) <- c('2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2018','2019','2020','2021')
spotato
mod <- lm(revenue~sun+isolation+temp+rain+Pcost+yield+Hprice+Dcost+gdp+area, data=spotato)
mod
summary(mod)

library(MASS)
mod2 <- stepAIC(mod, direction="both")
mod2
summary(mod2)

cor(spotato, method="pearson")

install.packages("corrgram")
library(corrgram)
corrgram(spotato, upper.panel=panel.conf)
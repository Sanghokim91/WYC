setwd("C:/Temp/R_data")
gochu <- read.csv("data.csv", header=T)
head(gochu)
rownames(gochu) <- c('2013','2014','2015','2016','2017','2018','2019','2020','2021','2022')
gochu
library(MASS)
mod <- lm(revenue~temp+rain+hpa+humidity+wind+Pcost+Dcost,data=gochu)
mod
summary(mod)

mod2 <- stepAIC(mod)
mod2
mod
summary(mod)

mod3 <- lm(revenue~temp+hpa+humidity+wind+Pcost+Dcost,data=gochu)
mod3
summary(mod3)

plot(gochu, pch=16, col="blue", main="¸ÅÃâ")

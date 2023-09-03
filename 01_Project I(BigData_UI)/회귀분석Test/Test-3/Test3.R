setwd("C:/Temp/R_data")
rice <- read.csv("data3.csv", header=T)
head(rice)
rownames(rice) <- c('2013','2014','2015','2016','2017','2018','2019','2020','2021','2022')
rice
library(MASS)
mod <- lm(revenue~temp+rain+wind+hpa+rh+sun+insolation+Pcost+area+yield+Hprice+Mprice+Dcost, data=rice)
mod
summary(mod)
rice

mod2 <- lm(revenue~temp+rain+wind+hpa+rh+sun+insolation+Pcost+area, data=rice)
mod2
summary(mod2)

mod3 <- lm(revenue~temp+rain+wind+hpa+rh+sun+insolation+Pcost, data=rice)
mod3
summary(mod3)

mod4 <- stepAIC(mod3)

mod5 <- lm(revenue~hpa+Pcost, data=rice)
mod5
summary(mod5)

mod6 <- lm(revenue~hpa+Pcost+Dcost, data=rice)
mod6
summary(mod6)

mod7 <- lm(revenue~hpa+Dcost, data=rice)
mod7
summary(mod7)

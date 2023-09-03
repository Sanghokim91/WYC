setwd("C:/Temp/R_data")
gochu <- read.csv("data2.csv", header=T)
head(gochu)
rownames(gochu) <- c('2013','2014','2015','2016','2017','2018','2019','2020','2021','2022')
gochu
library(MASS)

mod <- lm(revenue~temp+rain+hpa+rh+wind+sun+insolation+MchgRate+HchgRate+output, data=gochu)
mod
summary(mod)

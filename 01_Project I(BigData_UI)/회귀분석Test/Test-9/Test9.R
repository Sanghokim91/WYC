setwd("C:/Temp/R_data")
rice <- read.csv("data9.csv", header=T)
head(rice)
rownames(rice) <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38)
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

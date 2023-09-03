setwd("C:/Temp/R_data/matrix")
spinach <- read.csv("spinach_matrix.csv", header=T)
head(spinach)
spinach

mod <- lm(revenue~Pcost+Hprice+temp+rain+sun+insolation+gdp+area+yield, data=spinach)
mod
summary(mod)

#단계적 선택법을 사용한 회귀분석
library(MASS)
mod2 <- stepAIC(mod,direction = "both")
mod2
summary(mod2)

#23년도 매출을 다른변수 조건을 마지막 년도와 조건이 동일하다고 생각하고 매출 추출
Pcost <- c(spinach[19,3])
temp <- c(spinach[19,6])
gdp <- c(spinach[19,10])
area <- c(spinach[19,11])
result <- -8.218e+06 + 2.707e+00*Pcost + 4.831e+05*temp -2.074e+00*gdp -8.511e+02*area
result

#년도별 매출 추이를 분석하기 위해 값 입력
revenue <- c((spinach$revenue)/10000, result/10000)
revenue
year <- c((spinach$year), 2023)
year

#년도별 매출 추이 그래프 생성
plot(year, revenue, main="시금치 매출 변화", type="o", lty=1, lwd=2, col="red", xlab="년도", ylab="10a당 매출(만원)")
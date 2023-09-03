setwd("C:/Temp/R_data/matrix")
garlic <- read.csv("garlic_matrix.csv", header=T)
head(garlic)
garlic

mod <- lm(revenue~Pcost+Dcost+Hprice+temp+rain+sun+insolation+gdp+area+yield, data=garlic)
mod
summary(mod)

#단계적 선택법을 사용한 회귀분석
library(MASS)
mod2 <- stepAIC(mod,direction = "both")
mod2
summary(mod2)

#23년도 매출을 다른변수 조건을 마지막 년도와 조건이 동일하다고 생각하고 매출 추출
Dcost <- c(garlic[19,4])
temp <- c(garlic[19,6])
rain <- c(garlic[19,7])
sun <- c(garlic[19,8])
gdp <- c(garlic[19,10])
area <- c(garlic[19,11])
result <- -3.629e+06 -3.798e+04*Dcost + 6.777e+05*temp + 5.535e+03*rain + 1.703e+04*sun - 3.165e+00*gdp -7.064e+02*area
result

#년도별 매출 추이를 분석하기 위해 값 입력
revenue <- c((garlic$revenue)/10000, result/10000)
revenue
year <- c((garlic$year), 2023)
year

#년도별 매출 추이 그래프 생성
plot(year, revenue, main="마늘 매출 변화", type="o", lty=1, lwd=2, col="red", xlab="년도", ylab="10a당 매출(만원)")
setwd("C:/Temp/R_data/matrix")
stberry <- read.csv("stberry_matrix.csv", header=T)
head(stberry)
stberry

mod <- lm(revenue~Pcost+Dcost+Hprice+temp+rain+sun+insolation+gdp+area+yield, data=stberry)
mod
summary(mod)

#단계적 선택법을 사용한 회귀분석
library(MASS)
mod2 <- stepAIC(mod,direction = "both")
mod2
summary(mod2)

#23년도 매출을 다른변수 조건을 마지막 년도와 조건이 동일하다고 생각하고 매출 추출
Pcost <- c(stberry[19,3])
Dcost <- c(stberry[19,4])
Hprice <- c(stberry[19,5])
temp <- c(stberry[19,6])
sun <- c(stberry[19,8])
gdp <- c(stberry[19,10])
result <- -2.830e+07 + 2.085e+00*Pcost -2.638e+05*Dcost + 2.858e+03*Hprice + 2.079e+06*temp -2.873e+04*sun -2.370e+01*gdp
result

#년도별 매출 추이를 분석하기 위해 값 입력
revenue <- c((stberry$revenue)/10000, result/10000)
revenue
year <- c((stberry$year), 2023)
year

#년도별 매출 추이 그래프 생성
plot(year, revenue, main="딸기 매출 변화", type="o", lty=1, lwd=2, col="red", xlab="년도", ylab="10a당 매출(만원)")
setwd("C:/Temp/R_data/matrix")
pepper <- read.csv("pepper_matrix.csv", header=T)
head(pepper)
pepper

mod <- lm(revenue~Pcost+Dcost+Hprice+temp+rain+sun+insolation+gdp+area+yield, data=pepper)
mod
summary(mod)

#단계적 선택법을 사용한 회귀분석
library(MASS)
mod2 <- stepAIC(mod,direction = "both")
mod2
summary(mod2)

#23년도 매출을 다른변수 조건을 마지막 년도와 조건이 동일하다고 생각하고 매출 추출
Pcost <- c(pepper[19,3])
Hprice <- c(pepper[19,5])
sun <- c(pepper[19,8])
area <- c(pepper[19,11])
result <- 7.991e+06 + 1.886e+00*Pcost -1.553e+01*Hprice -1.472e+04*sun -2.270e+02*area
result

#년도별 매출 추이를 분석하기 위해 값 입력
revenue <- c((pepper$revenue)/10000, result/10000)
revenue
year <- c((pepper$year), 2023)
year

#년도별 매출 추이 그래프 생성
plot(year, revenue, main="고추 매출 변화", type="o", lty=1, lwd=2, col="red", xlab="년도", ylab="10a당 매출(만원)")
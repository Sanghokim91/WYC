setwd("C:/Temp/R_data/matrix")
kiwi <- read.csv("kiwi_matrix.csv", header=T)
head(kiwi)
kiwi

mod <- lm(revenue~Pcost+Hprice+temp+rain+sun+insolation+gdp, data=kiwi)
mod
summary(mod)

#단계적 선택법을 사용한 회귀분석
library(MASS)
mod2 <- stepAIC(mod,direction = "both")
mod2
summary(mod2)

#23년도 매출을 다른변수 조건을 마지막 년도와 조건이 동일하다고 생각하고 매출 추출
Pcost <- c(kiwi[14,3])
rain <- c(kiwi[14,7])
sun <- c(kiwi[14,8])
insolation <- c(kiwi[14,9])
result <- 3.163e+06 + 1.661e+00*Pcost + 1.820e+04*rain + 2.354e+04*sun -1.463e+04*insolation
result

#년도별 매출 추이를 분석하기 위해 값 입력
revenue <- c((kiwi$revenue)/10000, result/10000)
revenue
year <- c((kiwi$year), 2023)
year

#년도별 매출 추이 그래프 생성
plot(year, revenue, main="키위 매출 변화", type="o", lty=1, lwd=2, col="red", xlab="년도", ylab="10a당 매출(만원)")
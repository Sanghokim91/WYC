setwd("C:/Temp/R_data/matrix")
onion <- read.csv("onion_matrix.csv", header=T)
head(onion)
onion

mod <- lm(revenue~Pcost+Dcost+Hprice+temp+rain+sun+insolation+gdp+area+yield, data=onion)
mod
summary(mod)

#단계적 선택법을 사용한 회귀분석
library(MASS)
mod2 <- stepAIC(mod,direction = "both")
mod2
summary(mod2)

#23년도 매출을 다른변수 조건을 마지막 년도와 조건이 동일하다고 생각하고 매출 추출
Dcost <- c(onion[19,4])
Hprice <- c(onion[19,5])
temp <- c(onion[19,6])
rain <- c(onion[19,7])
insolation <- c(onion[19,9])
area <- c(onion[19,11])
result <- -1.950e+07 -7.163e+04*Dcost + 1.881e+02*Hprice + 8.248e+05*temp + 4.908e+03*rain + 1.946e+04*insolation -5.248e+02*area
result

#년도별 매출 추이를 분석하기 위해 값 입력
revenue <- c((onion$revenue)/10000, result/10000)
revenue
year <- c((onion$year), 2023)
year

#년도별 매출 추이 그래프 생성
plot(year, revenue, main="양파 매출 변화", type="o", lty=1, lwd=2, col="red", xlab="년도", ylab="10a당 매출(만원)")
setwd("C:/Temp/R_data/matrix")
spotato <- read.csv("spotato_matrix.csv", header=T)
head(spotato)
spotato

mod <- lm(revenue~Pcost+Dcost+temp+rain+sun+insolation+Hprice+gdp+area+yield, data=spotato)
mod
summary(mod)

#단계적 선택법을 사용한 회귀분석
library(MASS)
mod2 <- stepAIC(mod,direction = "both")
mod2
summary(mod2)

#23년도 매출을 다른변수 조건을 마지막 년도와 조건이 동일하다고 생각하고 매출 추출
Pcost <- c(spotato[19,3])
Dcost <- c(spotato[19,4])
temp <- c(spotato[19,5])
rain <- c(spotato[19,6])
sun <- c(spotato[19,7])
gdp <- c(spotato[19,10])
yield <- c(spotato[19,12])
result <- -2.841e+06 + 1.341e+00*Pcost + 1.192e+04*Dcost + 8.082e+04*temp + 2.398e+03*rain + 1.896e+03*sun + 7.914e-01*gdp -8.710e+00*yield
result

#년도별 매출 추이를 분석하기 위해 값 입력
revenue <- c((spotato$revenue)/10000, result/10000)
revenue
year <- c((spotato$year), 2023)
year

#년도별 매출 추이 그래프 생성
plot(year, revenue, main="고구마 매출 변화", type="o", lty=1, lwd=2, col="red", xlab="년도", ylab="10a당 매출(만원)")
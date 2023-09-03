setwd("C:/Temp/R_data/matrix")
rice <- read.csv("rice_matrix.csv", header=T)
head(rice)
rice

mod <- lm(revenue~Pcost+Dcost+Hprice+temp+rain+sun+insolation+gdp+area+yield, data=rice)
mod
summary(mod)

#단계적 선택법을 사용한 회귀분석
library(MASS)
mod2 <- stepAIC(mod,direction = "both")
mod2
summary(mod2)

#23년도 매출을 다른변수 조건을 마지막 년도와 조건이 동일하다고 생각하고 매출 추출
Pcost <- c(rice[19,3])
Dcost <- c(rice[19,4])
Hprice <- c(rice[19,5])
insolation <- c(rice[19,9])
area <- c(rice[19,11])
yield <- c(rice[19,12])
result <- 2.331e+06 + 8.507e-01*Pcost -3.746e+04*Dcost + 3.329e+01*Hprice -1.884e+03*insolation -1.606e+01*area + 1.738e+00*yield
result

#년도별 매출 추이를 분석하기 위해 값 입력
revenue <- c((rice$revenue)/10000, result/10000)
revenue
year <- c((rice$year), 2023)
year

#년도별 매출 추이 그래프 생성
plot(year, revenue, main="쌀 매출 변화", type="o", lty=1, lwd=2, col="red", xlab="년도", ylab="10a당 매출(만원)")
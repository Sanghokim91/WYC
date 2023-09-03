setwd("C:/Temp/R_data/matrix")
grape <- read.csv("grape_matrix.csv", header=T)
head(grape)
grape

mod <- lm(revenue~Pcost+Dcost+temp+rain+sun+insolation+gdp+area+yield, data=grape)
mod
summary(mod)

#단계적 선택법을 사용한 회귀분석
library(MASS)
mod2 <- stepAIC(mod,direction = "both")
mod2
summary(mod2)

#23년도 매출을 다른변수 조건을 마지막 년도와 조건이 동일하다고 생각하고 매출 추출
Pcost <- c(grape[13,3])
Dcost <- c(grape[13,4])
temp <- c(grape[13,5])
rain <- c(grape[13,6])
gdp <- c(grape[13,9])
area <- c(grape[13,10])
yield <- c(grape[13,11])
result <- -7.904e+07 + 2.073e+00*Pcost + 1.905e+05*Dcost + 2.599e+06*temp + 4.905e+04*rain + 6.459e+00*gdp -6.087e+04*area + 2.926e+03*yield
result

#년도별 매출 추이를 분석하기 위해 값 입력
revenue <- c((grape$revenue)/10000, result/10000)
revenue
year <- c((grape$year), 2023)
year

#년도별 매출 추이 그래프 생성
plot(year, revenue, main="포도 매출 변화", type="o", lty=1, lwd=2, col="red", xlab="년도", ylab="10a당 매출(만원)")
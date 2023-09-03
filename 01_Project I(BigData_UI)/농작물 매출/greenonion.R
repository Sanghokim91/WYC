setwd("C:/Temp/R_data/matrix")
greenonion <- read.csv("greenonion_matrix.csv", header=T)
head(greenonion)
greenonion

mod <- lm(revenue~Pcost+Hprice+temp+rain+sun+insolation+gdp+area+yield, data=greenonion)
mod
summary(mod)

#단계적 선택법을 사용한 회귀분석
library(MASS)
mod2 <- stepAIC(mod,direction = "both")
mod2
summary(mod2)

#23년도 매출을 다른변수 조건을 마지막 년도와 조건이 동일하다고 생각하고 매출 추출
Pcost <- c(greenonion[19,3])
temp <- c(greenonion[19,6])
insolation <- c(greenonion[19,9])
gdp <- c(greenonion[19,10])
area <- c(greenonion[19,11])
result <- -7.725e+05 + 2.799e+00*Pcost + 4.247e+05*temp -9.819e+03*insolation -8.849e-01*gdp -6.050e+02*area
result

#년도별 매출 추이를 분석하기 위해 값 입력
revenue <- c((greenonion$revenue)/10000, result/10000)
revenue
year <- c((greenonion$year), 2023)
year

#년도별 매출 추이 그래프 생성
plot(year, revenue, main="파 매출 변화", type="o", lty=1, lwd=2, col="red", xlab="년도", ylab="10a당 매출(만원)")
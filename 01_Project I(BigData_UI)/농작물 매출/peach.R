setwd("C:/Temp/R_data/matrix")
peach <- read.csv("peach_matrix.csv", header=T)
head(peach)
peach

mod <- lm(revenue~Pcost+Dcost+Hprice+temp+rain+sun+insolation+gdp+area+yield, data=peach)
mod
summary(mod)

#단계적 선택법을 사용한 회귀분석
library(MASS)
mod2 <- stepAIC(mod,direction = "both")
mod2
summary(mod2)

#23년도 매출을 다른변수 조건을 마지막 년도와 조건이 동일하다고 생각하고 매출 추출
Pcost <- c(peach[13,3])
Dcost <- c(peach[13,4])
Hprice <- c(peach[13,5])
temp <- c(peach[13,6])
sun <- c(peach[13,8])
insolation <- c(peach[13,9])
gdp <- c(peach[13,10])
area <- c(peach[13,11])
yield <- c(peach[13,12])
result <- -1.673e+06 + 1.390e+00*Pcost -1.822e+05*Dcost + 9.616e+02*Hprice -2.871e+05*temp + 2.562e+04*sun + 2.365e+04*insolation -1.090e+01*gdp + 3.123e+03*area + 1.377e+02*yield
result

#년도별 매출 추이를 분석하기 위해 값 입력
revenue <- c((peach$revenue)/10000, result/10000)
revenue
year <- c((peach$year), 2023)
year

#년도별 매출 추이 그래프 생성
plot(year, revenue, main="복숭아 매출 변화", type="o", lty=1, lwd=2, col="red", xlab="년도", ylab="10a당 매출(만원)")
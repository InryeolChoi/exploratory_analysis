rm(list=ls())

# 데이터 가져오기
class = c("numeric", "character", "factor", "numeric", "numeric")
pollution = read.csv("../../dataset/avgpm25.csv", colClasses = class)

# 구조 확인
head(pollution)
str(pollution)

# 요약통계량
summary(pollution$pm25)

# boxplot
boxplot(pollution$pm25, col="blue")
# pm25가 15보다 큰 케이스
## no.1
with(pollution, head(pm25[pm25 > 15]))
## no.2
subset(pollution, pm25 > 15)
## no.3
head(pollution[pollution$pm25 > 15,], 8)

# Histogram
hist(pollution$pm25, col="green", breaks=100)
rug(pollution$pm25)

# 저수준 그래픽 (예시 1)
hist(pollution$pm25, col="green")
abline(v = 12, lwd=2) # 기준치
abline(v = median(pollution$pm25), col="blue", lwd=4) # 

# 저수준 그래픽 (예시 2)
boxplot(pollution$pm25)
abline(h = 12) # 기준치

# Barplot
barplot(table(pollution$region), col="wheat")

## west와 east의 순서를 바꾸기
pollution$region = factor(pollution$region, levels = c("west", "east"))
barplot(table(pollution$region), col="purple")

# 다차원 boxplot
## 지역별 pm2.5을 도출
boxplot(pm25~region, data = pollution, col="red")

# 다차원 histogram
## 지역별 pm2.5의 수치 도출
par(mfrow=c(2,1))
hist(pollution[pollution$region == "west",]$pm25,
     col="green")
hist(pollution[pollution$region == "west",]$pm25,
     col="green")

# Scatterplot
## latitude에 따른 pm2.5
plot(pollution$pm25~pollution$latitude,
     xlab="latitude", ylab="pm25")
abline(h=12, lty=2)

## 추가 : 색으로 region 구분
plot(pollution$pm25~pollution$latitude,
     xlab="latitude", ylab="pm25",
     col=pollution$region)
abline(h=12, lty=2)

## 추가 : 지역에 따른 구분
par(mfrow=c(1,2), mar=c(5, 4, 2, 1))
with(subset(pollution, region == "West"),
     plot(pollution$pm25~pollution$latitude, 
          main="West", ylab="pm25", 
          xlab="latitude"))
with(subset(pollution, region == "East"),
     plot(pollution$pm25~pollution$latitude, 
          main="East", ylab="pm25", 
          xlab="latitude"))

par(mfrow=c(1,2), mar=c(5, 4, 2, 1))
plot(pm25~latitude,data=pollution[pollution$region=="west",],main="West")
plot(pm25~latitude,data=pollution[pollution$region=="east",],main="East")

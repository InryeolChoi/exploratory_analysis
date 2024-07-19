# 16. 분포확인

### 데이터 준비
rm(list=ls())
setwd("C:/Users/dlsfu/Desktop/EDA_rawData")
load("galaxy.RData")
attach(data)
str(data)

### 히스토그램과 kde
## 히스토그램
windows()
par(mfrow = c(1, 2))
hist(EastWest, ylim=c(0, 70), 
     main="Galaxy Data")
hist(EastWest, freq=F, ylim=c(0, 0.04), 
     main="Galaxy Data")

## 커널밀도추정
d = density(EastWest) # 밀도 파악
str(d) # 밀도의 구조
d$bw # 커널의 대역폭

# 커널의 히스토그램과 실제 데이터
windows()
par(mfrow = c(1, 2))
plot(d, main="Kernel density of Galaxy")
hist(EastWest, freq=F, main="Galaxy Data", 
     ylim=c(0, 0.04), xlim=c(-40, 40))
lines(d, lty=2, lwd=2)

# 대역폭이 미치는 영향
bandwidth = c(0.5, 1, 3, 7)
title = "Kernel density of Galaxy \n with bandwidth of "

windows()
par(mfrow=c(2, 2))
for(i in bandwidth){
    dens_galaxy = density(EastWest, bw=i)
    plot(dens_galaxy,
         xlim=c(-40, 40), ylim=c(0, 0.06),
         main = paste(title, i, sep=""))
}

### 상자그림
## 데이터 만들기
set.seed(101)
n = 400
simul = matrix(0, n, 3)

# 정규분포 추출
simul[, 1] = rnorm(n)

# 균등분포 추출 + 이상치
simul[1:(n-2), 2] = runif(n-2) * 2.4 - 1.2
simul[(n-1):n, 2] = c(-2.9, 2.9)

# 정규분포 3개의 혼합
tmp1 = rnorm(300) * 0.5
tmp2 = rnorm(50) * 0.4 - 2
tmp3 = rnorm(50) * 0.4 + 2

simul[, 3] = c(tmp1, tmp2, tmp3)

# 이름 붙이기
colnames(simul) = c("std. normal", "uniform + outlier", "mixed 3 normal")

# 커널밀도추정
dens_simul1 = density(simul[,1])
dens_simul2 = density(simul[,2])
dens_simul3 = density(simul[,3])

# 그래프
text2 = c("표준정규", "균등 + 이상치", "혼합")
windows()
par(mfrow=c(1, 2))
boxplot(simul, ylab="Values", 
        main="3 sample의 boxplot")
plot(dens_simul1, xlim=c(-4, 4), 
     ylim=c(0, 0.8), main="커널 밀도추정")
lines(dens_simul2, col=2)
lines(dens_simul3, col=4)
legend("topleft", text2, lty=1, col=c(1,2,4))

### 확률플롯
# 확률 얻기
sort_ew = sort(EastWest)
num = length(sort_ew)
probs = ((1:num)-0.5)/num

# 이론적인 표준정규분포 확인
q = qnorm(probs)

# 확률플롯 확인
windows()
par(mfrow=c(1, 1))
plot(q, sort_ew, xlab = "표준정규분포", 
     ylab = "sorted EastWest")

# 이론적 분포 vs 정렬데이터
q_ew = quantile(EastWest, prob=(0:99)/100)
q_std = qnorm((0:99)/100)
    
windows()
par(mfrow=c(1, 3))
plot(q, sort_ew, xlab="표준정규 사분위수",
     ylab="sorted EastWest",
     main="EastWest의 확률플롯")
plot(q_std, q_ew, xlab="표준정규분포",
     ylab="sample 사분위수",
     main="EastWest의 QQ플롯")
qqnorm(EastWest)

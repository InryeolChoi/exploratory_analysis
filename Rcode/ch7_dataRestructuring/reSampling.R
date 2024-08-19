rm(list=ls())
set.seed(101)

x = 7 # 추출 횟수
n = 1000 # 벡터의 크기
original = rep(NA, n) # 빈 벡터 제작
bootstrap = rep(NA, n) # 빈 벡터 제작

# original: 반복할 때마다 모집단에서 추출
for(i in 1:n){
    data_A = runif(x)
    original[i] = sd(data_A)
}

# sample : 모집단 추출 1번. 이후 내부에서 추출
data_B = runif(x)
for(i in 1:n){
    sample = data_B[sample.int(x, replace=T)]
    bootstrap[i] = sd(sample)
}

# 요약치 비교하기
summary(original)
summary(bootstrap)

# 그래프로 비교하기
par(mfrow=c(1,2))
hist(original)
hist(bootstrap)

# 사분위수 비교하기
quantile(original, prob=c(0.025, 0.975))
quantile(bootstrap, prob=c(0.025, 0.975))

## 95% 신뢰수준
mean(data_A)-qnorm(0.975)*sqrt(var(data_A))/sqrt(x)
mean(data_A)+qnorm(0.975)*sqrt(var(data_A))/sqrt(x)


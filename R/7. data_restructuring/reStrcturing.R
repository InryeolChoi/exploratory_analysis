rm(list=ls())

# 세팅
library(reshape)
set.seed(101)
a = 101
b = 2
c = 50

# 데이텨 생성
x = cbind(rep(1:a, each=2), rep(1:b, times=b), 
          matrix(rnorm(a*b*c), nrow=a*b))
datas1 = as.data.frame(x)
names(datas1) = c("ID", "time", paste(rep(LETTERS[1:25], each=2), 
                  rep(c(1, 2), times=25), sep="."))

# 구조보기
str(datas1)

# melt 이용하기
datas2 = melt(datas1,  id=c("ID", "time"))
head(datas2)

# cast 이용하기
datas3 = as.data.frame(cast(datas2, ID+time~variable))
head(datas3[, 1:6])
all.equal(datas1, datas3)
summary(datas1-datas3)

# cast()를 이용한 문항별 평균점수
cast(datas2, time~variable, mean)

# with()를 이용한 A.1의 문항별 평균점수
with(datas1, tapply(A.1, time, mean))

# merge() 이용하기
v = 5
x2 = cbind(rep(1:(a-v), each=2), 
             rep(1:b, times=a-v), 
             matrix(rnorm((a-v)*b*2), nrow=(a-v)*b))
datas_svy = as.data.frame(x2)
names(datas_svy)<-c("ID", "time", "HT", "WT")

dim(datas1)
dim(datas.svy)

merged_1 = merge(datas1[, 1:4], datas_svy, by="ID")
str(merged_1)
head(merged_1)

merged_2 = merge(datas1[, 1:4], datas.svy, by=c("ID", "time"),
                 all.x=T)
str(merged_2)
head(merged_2)

names(datas5)[apply(is.na(datas5), 2, sum)>0]

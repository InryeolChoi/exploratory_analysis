rm(list=ls())

# 파일 불러오기
ozone = read_csv("../../dataset/hourly_44201_2014_ver2.csv")
names(ozone) = make.names(names(ozone))

# 초기분석
dim(ozone)

nrow(ozone)
ncol(ozone)

str(ozone)
head(ozone)
tail(ozone)

# 결측값 보기
## hourly ozone data 보기
table(ozone$Time.Local)
## 주 이름 나열
unique(ozone$State.Name)

# 이상치 확인
summary(ozone$Sample.Measurement)
quantile(ozone$Sample.Measurement, seq(0, 1, 0.1))

# 오존 레벨 top 10인 지역
## aggregate 함수로 만들어주기
ranking = aggregate(Sample.Measurement~State.Name+County.Name, 
ozone, mean)
## 이름 바꿔주기 : ozone으로
names(ranking)[3] = "ozone"
## ranking의 order 정리
ranking = ranking[order(ranking$ozone, decreasing=T),]

# 캘리포니아, 마리포사의 관측치 찾아보기
## 이름 알아보기
mariposa.data = ozone[(ozone$State.Name == "California" & 
ozone$County.Name == "Mariposa"),]

## mariposa의 갯수 알아보기
nrow(mariposa.data)

## 시간 순으로 체크
with(mariposa.data, table(Method.Name, Time.Local))

## 월별 오존농도를 체크해보자
ozone$Date.Local = as.Date(ozone$Date.Local)
ozone$month = factor(months(ozone$Date.Local))
data = aggregate(Sample.Measurement~month, ozone, mean)
names(data)[2] = "ozone"
data

# 부트스트랩 샘플을 이용해 샘플을 뽑아나고, 새 ranking을 만들어보자.
set.seed(10234)
N = nrow(ozone)
idx = sample(N, N, replace=T)
ozone2 = ozone[idx, ]

# 이후 기존의 ranking과 새 ranking(ranking2)를 붙여보자.
# ranking2 만들기
ranking2 = aggregate(Sample.Measurement~State.Name + County.Name, 
ozone2, mean)
names(ranking2)[3] = "ozone"
ranking2 = ranking2[order(ranking2$ozone, decreasing=T),]

# ranking, ranking2 붙이고 확인
cbind(head(ranking, 10), head(ranking2, 10))
cbind(tail(ranking, 10), tail(ranking2, 10))

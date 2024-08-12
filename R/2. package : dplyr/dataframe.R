# 경로 부르기
chicago = readRDS("../../dataset/chicago.rds")

# 훓어보기
dim(chicago) # dim = dimension
str(chicago) # str = structure
head(chicago, 3) # 앞에만 보기
tail(chicago, 5) # 뒤에만 보기

# 변수 고르기
## 우선 순서대로 변수 이름을 본다.
names(chicago)

## city ~ dptp가 몇 번째인지 본 뒤, subset()과 select()를 응용해 뽑는다.
head(subset(chicago, select = c(1:3)), 6)
names(chicago)
# 1st answer
head(subset(chicago, select = -c(1:3)), 6)
# 2nd answer
head(chicago[, -c(1:3)], 6)

# 3. 필터링 & 순서 맞추기
### 1번) 2로 끝나는 변수들만 보기.
str(chicago[, grep("2$", names(chicago))])
### 2번) d로 시작하는 변수들만 보기
str(chicago[, grep("^d", names(chicago))])
### 3번) 숫자가 포함된 변수들 보기 (자체)
str(chicago[, grep("[1-9]", names(chicago))])
### 4번) 대문자가 포함된 변수들 보기 (자체)
str(chicago[, grep("[[:upper:]]", names(chicago))])

### 필터링 문제1 : 변수 PM2.5가 30보다 큰 chicago의 row 뽑고, summary 내기
names(chicago)
chic.f = chicago[(chicago$pm25 < 100 & !is.na(chicago$pm25)),]
summary(chic.f$pm25tmean2)

### 필터리 문제2 : 변수 PM2.5가 30보다 크고, 변수 tmpd가 80보다 큰 chicago의 row 뽑기
names(chicago)

#### 조건을 2개 걸기 = 괄호 2개 사이에 & 기호를 넣는다.
#### 열 데이터이므로 역시 콤마를 조건 뒤에 반드시 넣어야 한다.
chicago[(chicago$pm25tmean2 > 30) & (chicago$tmpd > 80),]
```
### 순서 맞추기 : 날짜 순서에 따라 데이터 맞추기
chic.o3 = chicago[order(chicago$date, decresing=T)]
head(chic.o3[, c("date", "pm25tmean2")])

# 4. 변수 이름 바꾸기
## names() 함수를 이용
names(chicago)
names(chicago)[c(3, 5)] = c("dewpoint", "pm25")

## 바꼈는지 재확인
names(chicago)

# 5. 새 열 만들기
# with() 함수를 이용한다.
# with(데이터프레임, 계산/변화 내용)
chicago$pm25detrend = with(chicago, pm25 - 
mean(pm25, na.rm=T))

chicago$pm10detrend = with(chicago, pm10tmean2 - 
mean(pm10tmean2, na.rm=T))

chicago$o3detrend = with(chicago, o3tmean2 - 
mean(o3tmean2, na.rm=T))

# 6. 그룹으로 묶기
## 1번 해답
chicago$year = as.POSIXlt(chicago$date)$year + 1900
with(chicago, cbind(
    tapply(pm25, year, mean, na.rm=TRUE),
    tapply(o3tmean2, year, mean, na.rm=TRUE), 
    tapply(no2tmean2, year, mean, na.rm=TRUE))
    )

## 2번 해답
qq = quantile(chicago$pm25, seq(0, 1, 0.2), na.rm=TRUE)
chicago$pm25.quint = with(chicago, cut(pm25, qq))
with(chicago, 
     cbind(
        tapply(o3tmean2, pm25.quint, mean, na.rm=TRUE),
        tapply(no2tmean2, pm25.quint, mean, na.rm=TRUE)
        )
    )

# 15. 평활화기법

##### environmental data
### 데이터 준비
rm(list=ls())
setwd("C:/Users/dlsfu/Desktop/EDA_rawData")
load("environmental.Rdata")
ls()
str(data)
attach(data)

# loess fit
x = 2/3
y = 1
data_loess = loess(Ozone~Temperature, 
                   span=x, degree=y)


# 산점도 그리기
windows()
plot(Temperature, Ozone, pch=16, 
     main="Environmental Data", 
     xlab="온도(F)", ylab="Ozone(PPB)", 
     axes=F, xlim=c(55, 100), ylim=c(0, 180))
axis(1, seq(55, 100, by=5), seq(55, 100, by=5))
axis(2, seq(0, 180, by=20), seq(0, 180, by=20))
ord_temp = order(Temperature)
lines(Temperature[ord_temp], fitted(data_loess)[ord_temp], lwd=2)
lines(Temperature[ord_temp], )


# 리샘플링 신뢰구간
sample_data = sample(data_loess$y, 30, replace=TRUE)
n = length(sample_data) # 반복횟수
x = 50000 # 부트스트랩 반복횟수

resamples = matrix(sample(sample_data, n*x, replace=TRUE))s

means = apply(resamples, 1, mean) #샘플별 평균

quantile(means, c(0.025, 0.975))

detach(data)

##### environmental data
### 데이터 준비
dev.off()
rm(list=ls())
setwd("C:/Users/dlsfu/Desktop/EDA_rawData")
load("galaxy.Rdata")
str(data)
attach(data)

# loess fit
galaxy_loess = loess(Velocity~EastWest*NorthSouth, span=0.25, degree=2)

# NorthSouth를 조건부로 loess fit 그리기
t1 = "East-West Coordinate (arcsec)"
t2 = "Given: North-South Coordinate (arcsec)"
windows()
coplot(Velocity~EastWest|NorthSouth, 
       panel = function(x, y, col, pch){
           idx = order(x)
           points(x, y, pch=pch, col=col)
           lines(x[idx], predict(loess(y~x))[idx], lwd=2, col=4)
       }, xlab=c(t1, t2), ylab="Velocity")


# 등고선 그리기
sort_ew = sort(EastWest)
sort_ns = sort(NorthSouth)
pred_val = predict(galaxy_loess, expand.grid(data.frame(EastWest=sort_ew, NorthSouth=sort_ns)))

windows()
contour(sort_ew, sort_ns, pred_val, 
        nlevels=15, main="Contour plot of loess fit to Velocity", xlab=t1, ylab=t2)

detach(data)











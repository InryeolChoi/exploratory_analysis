rm(list=ls())

# 데이터생성 + 시뮬레이션
set.seed(1234)
x = rnorm(12, rep(1:3, each=4), 0.2)
y = rnorm(12, rep(c(1, 2, 1), each=4), 0.2)
plot(x, y, col="blue", pch=19, cex=2)
text(x + 0.05, y + 0.05, labels=as.character(1:12))


# 거리행렬
## 거리행렬 만들기
dataFrame = data.frame(x=x, y=y)
## 행렬로 만들기
rdist_xy = as.matrix(dist(dataFrame))
## 최솟값인 대각원소 없애주기
diag(rdist_xy) = diag(rdist_xy) + 100000
# 가장 가까운 점
ind1 = which(rdist_xy == min(rdist_xy), arr.ind=TRUE)
ind1
# 그래프 그리기
plot(x, y, col="blue", cex=2, pch=19)
text(x + 0.05, y + 0.05, as.character(1:12))
points(x[ind1[1, ]], y[ind1[1, ]], col="orange", cex=2, pch=19)


# 덴드로그램 (1)
## 덴드로그램 만들기
hcluster = hclust(dist(dataFrame))
dendro = as.dendrogram(hcluster)
## 덴드로그램 자르기
cutDendro = cut(dendro, h=(hcluster$height[1] + 0.00001))
## 덴드로그램만 그리기
cutDendro$lower
plot(cutDendro$lower[[11]], yaxt="n", main="Begin building tree")
## 플롯과 같이 그리기
par(mfrow=c(1, 2))
plot(x, y, col="blue", pch=19, cex=2, main="Data")
text(x+0.05, y+0.05, labels=as.character(1:12))
points(x[ind1[1, ]], y[ind1[1, ]], col="orange", pch=19, cex=2)
plot(cutDendro$lower[[11]], yaxt="n", main="Begin building tree")

# 덴드로그램 (2)
## 플롯 준비 : 2번째로 가까운 point 찾기
nextmin = rdist_xy[order(rdist_xy)][3]
ind2 = which(rdist_xy == nextmin, arr.ind=TRUE)
ind2
## 덴드로그램 : 두 번째 파트 자르기
cutDendro2 = cut(dendro, h=(hcluster$height[2] + 0.00001))
hcluster$height
cutDendro2$lower
## 덴드로그램 그리기
par(mfrow=c(1,3))
plot(x, y, col="blue", pch=19, cex=2)
text(x+0.05, y+0.05, labels=as.character(1:12))
points(x[ind1[1, ]], y[ind1[1, ]], col="orange", pch=19, cex=2)
points(x[ind2[1, ]], y[ind2[1, ]], col="red", pch=19, cex=2)
plot(cutDendro2$lower[[10]], yaxt="n")
plot(cutDendro2$lower[[5]], yaxt="n")


# 히트맵
if ("gplots" %in% rownames(installed.packages())) {
  print("설치되어 있음")
} else {
  install.packages("gplots")
}
## 히트맵으로 보기
library(gplots)
datamat = as.matrix(data.frame(x=x, y=y))
# density.info = none
heatmap.2(datamat, trace='none', notecol = 'black', cellnote=round(datamat, 3), density.info = 'none')
## row의 덴드로그램 포함
heatmap.2(datamat, trace='none', notecol='black', 
          cellnote=round(datamat, 3), density.info='none', 
          dendrogram="row")
## col의 덴드로그램 포함
heatmap.2(datamat, trace='none', notecol='black', 
          cellnote=round(datamat, 3),
          density.info='none',
          dendrogram="col")


# 거리: 유클리드, linkage: complete
hc1 = hclust(dist(dataFrame))
plot(hc1, main="Euclidean distance/complete")

hc1$height
cut_hc1 = cut(hc1, h = hc1$height[1] + 0.00001)

# 거리: absolute, linkage: centroid
hc2 <- hclust(dist(dataFrame, method="minkowski", p=1), method="single")
plot(hc2, main="Absolute/Centroid")

# x만 클러스터링
hClustering2 <- hclust(dist(x))
plot(hClustering2)

# y만 클러스터링
hClustering3 <- hclust(dist(y))
plot(hClustering3)

# 정규화
dataFrame2<-scale(dataFrame)
hClustering4 <- hclust(dist(dataFrame2))
plot(hClustering4)

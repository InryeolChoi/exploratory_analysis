rm(list=ls())

# 데이터 준비 & plot 그리기 
rm(list=ls())
set.seed(1234)
x = rnorm(12, mean=rep(1:3, each=4), sd=0.2)
y = rnorm(12, mean = rep(c(1, 2, 1), each=4),
          sd=0.2)
plot(x, y, col="blue", cex=2, pch=19)
text(x+0.05, y+0.05, as.character(1:12))


# kmeans() 써보기
library(stats)
dataFrame = data.frame(x, y)
kmeans_obj = kmeans(dataFrame, centers=3, iter.max=100)
names(kmeans_obj)
kmeans_obj$cluster

# 최종결과
plot(x, y, col=kmeans_obj$cluster, cex=2, pch=19)
points(kmeans_obj$centers, pch=3, cex=2)


# 히트맵 - 데이터 준비
set.seed(1234)
datamat = as.matrix(dataFrame)[sample(1:12),]
kmeans_obj = kmeans(datamat, centers=3)
## 히트맵 만들기 1
library(gplots)
par(mfrow=c(1,2))
# density.info = none
heatmap.2(datamat, trace='none', notecol = 'black', density.info = 'none', dendrogram="none")


# 클러스터 평가하기
library(cluster)

data.dist = dist(datamat)
data.km2 = kmeans(datamat, centers=2)
data.km3 = kmeans(datamat, centers=3)
data.km4 = kmeans(datamat, centers=4)

data.km2.sil = silhouette(data.km2$cluster, data.dist)
plot(data.km2.sil)

heatmap.2(, trace='none', notecol = 'black', density.info = 'none', dendrogram="none")

# 히트맵 만들기 2
par(mfrow=c(1,2))
image(t(datamat)[, nrow(datamat):1], yaxt="n", main="original data")
image(t(datamat)[, order(kmeans_obj$cluster)], yaxt="n", main="clustered data")


# 사각형 씌우기
plot(hcluster)
rect.hclust(hcluster, k=2)

plot(hcluster)
rect.hclust(hcluster, k=4)


# 실루엣 통계량 만들기
library(cluster)
data.dist = dist(datamat)
data.km2 = kmeans(datamat, centers=2)
data.km3 = kmeans(datamat, centers=3)
data.km4 = kmeans(datamat, centers=4)


data.km2.sil = silhouette(data.km2$cl, data.dist)
data.km3.sil = silhouette(data.km3$cl, data.dist)
data.km4.sil = silhouette(data.km4$cl, data.dist)


par(mfrow=c(1, 3))
plot(data.km2.sil)
plot(data.km3.sil)
plot(data.km4.sil)

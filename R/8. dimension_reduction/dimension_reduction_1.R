# 14. 차원축소 1
### 데이터 준비하기
rm(list=ls())
setwd("C:/Users/dlsfu/Desktop/EDA_rawData")

set.seed(12345)
dataMat = matrix(rnorm(400), nrow = 40)

# 원본 데이터
image(1:10, 1:40, t(dataMat)[, nrow(dataMat):1])
heatmap(dataMat)

# 패턴 넣기
set.seed(678910)
for(i in 1:40){
    coinflip = rbinom(1, size=1, prob=0.5)
    if(coinflip){
        dataMat[i,]=dataMat[i,] + rep(c(0,3), each=5)
    }
}

# 클러스터링
image(1:10, 1:40, t(dataMat)[, nrow(dataMat):1])
heatmap(dataMat)

# 행과 열 평균 확인
hh = hclust(dist(dataMat))
ordered = dataMat[hh$order, ]

par(mfrow=c(1, 3))
image(t(ordered)[, nrow(ordered):1])
plot(rowMeans(ordered), 40:1,
     xlab="Row mean", ylab="Row", pch=19)
plot(colMeans(ordered), xlab="Column",
     ylab="Column Mean", pch=19)

### 패턴 1개일 때
# svd => 특이값벡터 => 근사행렬
svd1 = svd(scale(ordered))
approx = with(svd1, outer(u[, 1], v[, 1]))

# 기존데이터 vs 특이값분해 
par(mfrow=c(1, 2))
image(t(ordered)[, nrow(ordered):1], main="Original Matrix")
image(t(approx)[, nrow(approx):1], main = "Approximated Matrix")

# 첫번째 분산의 힘
const = ordered * 0
for(i in 1:dim(ordered)[1]){
    const[i, ] = rep(c(0,1), each=5)
}
svd2 = svd(const)

windows()
par(mfrow=c(1, 3))
image(t(const)[, nrow(const):1], 
      main="original data")
plot(svd2$d, xlab = "열", ylab = "특이값", 
     pch = 19)
plot(svd2$d^2/sum(svd2$d^2), xlab = "열", 
     ylab = "설명된 분산비율", pch = 19)

# 비교: PCA vs SVD
svd1 = svd(scale(ordered))
pca1 = prcomp(ordered, scale=TRUE)

par(mfrow=c(1,1))
plot(pca1$rotation[, 1], svd1$v[, 1], pch=19, xlab="주성분1", 
ylab="우측 특이값벡터")
abline(c(0, 1))


### 패턴 2개
# 데이터 만들기
set.seed(678910)
dataMat2 = matrix(rnorm(400), nrow=40)

# 패턴 만들기
for(i in 1:40){
    coinflip1 = rbinom(1, size=1, prob=0.5)
    coinflip2 = rbinom(1, size=1, prob=0.5)
    if(coinflip1){
        dataMat2[i, ] = dataMat2[i, ] + rep(c(0,5), each=5)
    }
    if(coinflip2){
        dataMat2[i, ] = dataMat2[i, ] + rep(c(0,5), 5)
    }
}

# 순서 정렬
hh2 = hclust(dist(dataMat2))
ordered2 = dataMat2[hh2$order, ]

# 시각화
windows()
par(mfrow=c(1,3))
image(t(ordered2)[, nrow(ordered2):1])
plot(rep(c(0, 1), each=5), pch=19, 
     xlab="column", ylab="pattern 1", 
     main="pattern 1")
plot(rep(c(0, 1), 5), pch=19, 
     xlab="column", ylab="pattern 2", 
     main="pattern 2")

# 특이값 벡터 비교
svd3 = svd(scale(ordered2))
windows()
par(mfrow=c(1, 2))
plot(svd3$v[, 1], pch=19, 
     xlab="column",
     ylab="1st right singular vector")
plot(svd3$v[, 2], pch=19, 
     xlab="column",
     ylab="2nd right singular vector")

# 설명된 분산 보기
par(mfrow=c(1, 2))
plot(svd3$d, pch=19,
     xlab="column", ylab="Singular value")
plot(svd3$d^2/sum(svd3$d^2), pch=19,
     xlab="column", ylab="percent of var. explained")


### 얼굴 데이터
rm(list=ls())
setwd("C:/Users/dlsfu/Desktop/EDA_rawData")
load("face.rda")
par(mfrow=c(1,1))

# 얼굴 보기
image(t(faceData)[, nrow(faceData):1])

# 설명된 분산의 비율
svd_face = svd(scale(faceData))
u = svd_face$u
d = svd_face$d
v = svd_face$v
svd_face

plot(d^2/sum(d^2), pch=19, 
     xlab="Singular Vector", 
     ylab="Var. explained")

# 다양한 근사행렬
approx1 = u[, 1] %*% t(v[, 1]) * d[1]
approx5 = u[, 1:5] %*% diag(d[1:5]) %*% t(v[, 1:5])
approx10 = u[, 1:10] %*% diag(d[1:10]) %*% t(v[, 1:10])

windows()
par(mfrow=c(1, 4))
image(t(approx1)[, nrow(approx1):1], main="1 vector")
image(t(approx5)[, nrow(approx5):1], main="5 vector")
image(t(approx10)[, nrow(approx10):1], main="10 vector")
image(t(faceData)[, nrow(faceData):1], main="origin")


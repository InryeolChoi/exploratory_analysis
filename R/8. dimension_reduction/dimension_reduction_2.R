# 14. 차원축소 2
rm(list=ls())
setwd("C:/Users/dlsfu/Desktop/EDA_rawData")
load("stockreturns.RData")
mu = matrix(apply(stocks, 2, mean), dim(stocks)[1], dim(stocks)[2], byrow=T)
sig = matrix(apply(stocks, 2,sd), dim(stocks)[1], dim(stocks)[2], byrow=T)

stocks_new = (stocks - mu) / sig

### PCA: stock data 1
stock_pca1 = prcomp(stocks)
stock_pca2 = prcomp(stocks_new)

windows()
par(mfrow=c(1, 2))
screeplot(stock_pca1)
screeplot(stock_pca2)

windows()
par(mfrow=c(1, 2))
biplot(stock_pca1, 1:2)
abline(h=0, lty=3, col='red')
abline(v=0, lty=3, col='red')
biplot(stock_pca2, 1:2)
abline(h=0, lty=3, col='red')
abline(v=0, lty=3, col='red')


# biplot()
windows()
par(mfrow=c(1, 2))
with(stock_pca1, plot(rotation[, 1], rotation[, 2], type='n', xlab='PC1', ylab='PC2'))
with(stock_pca1, text(rotation[, 1], rotation[, 2], 1:10))
abline(h=0, lty=3, col='red')
abline(v=0, lty=3, col='red')

# biplot()
biplot(stock_pca1, 1:2)
abline(h=0, lty=3, col='red')
abline(h=0, lty=3, col='red')

### FA: stock data
kval = 3
stock_fa = factanal(stocks_new, kval, rotation="none", score="reg")

str(stock_fa)
stock_fa
stock_fa$loadings

stock_fa2 = factanal(stocks_new, kval, rotation="none")

# 비교1 : 요인분석의 상관관계 vs 원 상관관계
all.equal(stock_fa$correlation, cor(stocks))

# 비교2 : 
x1 = apply(stocks_new, 2, var)
x2 = apply(stock_fa$loadings^2, 1, sum)
all.equal(x1-x2, stock_fa$uniquenesses)

# 회전행렬 만들기
stock_fa_R1 = factanal(stocks_new, kval, rotation="varimax")
stock_fa_R2 = factanal(stocks_new, kval, rotation="promax")


## 텍스트
text = "Stocks (No rotation)"
text2 = "Stocks (Varimax)"
text3 = "Stocks (Promax)"

## 새 창 만들기 
windows()
par(mfcol=c(2,3))

# 회전없이 갈 때
with(stock_fa, plot(loadings[, 1], loadings[, 2], xlim=c(-1, 1), 
ylim=c(-1, 1), main=text, xlab='Factor 1', ylab="Factor 2"))
with(stock_fa, text(loadings[, 1], loadings[, 2], labels = 1:10, pos=4))
abline(v=0)
abline(h=0)

with(stock_fa, plot(loadings[, 1], loadings[, 3], xlim=c(-1, 1), 
ylim=c(-1, 1), main=text, xlab='Factor 1', ylab="Factor 3"))
abline(v=0)
abline(h=0)

# varimax 일 때
with(stock_fa_R1, plot(loadings[, 1], loadings[, 2], xlim=c(-1, 1), ylim=c(-1, 1), main=text2, 
xlab='Factor 1', ylab="Factor 2"))
with(stock_fa_R1, text(loadings[, 1], loadings[, 2], labels = 1:10, pos=4))
abline(v=0)
abline(h=0)

with(stock_fa_R1, plot(loadings[, 1], loadings[, 3], xlim=c(-1, 1), 
ylim=c(-1, 1), main=text2, 
xlab='Factor 1', ylab="Factor 3"))
with(stock_fa_R1, text(loadings[, 1], loadings[, 3], labels = 1:10, pos=4))
abline(v=0)
abline(h=0)

# promax로 갈 때
with(stock_fa_R2, plot(loadings[, 1], loadings[, 2], xlim=c(-1, 1), 
ylim=c(-1, 1), main=text3, 
xlab='Factor 1', ylab="Factor 2"))
with(stock_fa_R2, text(loadings[, 1], loadings[, 2], labels = 1:10, pos=4))
abline(v=0)
abline(h=0)

with(stock_fa_R2, plot(loadings[, 1], loadings[, 3], xlim=c(-1, 1), 
ylim=c(-1, 1), main=text3, 
xlab='Factor 1', ylab="Factor 3"))
with(stock_fa_R2, text(loadings[, 1], loadings[, 3], labels = 1:10, pos=4))
abline(v=0)
abline(h=0)


# 요인점수 시각화
text = "Stocks (No rotation)"
windows()
par(mfrow=c(1, 2))
with(stock_fa, plot(scores[, 1], scores[, 2], xlim=c(-4, 4), 
ylim=c(-4,4), main=text, 
xlab="Factor 1", ylab="Factor 2"))
abline(v=0)
abline(h=0)

with(stock_fa, plot(scores[, 1], scores[, 2], xlim=c(-4, 4), 
ylim=c(-4,4), main=text, 
xlab="Factor 1", ylab="Factor 2"))
abline(v=0)
abline(h=0)


# 
cor(stock_fa$scores)
as.matrix(cor(stock_fa$scores))

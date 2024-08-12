rm(list=ls())

## 연습
data = read.csv("abcde.csv", sep="", header="")
data2 = read.table("abcde.txt", sep="\t", header=T)
data3 = read.table("abcde.txt", na.string = "NA")

## RDS 불러오기
library(readr)
readRDS("")

## 엑셀 파일 불러오기
install.packages("readxl")
library(readxl)
tmp3 = read_excel("abced.xlsx", sheet=1)

## RDatat 파일 저장
save(tmp3, file="abcde.RData")
save.image(file="abced.RData")

load("abcde.RData")

## 내보내기
write.csv(pm25.summary, file="pm25_summary.csv")

## 그림 저장하기
png("example_plot.png", height=960)
plot(1:10, 1:10, col='red', pch=19)

dev.off()

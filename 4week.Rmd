

---
title: "4주차 코드"
author: "이철기"
date: '2020 4 6 '
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## SW 프로그래밍 집중교육 4주차

ㅍ###  1. 데이터 다운로드 ([국가 공공 데이터 포털](https://www.data.go.kr/search/index.do))

한국행정연구원 설문조사 데이터 현황 설문지 데이터 다운받기([한국행정연구원 설문조사](https://www.data.go.kr/dataset/15004274/fileData.do))

'haven' package를 사용하여 SPSS 파일을 불러올수 있다.
```{R}
#install.packages("haven")
library(haven)
#SPSS 데이터 파일인 SAV 파일을 불러오기
temp <- read_sav("data.sav")
#데이터 프레임으로 데이터를 변경
data <- as.data.frame(temp)
data2 <- data.frame(data$q3_1,data$q3_2,data$q3_3)
```

---
### 2. 기술통계량 구하기 
'psych' package를 사용하여 데이터의 기술통계량 구할 수 있다.
```{R}
#install.packages("psych")
library(psych)
attach(data)
#패키지 내의 describe() 함수를 사용하여 기술통계량을 구할 수있다.
#데이터 벡터의 기술통계량 구하기
describe(q3_1)
detach(data)
#dataframe을 넣으면 모든 열에 대한 기술통계량을 구할 수 있다.
describe(data2)
```
---
### 3. 사용자정의 함수를 이용하여 기술통계량 구하기

평균을 구하는 사용자 정의 함수
```{R}
mymean <- function(x){
  return(sum(x)/length(x))
}

mymean(data$q3_1)

```

표본 표준편차를 구하는 사용자 정의 함수
```{R}
mysd <- function(x){
  return(sqrt(sum((x-mymean(x))^2)/(length(x)-1)))
}
mysd(data$q3_1)
  
```
왜도를 구하는 사용자 정의 함수
```{R}
myskew<-function(x){
  return(sum(((x-mymean(x))/mysd(x))^3)/length(x))
}

myskew(data$q3_1)
```

첨도를 구하는 사용자 정의 함수
```{R}
mykurt<-function(x){
  return(sum(((x-mymean(x))/mysd(x))^4)/length(x)-3)
}
mykurt(data$q3_1)

```

### 4. 상관계수 구하기

상관계수 구하고 검정하기
```{R}
data(iris)
#산점도를 통해 데이터의 선형관계 확인하기
plot(iris[,1:4])
#Petal.Length 와 Petal.Width의 상관계수 구하기
attach(iris)
#상관계수 구하기
#피어슨
cor(Petal.Length,Petal.Width,method = c("pearson"))
#스피어만
cor(Petal.Length,Petal.Width,method = c("spearman"))
#캔달
cor(Petal.Length,Petal.Width,method = c("kendal"))
#상관 계수 검정
#피어슨 검정
cor.test(Petal.Length,Petal.Width,method = c("pearson"))
#스피어만  검정
cor.test(Petal.Length,Petal.Width,method = c("spearman"))
#캔달  검정
cor.test(Petal.Length,Petal.Width,method = c("kendal"))

#데이터 프레임에대해 상관계수 매트릭스 구하기
cor(iris[,1:4])
```


사용자 정의 함수를 사용하여 데이터 프레임 검정하기
```{R}
mycortest <- function(x){
n <- length(x)
temp <- matrix(0, nrow=n,ncol=n)
for (i in 1:n) {
  for (j in i:n) {
    temp[i,j]<-cor.test(x[,i],x[,j])$p.value
    temp[j,i]<-temp[i,j]
  }
}
colnames(temp)=names(x)
rownames(temp)=names(x)
return(temp)
}

```


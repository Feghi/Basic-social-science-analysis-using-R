

---
title: "6주차 코드"
author: "이철기"
date: '2020 4 20 '
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## SW 프로그래밍 집중교육 6주차

###  1. 데이터 입력
한국행정연구원 설문조사 데이터를 사용하여 분석

```{R}
#install.package("haven")
library(haven)
#SPSS 데이터 파일인 SAV 파일을 불러오기
temp <- read_sav("data.sav")
#데이터 프레임으로 데이터를 변경
data <- as.data.frame(temp)
```
### 2. 탐색적 요인 분석 하기 

'psych' package를 사용하여 탐색적 요인 분석을 할 수 있다.
```{R}
library(psych)
#배치전환, 보수 및 보상, 후생복지 부분 데이터 만들기
subdata3 = subset(data,select = q8_1:q10_3)  
#fa 함수를 이용해 요인 분석(ml : Maximul likelyhood_최대우도 기법 사용)
res=fa(subdata3, nfactors = 3, rotate="varimax",fm="ml")
print(res)

#'stat'package를 사용하여 탐색적 요인 분석을 할 수 있다.(default 값이 최대우도 기법 사용)
res2=factanal(subdata3,factors = 3, rotation="varimax")
print(res2)

```

### 3. 확인적 요인 분석 하기 
'lavaan' package를 사용하여 확인적 요인 분석을 할 수 있다.
```{R}
library(lavaan)

#업무 자율성, 후생복지 부분 모델 만들기
mymodel='autonomy=~q4_1+q4_2+q4_3
welfare=~q10_1+q10_2+q10_3'

#업무 자율성, 후생복지 요인에 대해 확인적 요인분석 하기
res=cfa(mymodel,data = data)
summary(res,fit.measures=T)

#업무 자율성, 후생복지 요인에 대해 성별별 확인적 요인분석 하기
res2=cfa(mymodel,data = data,group = "dq1")
summary(res2,fit.measures=T)

#요인 그래프 그리기
library(lavaanPlot)
lavaanPlot(model=res,coefs=T)
```

### 4. 주성분 분석 하기 
'stats' prcomp를 사용하여 확인적 요인 분석을 할 수 있다.

```{R}
#iris 데이터셋
data("iris")

#iris 데이터의 주성분 분석
res<-prcomp(iris[-5])
print(res)

#공분산 매트릭스의 주성분 분해를 통해 주성분 분석하기
c=cov(iris[-5])
res2<-eigen(c)
print(res2)

#2개의 주성분에 대해 ploting 하고 레이블별 색상 칠하기
plot(res$x[,1:2], pch=21, bg=c("red","green","blue")[unclass(iris$Species)])
```

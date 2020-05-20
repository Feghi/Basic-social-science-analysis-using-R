

---
title: "5주차 코드"
author: "이철기"
date: '2020 4 13 '
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## SW 프로그래밍 집중교육 5주차

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

---
### 2. 크론바흐 알파 구하기 
'psych' package를 사용하여 설문 데이터의 신뢰성을 분석 할 수 있다.
```{R}
#install.packages("psych")
library(psych)
attach(data)

#보수 및 보상 신뢰성 분석
subdata1=data.frame(q9_1,q9_2,q9_3,q9_4)
#패키지 내의 alpha() 함수를 사용하여 크론바흐 알파를 구할 수있다.
alpha(subdata1)

#후생복지 신뢰성 분석
subdata2=data.frame(q10_1,q10_2,q10_3)
alpha(subdata2)
```
---
### 3. 사용자정의 함수를 이용하여 크론바흐 알파 구하기

크론바흐 알파를 구하는 사용자 정의 함수
```{R}
myalpha=function(y){
k=length(y)
#동일 항목 점수의 합인 X 만들기
x=apply(y,1,sum)
x2=data.frame(y,x)
#각 열의; 분산 구하기
v=apply(x2,2,var)
output=(k/(k-1))*(1-((sum(v[-(k+1)]))/v[k+1]))
return(output)
}
myalpha(subdata1)
```
### 4. 요인 분석 하기 

'psych' package를 사용하여 요인 분석을 할 수 있다.
```{R}
#배치전환, 보수 및 보상, 후생복지 부분 데이터 만들기
subdata3 = subset(data,select = q8_1:q10_3)  
#fa 함수를 이용해 요인 분석
res=fa(subdata3, nfactors = 3, rotate="varimax")
print(res)
biplot(res)
```

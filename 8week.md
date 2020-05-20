

---
title: "8주차 코드"
author: "이철기"
date: '2020 5 04 '
output: html_document
runtime: shiny
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## SW 프로그래밍 집중교육 8주차

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
### 2. 구조방정식 모델링 

'lavaan' package를 사용하여 모델링
```{R}
library(lavaan)

#9.보수 및 보상(reward), 후생복지(welfare), 업무수행 역량, 교육 훈련, 조직성과 잠재변수
attach(data)
mymodel<-'reward=~q9_1+q9_2+q9_3+q9_4
welfare=~q10_1+q10_2+q10_3
capability=~q14_1+q14_2+q14_3
education=~q15_1+q15_2
performance=~q24_1+q24_2+q24_3
reward~~welfare
capability~~education
performance~reward+welfare+capability+education'

#구조방정식
library(lavaan)
mysem<-sem(mymodel,data=data)
summary(mysem,standardized=TRUE)

#fitMeasures 함수를 이용한 모델 적합도
fitMeasures(mysem)

#lavaanPlot 을 이용한 시각화
library(lavaanPlot)
lavaanPlot(model = mysem, coefs = TRUE)

#semPlot을 이용한 시각화
#install.packages("semPlot")
library(semPlot)
semPaths(mysem,whatLabels = "est")

```


### 3. 데이터 시각화 ggplot2
'ggplot2' package를 사용하여 그래프를 그릴 수 있다.
```{R}
#install.packages("ggplot2")
library(ggplot2)
starbucks <- read.csv2("starbucks.csv",sep = ',')
Decaffein<-as.factor(ifelse(starbucks$Caffeine==0,1,0))
starbucks2<-data.frame(starbucks,Decaffein)
#스타벅스 데이터 카테고리 빈도 막대 그래프(카페인별)
ggplot(data = starbucks2) + 
stat_count(mapping = aes(x = Category,fill=Decaffein))
```

```{R}
#스타벅스 데이터 카테고리 파이차트
ggplot(data = starbucks2) + 
stat_count(mapping = aes(x = Category,fill=Category)) +coord_polar("x")
```

```{R}
#iris 데이터 PCA 후에 Label별 점도표
mypca <- prcomp(iris[1:4])
df=data.frame(mypca$x,iris$Species)
ggplot(data = df)+
  geom_point(mapping = aes(x = PC1, y = PC2,color=iris.Species))

```


### 4. 데이터 시각화 shiny
타이타닉 데이터 빈도표 및 그래프 만들기
```{R}
#install.packages("shiny")
library(shiny)
# UI 정의
ui <- fluidPage(
   # 타이틀
   titlePanel("스타벅스 데이터 빈도 막대 그래프 및 파이차트"),
   # 사이드바 페널 
   sidebarPanel(
     selectInput("var_x", 
                 label = "데이터 선택", 
                 choices = list("Category"="Category", "Manu"="Manu", "Decaffein"="Decaffein"), 
                 selected = "Category"),
          checkboxInput(inputId = "freq_checked", label = strong("빈도표 출력"),value = TRUE)),
      # 메인 페널
      mainPanel(
        h4("막대 그래프"), 
        plotOutput("barPlot"), 
        h4("파이차트"), 
        plotOutput("piePlot"), 
        h4("빈도표"), 
        verbatimTextOutput("freq")
      )

   )

# server 정의

server <- function(input, output) {
  starbucks <- read.csv2("starbucks.csv",sep = ',')
  Decaffein<-as.factor(ifelse(starbucks$Caffeine==0,"Decaffein","Caffeine"))
  starbucks2<-data.frame(starbucks,Decaffein)
  starbucks3 <- starbucks2[,c("Category", "Manu", "Decaffein")]
  library(ggplot2)
  library(descr)
  
  # 막대그래프
  output$barPlot <- renderPlot({
    var_name_x <- as.character(input$var_x)
    
    ggplot(data = starbucks3) + 
    stat_count(mapping = aes(x = starbucks3[, input$var_x],fill=starbucks3[, input$var_x]))+ xlab(var_name_x)
    
    })
  # 파이차트
  output$piePlot <- renderPlot({
    var_name_x <- as.character(input$var_x)
    
    ggplot(data = starbucks3) + 
    stat_count(mapping = aes(x = starbucks3[, input$var_x],fill=starbucks3[, input$var_x])) + coord_polar("x") + xlab(var_name_x)
    
    })
  
  
  # 빈도표
  output$freq <- renderText({
    if(input$freq_checked){
    freq(starbucks3[, input$var_x])}
    })
}

# 실행
shinyApp(ui = ui, server = server)

```

스타벅스 데이터 회귀분석 및 상관분석 만들기
```{R}
#install.packages("shiny")
library(shiny)
# UI 정의
ui <- fluidPage(
   # 타이틀
   titlePanel("스타벅스 데이터 회귀분석&상관분석"),
   # 사이드바 페널 
   sidebarPanel(
     selectInput("var_x", 
                 label = "독립변수 선택", 
                 choices = list("Price"="Price", "Kcal"="Kcal", "Sugars"="Sugars", "Caffeine"="Caffeine"), 
                 selected = "Sugars"),
     selectInput("var_y", 
                 label = "종속변수 선택", 
                 choices = list("Price"="Price", "Kcal"="Kcal", "Sugars"="Sugars", "Caffeine"="Caffeine"), 
                 selected = "Kcal"), 
          checkboxInput(inputId = "corr_checked", label = strong("상관계수"),value = TRUE), 
          checkboxInput(inputId = "reg_checked", label = strong("단순회귀"),value = TRUE)),
      # 메인 페널
      mainPanel(
        h4("산점도"), 
        plotOutput("scatterPlot"), 
        h4("상관계수"), 
        verbatimTextOutput("corr_coef"), 
        h4("단순회귀"), 
        verbatimTextOutput("reg_fit")
      )

   )

# server 정의

server <- function(input, output) {
  starbucks <- read.csv2("starbucks.csv",sep = ',')
  scatter <- starbucks[,c("Price", "Kcal", "Sugars", "Caffeine")]

  # 산점도
  output$scatterPlot <- renderPlot({
    var_name_x <- as.character(input$var_x)
    var_name_y <- as.character(input$var_y)

    plot(scatter[, input$var_x],
         scatter[, input$var_y],
         xlab = var_name_x,
         ylab = var_name_y,
         main = "산점도 및 회귀곡선")
    # 그래프에 추세선 넣기
    fit <- lm(scatter[, input$var_y] ~ scatter[, input$var_x])
    abline(fit)
    })
  # 상관계수
  output$corr_coef <- renderText({
    if(input$corr_checked){
      cor(scatter[, input$var_x], 
          scatter[, input$var_y])
      }
    })
  # 단순회귀
  output$reg_fit <- renderPrint({
    if(input$reg_checked){
      fit <- lm(scatter[, input$var_y] ~ scatter[, input$var_x])
      names(fit$coefficients) <- c("Intercept", input$var_x)
      summary(fit)$coefficients
    }
  })
}

# 실행
shinyApp(ui = ui, server = server)

```


install.packages("KoNLP")
library(dplyr)
library(stringr)
library(RColorBrewer)
install.packages("readxl")
library(readxl)
require(devtools)
install_github("lchiffon/wordcloud2")
library(wordcloud2)
install.packages("readr")
library(readr)
library(KoNLP)
useNIADic()


#크롤링 데이터 불러오기

rawData <- read_csv("C://Users/user/Desktop/gangnamc2.csv")
dt <- as.data.frame(rawData)

#가중치 비율 구하기
df <- read.csv("C://Users/user/Desktop/gn1.csv", fileEncoding = "utf-8")
result <- data.frame(df)

df = head(df, n = 100)

numa = df["ITEM_QTY"][100,]
numb = round(df["ITEM_QTY"] / numa)

#크롤링 데이터와 가중치 비율 cbind
cr <- cbind(dt,numb)

#크롤링 데이터에 가중치 반영
list=NULL
for (i in 1:nrow(cr)){
  result=rep(cr$title[i],times=cr$ITEM_QTY[i])
  list=append(list,result)
}

# 특수문자, 띄어쓰기 등 삭제
test<-str_replace_all(list,"\\W"," ")
test<-gsub("\t","",test)
test<-gsub("\\d+","",test)
test<-gsub("\\n+","",test)
test<-gsub("[A-z]","",test)
test<-gsub("[ㄱ-ㅎ]","",test)
test<-gsub("[[:cntrl:]]","",test)
test<-gsub("[[:punct:]]", "", test)

# del.txt파일에 의미없는 단어를 불러와 제거
del <- readLines("del.txt", encoding = "cp949")
for (i in 1:length(del)){
  test<-gsub(del[i],"",test)
}

#형태소 분석
nouns<-extractNoun(test)

#추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount<-table(unlist(nouns))

# 데이터 프레임으로 변환
re_word <- as.data.frame(wordcount, stringsAsFactors = F)

#변수명 수정
re_word<-rename(re_word,word=Var1,freq=Freq)

#두 글자 이상 단어 추출
re_word<-filter(re_word,nchar(word)>=2)

#빈도수가 많은 순으로 500개만 
top_500<-re_word %>%
  arrange(desc(freq)) %>%
  head(500)

top_500

# 워드클라우드2를 사용한 시각화
wordcloud2(data=top_500, size = 1,  col="random-dark", rotateRatio= 2)
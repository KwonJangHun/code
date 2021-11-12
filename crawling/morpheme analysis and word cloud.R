library(KoNLP)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(wordcloud)

install.packages("readxl")
library(readxl)

test <- readLines("test.txt", encoding = "UTF-8")

useNIADic()
extractNoun(test)

test<-str_replace_all(txt,"\\W"," ")
test<-gsub("\\d+","",txt)
test<-gsub("\\n+","",txt)
test<-gsub("[A-z]","",txt)
test<-gsub("[[:cntrl:]]","",txt)
nouns<-extractNoun(test)

#추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount<-table(unlist(nouns))

# 데이터 프레임으로 변환
re_word <- as.data.frame(wordcount, stringsAsFactors = F)

#변수명 수정
re_word<-rename(re_word,word=Var1,freq=Freq)

#두 글자 이상 단어 추출
re_word<-filter(re_word,nchar(word)>=2)

#빈도수가 많은 순으로 40개만 
top_40<-re_word %>%
  arrange(desc(freq)) %>%
  head(40)

top_40

pal <- brewer.pal(8,"Dark2") #색깔지정
wordcloud(words = top_40$word,  # 단어
          freq = top_40$freq,   # 빈도
          min.freq = 2,          # 최소 단어 빈도
          max.words = 200,       # 표현 단어 수
          random.order = F,      # 고빈도 단어 중앙 배치
          rot.per = .1,          # 회전 단어 비율
          scale = c(4, 0.3),     # 단어 크기 범위
          colors = pal)
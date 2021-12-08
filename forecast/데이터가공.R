#sarima, lstm 분석용 데이터 가공
#데이터 불러오기
fc3=read.csv('C:/Users/user/Desktop/cj/fc3.csv',header=T, sep = ',', quote = "")
fc4=read.csv('C:/Users/user/Desktop/cj/fc4.csv',header=T, sep = ',', quote = "")
fc5=read.csv('C:/Users/user/Desktop/cj/fc5.csv',header=T, sep = ',', quote = "")
fc6=read.csv('C:/Users/user/Desktop/cj/fc6.csv',header=T, sep = ',', quote = "")

#데이터 합치기
fc=rbind(fc3,fc4,fc5,fc6)

#서울의 어느구가 가장 판매량이 많냐.
seoul=subset(fc,BKG_TYP==7&(CNEE_ADDR_1=="서울"|
                            CNEE_ADDR_1=="서울시"|
                            CNEE_ADDR_1=="서울특별시"|
                            CNEE_ADDR_1=="울특별시"),select=c(BKG_DATE,ITEM_QTY,CNEE_ADDR_2,ITEM_CD))

library(dplyr)
s1=aggregate(ITEM_QTY~CNEE_ADDR_2,seoul,sum)%>%arrange(desc(ITEM_QTY))
s1

#강남,송파,강서가 top3

# 해당구만.
songpa=subset(seoul, CNEE_ADDR_2=="강남구")


#시간형 데이터로 변환
library(lubridate)
songpa$date=ymd(songpa$BKG_DATE)

#상품코드 가공(-,_이 들어있으면 그 전까지의 숫자들까지만 살리기.)
library(stringr)
songpa$cd=gsub("-","_",songpa$ITEM_CD)
songpa$cd
v1=str_split_fixed(songpa$cd,pattern='_',2)
v1
v1[,1]
songpa$cd=v1[,1]

#전처리 과정에서 생긴 불필요한 데이터 삭제.
songpa=subset(songpa,select=c(date,cd,ITEM_QTY))


#가장 많이 팔린 품목들이 뭐고, 그 갯수들을 많이 팔린 순으로 보기
songpa_rank=aggregate(ITEM_QTY~cd,songpa,sum)%>%arrange(desc(ITEM_QTY))
songpa_rank

#품목코드 별로 날짜 당 몇개 팔렸는지 피벗테이블화 한 후 합침.
#데이터 행의 수가 100개 이하면 분석에 어려움이 있으므로 제외.

songpa_series=NULL
i=0
while(i<=300){
  i=i+1
  result=subset(songpa,cd==songpa_rank[i,1])
  result1=aggregate(ITEM_QTY~date,result,sum)
  result1$cd=rep(songpa_rank[i,1],times=nrow(result1))
  if(nrow(result1)<100){
    next
  }
  songpa_series=rbind(songpa_series,result1)
}

head(songpa_series)

write.csv(songpa_series,'C:/Users/user/Desktop/cj/songpa_series.csv',row.names=F)


#워드클라우드용 데이터 가공
#자치구별 상품별 빈도파악. 상위 n%만 분석에 넣음.

# 해당구만.
songpa=subset(seoul, CNEE_ADDR_2=="송파구")

#상품코드 가공(-,_이 들어있으면 그 전까지의 숫자들까지만 살리기.)
library(stringr)
songpa$cd=gsub("-","_",songpa$ITEM_CD)
songpa$cd
v1=str_split_fixed(songpa$cd,pattern='_',2)
v1
v1[,1]
songpa$cd=v1[,1]


sp1=aggregate(ITEM_QTY~cd,songpa,sum)%>%arrange(desc(ITEM_QTY))

write.csv(sp1,'C:/Users/user/Desktop/cj/sp1.csv',row.names=F)



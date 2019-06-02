air16<-read.csv("fineDust/airpollution_2016.csv")
dis16<-read.csv("disorder/disorder2016_f.csv")

#대기오염 데이터에서 서울 지역 데이터만 추출
library(dplyr)
air16_seoul<-air16%>%
  filter(area=="서울")

#min-max정규화
normalize<-function(x){
  res<-(x-min(x))/(max(x)-min(x))
  return(res)
}
air16_seoul_pm25_nor<-normalize(air16_seoul[8])
dis16_patient_nor<-normalize(dis16[2])

#미세먼지 수치와 공황장애 환자수 간의 상관관계 분석
cor(air16_seoul_pm25_nor$PM2.5,dis16_patient_nor$x) #-0.06482165

#상관관계 가시화
plot(air16_seoul_pm25_nor$PM2.5,dis16_patient_nor$x)


#추후 이상치 제거 필요
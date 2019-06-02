air16<-read.csv("air_pollution/airpollution_2016.csv")
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



#date 문자열로 변경
dis16_df_nor <- data.frame(dis16[1],dis16_patient_nor)
dis16_df_nor$Group.1<-as.character(dis16_df_nor$Group.1)
str(dis16_df_nor)

#이상치 확인
bp<-boxplot(dis16_df_nor$x)
bp$stats

# 1. 이상치를 평균값으로 대체
dis16_df_nor$x<-ifelse(dis16_df_nor$x>0.7,NA,dis16_df_nor$x)
dis16_df_nor$x<-ifelse(dis16_df_nor$x<0.13,NA,dis16_df_nor$x)
dis16_df_nor$x<-ifelse(is.na(dis16_df_nor$x),mean(dis16_df_nor$x,na.rm = T),dis16_df_nor$x)

# 2. 이상치를 제거
dis16_df_nor$x<-ifelse(dis16_df_nor$x>0.7,NA,dis16_df_nor$x)
dis16_df_nor$x<-ifelse(dis16_df_nor$x<0.13,NA,dis16_df_nor$x)
total_df<-cbind(air16_seoul_pm25_nor,dis16_df_nor)
total_df<-na.omit(total_df)

library(ggplot2)
ggplot(data=dis16_df_nor, aes(x=Group.1,y=x)) +
  geom_point()

# 1. 이상치 평균 대체
#다시 미세먼지 수치와 공황장애 환자수 간의 상관관계 분석
cor(air16_seoul_pm25_nor$PM2.5,dis16_df_nor$x) 

#상관관계 가시화
plot(air16_seoul_pm25_nor$PM2.5,dis16_df_nor$x)


# 2. 이상치 제거
cor(total_df$PM2.5,total_df$x) 
plot(total_df$PM2.5,total_df$x)


#분기별 대기오염 데이터 불러오기
w_2016_1<-read.csv("C:/Users/Junghyun Lee/Desktop/기상데이터 분석 프로젝트/2016/2016년 1분기.csv", header=T)
w_2016_2<-read.csv("C:/Users/Junghyun Lee/Desktop/기상데이터 분석 프로젝트/2016/2016년 2분기.csv", header=T)
w_2016_3<-read.csv("C:/Users/Junghyun Lee/Desktop/기상데이터 분석 프로젝트/2016/2016년 3분기.csv", header=T)
w_2016_4<-read.csv("C:/Users/Junghyun Lee/Desktop/기상데이터 분석 프로젝트/2016/2016년 4분기.csv", header=T)
w_2016 <- rbind(w_2016_1,w_2016_2)
w_2016 <- rbind(w_2016,w_2016_3)
w_2016 <- rbind(w_2016,w_2016_4)

#결측치 평균값 처리
w_2016$SO2<-ifelse(is.na(w_2016$SO2),mean(w_2016$SO2,na.rm=T),w_2016$SO2)
w_2016$CO<-ifelse(is.na(w_2016$CO),mean(w_2016$CO,na.rm=T),w_2016$CO)
w_2016$O3<-ifelse(is.na(w_2016$O3),mean(w_2016$O3,na.rm=T),w_2016$O3)
w_2016$NO2<-ifelse(is.na(w_2016$NO2),mean(w_2016$NO2,na.rm=T),w_2016$NO2)
w_2016$PM10<-ifelse(is.na(w_2016$PM10),mean(w_2016$PM10,na.rm=T),w_2016$PM10)
w_2016$PM25<-ifelse(is.na(w_2016$PM25),mean(w_2016$PM25,na.rm=T),w_2016$PM25)

#지역과 측정일시로 그룹화 및 평균 summarise
library(dplyr)
w_total<-w_2016%>%
  group_by(area=substr(지역,1,2),day=substr(측정일시,1,8))%>%
  summarise(SO2=mean(SO2),CO=mean(CO),O3=mean(O3),NO2=mean(NO2),PM10=mean(PM10),PM2.5=mean(PM25))
View(w_total)

#서울의 미세먼지(pm2.5) 시각화
library(ggplot2)
ggplot(data=(w_total%>%filter(area=="서울")), aes(x=day,y=PM10,group=1)) +
  geom_line(aes(color=area))

#csv파일로 저장
write.csv(w_total,"c/Users/Junghyun Lee/Documents/Rproject/Rproject/airpollution_2016.csv",row.names = F)

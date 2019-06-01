w_2016_1<-read.csv("C:/Users/Junghyun Lee/Desktop/기상데이터 분석 프로젝트/2016/2016년 1분기.csv", header=T)
w_2016_2<-read.csv("C:/Users/Junghyun Lee/Desktop/기상데이터 분석 프로젝트/2016/2016년 2분기.csv", header=T)
w_2016_3<-read.csv("C:/Users/Junghyun Lee/Desktop/기상데이터 분석 프로젝트/2016/2016년 3분기.csv", header=T)
w_2016_4<-read.csv("C:/Users/Junghyun Lee/Desktop/기상데이터 분석 프로젝트/2016/2016년 4분기.csv", header=T)
w_2016 <- rbind(w_2016_1,w_2016_2)
w_2016 <- rbind(w_2016,w_2016_3)
w_2016 <- rbind(w_2016,w_2016_4)


w_2016_1<-read.csv("./2015/2015년1분기.csv",header=T)
w_2016_2<-read.csv("./2015/2015년2분기.csv",header=T)
w_2016_3<-read.csv("./2015/2015년3분기.csv",header=T)
w_2016_4<-read.csv("./2015/2015년4분기.csv",header=T)




str(w_2016_2)
head(w_2016,30)
count<-0
start <-1
substr(w_2016[1,11],1,8)
mean(w_2016$SO2[1:24])
sum(is.na(w_2016))
w_2016[24,1]

mean(w_2016$SO2,na.rm=T)
mean(w_2016$CO,na.rm=T)
mean(w_2016$O3,na.rm=T)
mean(w_2016$NO2,na.rm=T)
mean(w_2016$PM10,na.rm=T)
mean(w_2016$PM25,na.rm=T)
w_2016$SO2<-ifelse(is.na(w_2016$SO2),mean(w_2016$SO2,na.rm=T),w_2016$SO2)
w_2016$CO<-ifelse(is.na(w_2016$CO),mean(w_2016$CO,na.rm=T),w_2016$CO)
w_2016$O3<-ifelse(is.na(w_2016$O3),mean(w_2016$O3,na.rm=T),w_2016$O3)
w_2016$NO2<-ifelse(is.na(w_2016$NO2),mean(w_2016$NO2,na.rm=T),w_2016$NO2)
w_2016$PM10<-ifelse(is.na(w_2016$PM10),mean(w_2016$PM10,na.rm=T),w_2016$PM10)
w_2016$PM25<-ifelse(is.na(w_2016$PM25),mean(w_2016$PM25,na.rm=T),w_2016$PM25)
sum(is.na(w_2016))
rbind(w_mean, data.frame(w_2016[24,1],w_2016[24,2],w_2016[24,3],substr(w_2016[24,4],1,8),mean(w_2016$SO2[1:24]),
                         mean(w_2016$CO[1:24]),mean(w_2016$O3[1:24]),mean(w_2016$NO2[1:24]),mean(w_2016$PM10[1:24]),
                         mean(w_2016$PM25[1:24]),w_2016[24,11]))
w_r<-data.frame(지역=w_2016[24,1],w_2016[24,2],w_2016[24,3],substr(w_2016[24,4],1,8),mean(w_2016$SO2[1:24]),
                  mean(w_2016$CO[1:24]),mean(w_2016$O3[1:24]),mean(w_2016$NO2[1:24]),mean(w_2016$PM10[1:24]),
                  mean(w_2016$PM25[1:24]),w_2016[24,11])
w_mean<-data.frame()
for(i in 1:nrow(w_2016)){
  count<-count+1
  #cat(count,"\n")
  if(count==24){
    w_mean<-rbind(w_mean,data.frame(area=w_2016[i,1],code=w_2016[i,2],measurement_name=w_2016[i,3],day=substr(w_2016[i,4],1,8),SO2=mean(w_2016$SO2[start:i]),
                                    CO=mean(w_2016$CO[start:i]),O3=mean(w_2016$O3[start:i]),NO2=mean(w_2016$NO2[start:i]),PM10=mean(w_2016$PM10[start:i]),
                                    PM25=mean(w_2016$PM25[start:i]),주소=w_2016[i,11]))
    start<-i+1
    count<-0
  }
}
w_mean
for(i in 1:nrow(w_2016)){
  count<-count+1
  cat(count,"\n")
  if(count==24){
    count<-1
  }
}
View(w_2016_2)
View(w_mean)
View(w_total)
plot(head(w_mean,100))
plot(head(w_mean,100), x=w_mean$CO, y=w_mean$day, type="l")
library(ggplot2)
w_total%>%
  filter(area=="서울")

ggplot(data=(w_total%>%filter(area=="서울")), aes(x=day,y=PM10,group=1)) +
  geom_line(aes(color=area))
ggplot(data=(w_total), aes(x=day,y=PM10,group=1)) +
  geom_line(aes(color=area),size=10)
w_total <- data.frame()
substr(w_mean$day,1,6)
substr(w_mean$area,1,2)
sum_CO
library(dplyr)
w_total<-w_mean%>%
  group_by(day)%>%
  summarise(SO2=mean(SO2),CO=mean(CO),O3=mean(O3),NO2=mean(NO2),PM10=mean(PM10),PM2.5=mean(PM25))
View(w_total)

w_total_1<-w_mean%>%
  group_by(area=substr(area,1,2),day=substr(day,1,6))%>%
  summarise(SO2=mean(SO2),CO=mean(CO),O3=mean(O3),NO2=mean(NO2),PM10=mean(PM10),PM2.5=mean(PM25))

####최종 공식
w_total<-w_2016%>%
  group_by(area=substr(지역,1,2),day=substr(측정일시,1,8))%>%
  summarise(SO2=mean(SO2),CO=mean(CO),O3=mean(O3),NO2=mean(NO2),PM10=mean(PM10),PM2.5=mean(PM25))

w_total_mean<-w_2016%>%
  group_by(day=substr(측정일시,1,8))%>%
  summarise(SO2=mean(SO2),CO=mean(CO),O3=mean(O3),NO2=mean(NO2),PM10=mean(PM10),PM2.5=mean(PM25))



w_total<-rbind(w_total,w_total_4)
write.csv(w_total,file="airkorea_2018.csv",row.names = F)
write.csv(w_total_mean,file="airpollution_2016.csv",row.names = F)

View(w_total_mean)
getwd()
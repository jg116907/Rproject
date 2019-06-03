rm(list = ls())
getwd()
setwd("C:\\Users\\pulum\\Desktop\\programming\\R\\Rproject")
ad1618 <- read.csv('ad1618.csv', stringsAsFactors = F)
#ad1618[1,2]
#ad1618 <- ad1618[-6,]
#ad1618 <- ad1618[-8,]
#str(ad1618)

#인구수 추출
ad1618 <- ad1618[-c(1:2),c(seq(3, ncol(ad1618), 5))]

# 공백 제거
ad1618 <- ad1618[-c(18:25),]

ad1618 <- as.data.frame(ad1618)

# 이름
rownames(ad1618) <- c("서울", "부산", "인천", "대구", "광주", "대전", "울산", "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남", "제주", "세종")

#년도
da <- vector()
k <- 1
for (i in 16:18) {
  for (j in 01:12) {
    da[k] <- paste0("20",i,"년",j,"월")
    k <- k + 1
  }
}
ncol(ad1618)
colnames(ad1618) <- da[-c(35:36)]



ad2016 <- ad1618[ ,1:12]
ad2017 <- ad1618[ ,13:24]
ad2018 <- ad1618[ ,25:34]

tad2016 <- t(ad2016)

library(ggplot2)
#ggplot(aes())

library(dplyr)
ad1618%>%
  filter(rownames(ad1618)=="서울")%>%
  filter(colnames(ad1618)==colnames(ad2016_ex$년월))
ad2016_ex <- data.frame("년월" = colnames(ad1618), "지역" = c("서울"), "환자수" = ad1618%>%filter(rownames(ad1618)=="서울"))
View(ad2016_ex)


##############################
seoul_pm <- read.csv("seoul_pm.csv", stringsAsFactors = F)
seoul_pa <- read.csv("seoul_patient.csv", stringsAsFactors = F)
seoul_pma <- data.frame(seoul_pa, pm2.5 = seoul_pm[,3])

seould_pma <-na.omit(seoul_pma)

normal <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}

seoul_pma$환자수<-as.character(seoul_pma$환자수)
seoul_pma$환자수<-gsub(",","",seoul_pma$환자수)
seoul_pma$환자수<-as.numeric(seoul_pma$환자수)
str(seoul_pma)

seoul_pma_norm <- as.data.frame(lapply(seoul_pma[3:4], normal))
plot(seoul_pma_norm)

ggplot(aes(seoul_pma$년월)) + 
  geom_line(color = seoul_pma$환자수,seoul_pma$pm2.5)

###############################
library(ggplot2)
pd2016 <- read.csv("disorder2016_f.csv")
pd2015 <- read.csv("disorder2015_f.csv")


str(pd2016)

names(pd2016) <- c("Day","Population")
names(pd2015) <- c("Day","Population")

pd2016$Day <- as.character(pd2016$Day)
pd2015$Day <- as.character(pd2015$Day)

ggplot(pd2016, aes(x = Day, y = Population, group = 1)) + geom_line(linetype = 1, lwd = 0.8, color = 'red') + 
  ggtitle("2016, Mental disorder Population") + 
  theme(plot.title = element_text(size = 17))


ggplot(pd2015, aes(x = Day, y = Population, group = 1)) + geom_line(linetype = 1, lwd = 0.8, color = 'red') + 
  ggtitle("2015, Mental disorder Population") + 
  theme(plot.title = element_text(size = 17))

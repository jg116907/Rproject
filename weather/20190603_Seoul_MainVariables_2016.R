#1. 서울지역 주요 변수 추출
wt16_Seoul <- read.csv("2016서울기상(일별)(추출ver).csv", header = T, sep = ",")
wt16_Seoul
colMeans(wt16_Seoul[, c(3:17)]) #서울지역 각 변수들의 1년 전체 평균 출력

wt16_Seoul[, c(1, 2, 3, 6, 8, 12, 13, 15)]   
#컬럼명 정리
#1: day(일시), 2: location(지역코드), 3: mean_temp(평균기온), 6: daily_rain(일가수량), 
#8: mean_humid(평균 상대습도), 12: amt_sun(일사량), 13: max_snow(최심신적설), 15: mean_cloud(평균 운량)

#2. 서울지역 일교차 출력, 시각화 
wt16_Seoul
temp_diff_Seoul <- c(wt16_Seoul$high_temp - wt16_Seoul$low_temp)  #(최고기온 - 최저기온)
temp_diff_Seoul <- as.data.frame(temp_diff_Seoul)                 #데이터프레임으로 형변환
temp_diff_Seoul <- cbind(wt16_Seoul$day, wt16_Seoul$location, temp_diff_Seoul)       #변수 따로 추출 및 새 변수 생성: 일시(day), 지역(location), 서울 일교차
colnames(temp_diff_Seoul) <- c("day", "location", "temp_diff_Seoul") #열 이름 변경
temp_diff_Seoul                                                      #날짜별 일교차 출력

ggplot(temp_diff_Seoul, aes(x = day, y = temp_diff_Seoul, group = 1)) + geom_line(linetype = 1, lwd = 0.8, color = 'red') + # 시각화 
  ggtitle("2016년 서울지역 일교차 추이(℃)")+
  theme(plot.title = element_text(size = 17))
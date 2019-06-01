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
pd2016 <- read.csv("disorder2016_f.csv")
pd2015 <- read.csv("disorder2015_f.csv")


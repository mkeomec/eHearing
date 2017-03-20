setwd("~/Github/eHearing")
raw.data <- read.csv('HHT_rawdata.csv')

cor.test(raw.data$RAudio_PTA,raw.data$RHHT_PTA)
cor.test(raw.data$LAudio_PTA,raw.data$LHHT_PTA)


r.regline <- lm(RAudio_PTA~RHHT_PTA, data=raw.data)
summary(r.regline)
l.regline <- lm(LAudio_PTA~LHHT_PTA, data=raw.data)
summary(l.regline)
plot(raw.data$RAudio_PTA,raw.data$RHHT_PTA)
abline(r.regline)
plot(raw.data$LAudio_PTA,raw.data$LHHT_PTA)
abline(l.regline)

raw.data$HHT_HL_Degree==1
strat1 <-raw.data[raw.data$HHT_HL_Degree==1,] 
strat2 <- raw.data[raw.data$HHT_HL_Degree==2,]
strat3 <- raw.data[raw.data$HHT_HL_Degree==3,]
strat4 <- raw.data[raw.data$HHT_HL_Degree==4,]

cor.test(strat1$RAudio_PTA,strat1$RHHT_PTA)
cor.test(strat1$LAudio_PTA,strat1$LHHT_PTA)

cor.test(strat2$RAudio_PTA,strat2$RHHT_PTA)
cor.test(strat2$LAudio_PTA,strat2$LHHT_PTA)

cor.test(strat3$RAudio_PTA,strat3$RHHT_PTA)
cor.test(strat3$LAudio_PTA,strat3$LHHT_PTA)

cor.test(strat4$RAudio_PTA,strat4$RHHT_PTA)
cor.test(strat4$LAudio_PTA,strat4$LHHT_PTA)

raw.data[,c('HL_DegreeStd','HHT_HL_Degree')]
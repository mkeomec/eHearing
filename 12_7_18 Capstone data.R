library(gridExtra)
library(plyr)
library("dplyr")
library(ggplot2)
library(scales)


data<- read.csv("Documents/Capstone/AF_US_UNI_AllSubsSurvey+Audio_2.20.18.csv", header=T, na.strings=c("#NULL!",'90NR'))

data_prep <- function(data){
  
  #If thresholds are not numeric convert to numeric.
  
  
  #data$L500<-as.numeric(as.character(data[,'L500']))
  #data$L1000<-as.numeric(as.character(data[,'L1000']))
  #data$L2000<-as.numeric(as.character(data[,'L2000']))
  #data$L4000<-as.numeric(as.character(data[,'L4000']))
  #data$R500<-as.numeric(as.character(data[,'R500']))
  #data$R1000<-as.numeric(as.character(data[,'R1000']))
  #data$R2000<-as.numeric(as.character(data[,'R2000']))
  #data$R400<-as.numeric(as.character(data[,'R4000']))

  if(class(data$L1000) != 'integer'){data[,c('L500','L1000','L2000','L4000','R500','R1000','R2000','R4000')] <- as.numeric(as.character(data[,c('L500','L1000','L2000','L4000','R500','R1000','R2000','R4000')]))}
  
  
  # Remove participants under age 60.
  data <- data[!(data$q0003==1),]
  
  #issue with 90NR --> 95 change: the HHT doesn't measure other thresholds if 90NR at 1000Hz (first threshold tested). Should we set all thresholds to 95 or leave them missing? ---> SOLUTION: SET ALL >90 to 95
  
  #Changing 90NR (aka 9999) and Conventional audio thresholds that are > 90 to 95
  #Left Ear
  
  data$L5<-data$L500
  data$L5[data$L500 > 90] <- 95
  data$L1<-data$L1000
  data$L1[data$L1000 > 90] <- 95
  data$L2<-data$L2000
  data$L2[data$L2000 > 90] <- 95
  data$L4<-data$L4000
  data$L4[data$L4000 > 90] <- 95
  
  
  #Right Ear
  data$R5<-data$R500
  data$R5[data$R500 > 90] <- 95
  data$R1<-data$R1000
  data$R1[data$R1000 > 90] <- 95
  data$R2<-data$R2000
  data$R2[data$R2000 > 90] <- 95
  data$R4<-data$R4000
  data$R4[data$R4000 > 90] <- 95
  
  #At 8000Hz
  data$L8<-data$L8000
  data$L8[data$L8000 > 90] <- 95
  data$R8<-data$R8000
  data$R8[data$R8000 > 90] <- 95
  
  # Calculating PTA in each ear
  
  #colnames(data[,1000:1021])
  data$LPTA <- apply(data[,c('L5','L1','L2','L4')], 1, mean, na.rm=T)
  data$RPTA <- apply(data[,c('R5','R1','R2','R4')], 1, mean, na.rm=T)
  
  
  data$LPTA[is.nan(data$LPTA)]  <- NA
  data$RPTA[is.nan(data$RPTA)]  <- NA
  
  #Now can calculate BEPTA (PTA in better ear)
  data$BEPTA<- ifelse((!is.na(data$RPTA) & data$RPTA <= data$LPTA) | is.na(data$LPTA),
                      data$RPTA, data$LPTA)
  
  data$hcat<- cut(data$BEPTA,
                  breaks=c(-Inf, 25, 40, 55, Inf),
                  labels=c("1", "2", "3","4"))
  
  # Convert hcat into an ordered factor... So for the lables: 1= below 25, 2 is from 25-40, #3 = 40-55, # 55 and greater?
  data$hcat <- factor(data$hcat,levels=c(1:4),ordered=TRUE)
  
  return(data)
}

US_data <- data_prep(data)

#US_data$hcat is the levels of HL. The table fun. shows who from each HL cat. picked which question in the survey.

AUS_data<- read.csv("Documents/Capstone/AF_US_UNI_AllSubsSurvey+Audio_2.20.18.csv", header=T, na.strings=c("#NULL!",'90NR'))

CAN_data<- read.csv("Documents/Capstone/AF_CAN_InPerson_AllSubs_Survey+Audio_CLEAN_07.25.2018.csv", header=T, na.strings=c("#NULL!",'90NR'))

UK_data<- read.csv("Documents/Capstone/AF_UK_InPerson_AllSurvey+AllAudios_CLEAN_7.26.18.csv", header=T, na.strings=c("#NULL!",'90NR'))

US_data<- read.csv("Documents/Capstone/AF_US_UNI_AllSubsSurvey+Audio_2.20.18.csv", header=T, na.strings=c("#NULL!",'90NR'))

UK_data <- data_prep(UK_data)
CAN_data <- data_prep(CAN_data)
AUS_data <- data_prep(AUS_data)
US_data <- data_prep(US_data)

# Run regression model seclection on all countries data
# Combine all country's datasets 
AUS_data$country <- 'AUS'
US_data$country <- 'US'
UK_data$country <- 'UK'
CAN_data$country <- 'CAN'


global_data <- rbind.fill(AUS_data,CAN_data)
global_data <- rbind.fill(global_data,UK_data)
global_data <- rbind.fill(global_data,US_data)




ifelse(global_data$q0054=="#NULL!",ifelse(global_data$q0104=="#NULL!",global_data$provider <- global_data$q0157,global_data$provider <- global_data$q0104),global_data$provider <- global_data$q0054)

#Code for summing tables together in the case of comparing answers across the  questions that appear again on separate pathways
global_data$provider <- ifelse(is.na(global_data$q0054),ifelse(is.na(global_data$q0104),global_data$q0157,global_data$q0104),global_data$q0054)
global_data$locationcon <- ifelse(is.na(global_data$q0056_0001),ifelse(is.na(global_data$q0108_0001),global_data$q0161_0001,global_data$q0108_0001),global_data$q0056_0001)
# 89 and 90 looking at preferences for types of providers and how important location convenience is

#94-132 5 ways reasons that encourage OBJ. Sum that tables together across the different HL cats. Since there are multiple choices they can select, will repeat the same code for each question and generate multiple tables. 
global_data$hearbetter <- ifelse(is.na(global_data$q0079_0001),ifelse(is.na(global_data$q0133_0001),global_data$q0186_0001,global_data$q0133_0001),global_data$q0079_0001)
#assigns NAs to 0 value
global_data$hearbetter[is.na(global_data$hearbetter)] <- 0

global_data$active <- ifelse(is.na(global_data$q0079_0002),ifelse(is.na(global_data$q0133_0002),global_data$q0186_0002,global_data$q0133_0002),global_data$q0079_0002)
global_data$active[is.na(global_data$active)] <- 0

global_data$hope <- ifelse(is.na(global_data$q0079_0003),ifelse(is.na(global_data$q0133_0003),global_data$q0186_0003,global_data$q0133_0003),global_data$q0079_0003)
global_data$hope[is.na(global_data$hope)] <- 0

global_data$friends <- ifelse(is.na(global_data$q0079_0004),ifelse(is.na(global_data$q0133_0004),global_data$q0186_0004,global_data$q0133_0004),global_data$q0079_0004)
global_data$friends[is.na(global_data$friends)] <- 0

global_data$trust <- ifelse(is.na(global_data$q0079_0005),ifelse(is.na(global_data$q0133_0005),global_data$q0186_0005,global_data$q0133_0005),global_data$q0079_0005)
global_data$trust[is.na(global_data$trust)] <- 0

global_data$cost <- ifelse(is.na(global_data$q0079_0006),ifelse(is.na(global_data$q0133_0006),global_data$q0186_0006,global_data$q0133_0006),global_data$q0079_0006)
global_data$cost[is.na(global_data$cost)] <- 0

global_data$worth <- ifelse(is.na(global_data$q0079_0007),ifelse(is.na(global_data$q0133_0007),global_data$q0186_0007,global_data$q0133_0007),global_data$q0079_0007)
global_data$worth[is.na(global_data$worth)] <- 0

global_data$stigma <- ifelse(is.na(global_data$q0079_0008),ifelse(is.na(global_data$q0133_0008),global_data$q0186_0008,global_data$q0133_0008),global_data$q0079_0008)
global_data$stigma[is.na(global_data$stigma)] <- 0

global_data$access <- ifelse(is.na(global_data$q0079_0009),ifelse(is.na(global_data$q0133_0009),global_data$q0186_0009,global_data$q0133_0009),global_data$q0079_0009)
global_data$access[is.na(global_data$access)] <- 0

global_data$age <- ifelse(is.na(global_data$q0079_0010),ifelse(is.na(global_data$q0133_0010),global_data$q0186_0010,global_data$q0133_0010),global_data$q0079_0010)
global_data$age[is.na(global_data$age)] <- 0

global_data$rec <- ifelse(is.na(global_data$q0079_0011),ifelse(is.na(global_data$q0133_0011),global_data$q0186_0011,global_data$q0133_0011),global_data$q0079_0011)
global_data$rec[is.na(global_data$rec)] <- 0

global_data$work <- ifelse(is.na(global_data$q0079_0012),ifelse(is.na(global_data$q0133_0012),global_data$q0186_0012,global_data$q0133_0012),global_data$q0079_0012)
global_data$work[is.na(global_data$work)] <- 0

global_data$style <- ifelse(is.na(global_data$q0079_0013),ifelse(is.na(global_data$q0133_0013),global_data$q0186_0013,global_data$q0133_0013),global_data$q0079_0013)
global_data$style[is.na(global_data$style)] <- 0

#135-169 5 reasons to discourage you
global_data$dhealth <- ifelse(is.na(global_data$q0080_0001),ifelse(is.na(global_data$q0134_0001),global_data$q0187_0001,global_data$q0134_0001),global_data$q0080_0001)
global_data$dhealth[is.na(global_data$dhealth)] <- 0

global_data$dcost <- ifelse(is.na(global_data$q0080_0002),ifelse(is.na(global_data$q0134_0002),global_data$q0187_0002,global_data$q0134_0002),global_data$q0080_0002)
global_data$dcost[is.na(global_data$dcost)] <- 0

global_data$dworth <- ifelse(is.na(global_data$q0080_0003),ifelse(is.na(global_data$q0134_0003),global_data$q0187_0003,global_data$q0134_0003),global_data$q0080_0003)
global_data$dworth[is.na(global_data$dworth)] <- 0

global_data$dhope <- ifelse(is.na(global_data$q0080_0004),ifelse(is.na(global_data$q0134_0004),global_data$q0187_0004,global_data$q0134_0004),global_data$q0080_0004)
global_data$dhope[is.na(global_data$dhope)] <- 0

global_data$dtrust <- ifelse(is.na(global_data$q0080_0005),ifelse(is.na(global_data$q0134_0005),global_data$q0187_0005,global_data$q0134_0005),global_data$q0080_0005)
global_data$dtrust[is.na(global_data$dtrust)] <- 0

global_data$dold <- ifelse(is.na(global_data$q0080_0006),ifelse(is.na(global_data$q0134_0006),global_data$q0187_0006,global_data$q0134_0006),global_data$q0080_0006)
global_data$dold[is.na(global_data$dold)] <- 0

global_data$dHAold <- ifelse(is.na(global_data$q0080_0007),ifelse(is.na(global_data$q0134_0007),global_data$q0187_0007,global_data$q0134_0007),global_data$q0080_0007)
global_data$dHAold[is.na(global_data$dHAold)] <- 0

global_data$dstigma <- ifelse(is.na(global_data$q0080_0008),ifelse(is.na(global_data$q0134_0008),global_data$q0187_0008,global_data$q0134_0008),global_data$q0080_0008)
global_data$dstigma[is.na(global_data$dstigma)] <- 0

global_data$dtime <- ifelse(is.na(global_data$q0080_0009),ifelse(is.na(global_data$q0134_0009),global_data$q0187_0009,global_data$q0134_0009),global_data$q0080_0009)
global_data$dtime[is.na(global_data$dtime)] <- 0

global_data$dbad <- ifelse(is.na(global_data$q0080_0010),ifelse(is.na(global_data$q0134_0010),global_data$q0187_0010,global_data$q0134_0010),global_data$q0080_0010)
global_data$dbad[is.na(global_data$dbad)] <- 0

global_data$dpressure <- ifelse(is.na(global_data$q0080_0011),ifelse(is.na(global_data$q0134_0011),global_data$q0187_0011,global_data$q0134_0011),global_data$q0080_0011)
global_data$dpressure[is.na(global_data$dpressure)] <- 0

US_data$didk <- ifelse(is.na(US_data$q0080_0012),ifelse(is.na(US_data$q0134_0012),US_data$q0187_0012,US_data$q0134_0012),US_data$q0080_0012)
US_data$didk[is.na(US_data$didk)] <- 0

#looking at q0041: Where would you prefer to get information about hearing health care? Check all that apply. Making it so we pick up yes/no responses 
global_data$Q41_1 <- global_data$q0041_0001
global_data$Q41_1[is.na(global_data$Q41_1)] <- 0
#Making Labels for Q41_1 table
tableQ41_1 <- table(global_data$Q41_1,global_data$hcat)
tableQ41_1 <- as.matrix(tableQ41_1)
row.names(tableQ41_1)<-c('Did Not Select','Selected')
colnames(tableQ41_1) <-c('Normal','Mild','Moderate','Severe')
#BarPlots
barplot(tableQ41_1, main="Doctor",
        xlab="Categories of Hearing Loss", col=c("red","blue"),
        ylim=c(0,1000),
        ylab = "Number of Participants",
        legend = rownames(tableQ41_14), beside=TRUE)

global_data$Q41_2 <- global_data$q0041_0002
global_data$Q41_2[is.na(global_data$Q41_2)] <- 0
#Making Labels for Q41_2 table
tableQ41_2 <- table(global_data$Q41_2,global_data$hcat)
tableQ41_2 <- as.matrix(tableQ41_2)
row.names(tableQ41_2)<-c('Did Not Select','Selected')
colnames(tableQ41_2) <-c('Normal','Mild','Moderate','Severe')
#BarPlots
barplot(tableQ41_2, main="Pamphlets in Doctor's Office (or other HCP)",
        xlab="Categories of Hearing Loss", col=c("red","blue"),
        ylim=c(0,1000),
        ylab = "Number of Participants",
        legend = rownames(tableQ41_14), beside=TRUE)


global_data$Q41_3 <- global_data$q0041_0003
global_data$Q41_3[is.na(global_data$Q41_3)] <- 0
#Making Labels for Q41_3 table
tableQ41_3 <- table(global_data$Q41_3,global_data$hcat)
tableQ41_3 <- as.matrix(tableQ41_3)
row.names(tableQ41_3)<-c('Did Not Select','Selected')
colnames(tableQ41_3) <-c('Normal','Mild','Moderate','Severe')
#BarPlot
barplot(tableQ41_3, main="Radio Ads",
        xlab="Categories of Hearing Loss", col=c("red","blue"),
        ylim=c(0,1000),
        ylab = "Number of Participants",
        legend = rownames(tableQ41_14), beside=TRUE)

global_data$Q41_4 <- global_data$q0041_0004
global_data$Q41_4[is.na(global_data$Q41_4)] <- 0
#Making Labels for Q41_4 table
tableQ41_4 <- table(global_data$Q41_4,global_data$hcat)
tableQ41_4 <- as.matrix(tableQ41_4)
row.names(tableQ41_4)<-c('Did Not Select','Selected')
colnames(tableQ41_4) <-c('Normal','Mild','Moderate','Severe')
#BarPlot
barplot(tableQ41_4, main="Newspaper",
        xlab="Categories of Hearing Loss", col=c("red","blue"),
        ylim=c(0,1000),
        ylab = "Number of Participants",
        legend = rownames(tableQ41_14), beside=TRUE)

global_data$Q41_5 <- global_data$q0041_0005
global_data$Q41_5[is.na(global_data$Q41_5)] <- 0
#Making Labels for Q41_5 table
tableQ41_5 <- table(global_data$Q41_5,global_data$hcat)
tableQ41_5 <- as.matrix(tableQ41_5)
row.names(tableQ41_5)<-c('Did Not Select','Selected')
colnames(tableQ41_5) <-c('Normal','Mild','Moderate','Severe')
#BarPlot
barplot(tableQ41_5, main="Pamphlets that come to you in the mail",
        xlab="Categories of Hearing Loss", col=c("red","blue"),
        ylim=c(0,1000),
        ylab = "Number of Participants",
        legend = rownames(tableQ41_14), beside=TRUE)

global_data$Q41_6 <- global_data$q0041_0006
global_data$Q41_6[is.na(global_data$Q41_6)] <- 0
#Making Labels for Q41_6 table
tableQ41_6 <- table(global_data$Q41_6,global_data$hcat)
tableQ41_6 <- as.matrix(tableQ41_6)
row.names(tableQ41_6)<-c('Did Not Select','Selected')
colnames(tableQ41_6) <-c('Normal','Mild','Moderate','Severe')
#BarPlot
barplot(tableQ41_6, main="TV Ads",
        xlab="Categories of Hearing Loss", col=c("red","blue"),
        ylim=c(0,1000),
        ylab = "Number of Participants",
        legend = rownames(tableQ41_14), beside=TRUE)

global_data$Q41_7 <- global_data$q0041_0007
global_data$Q41_7[is.na(global_data$Q41_7)] <- 0
#Making Labels for Q41_7 table
tableQ41_7 <- table(global_data$Q41_7,global_data$hcat)
tableQ41_7 <- as.matrix(tableQ41_7)
row.names(tableQ41_7)<-c('Did Not Select','Selected')
colnames(tableQ41_7) <-c('Normal','Mild','Moderate','Severe')
#BarPlot
barplot(tableQ41_7, main="University Websites on the Internet",
        xlab="Categories of Hearing Loss", col=c("red","blue"),
        ylim=c(0,1000),
        ylab = "Number of Participants",
        legend = rownames(tableQ41_14), beside=TRUE)

global_data$Q41_8 <- global_data$q0041_0008
global_data$Q41_8[is.na(global_data$Q41_8)] <- 0
#Making Labels for Q41_8 table
tableQ41_8 <- table(global_data$Q41_8,global_data$hcat)
tableQ41_8 <- as.matrix(tableQ41_8)
row.names(tableQ41_8)<-c('Did Not Select','Selected')
colnames(tableQ41_8) <-c('Normal','Mild','Moderate','Severe')
#BarPlot
barplot(tableQ41_8, main="Advertisements on the Internet",
        xlab="Categories of Hearing Loss", col=c("red","blue"),
        ylim=c(0,1000),
        ylab = "Number of Participants",
        legend = rownames(tableQ41_14), beside=TRUE)

global_data$Q41_9 <- global_data$q0041_0009
global_data$Q41_9[is.na(global_data$Q41_9)] <- 0
#Making Labels for Q41_9 table
tableQ41_9 <- table(global_data$Q41_9,global_data$hcat)
tableQ41_9 <- as.matrix(tableQ41_9)
row.names(tableQ41_9)<-c('Did Not Select','Selected')
colnames(tableQ41_9) <-c('Normal','Mild','Moderate','Severe')
#BarPlot
barplot(tableQ41_9, main="Government Websites on the Internet",
        xlab="Categories of Hearing Loss", col=c("red","blue"),
        ylim=c(0,1000),
        ylab = "Number of Participants",
        legend = rownames(tableQ41_14), beside=TRUE)


global_data$Q41_10 <- global_data$q0041_0010
global_data$Q41_10[is.na(global_data$Q41_10)] <- 0
#Making Labels for Q41_10 table
tableQ41_10 <- table(global_data$Q41_10,global_data$hcat)
tableQ41_10 <- as.matrix(tableQ41_10)
row.names(tableQ41_10)<-c('Did Not Select','Selected')
colnames(tableQ41_10) <-c('Normal','Mild','Moderate','Severe')
#BarPlot
barplot(tableQ41_10, main="Medical Websites on the Internet",
        xlab="Categories of Hearing Loss", col=c("red","blue"),
        ylim=c(0,1000),
        ylab = "Number of Participants",
        legend = rownames(tableQ41_14), beside=TRUE)


global_data$Q41_11 <- global_data$q0041_0011
global_data$Q41_11[is.na(global_data$Q41_11)] <- 0
#Making Labels for Q41_11 table
tableQ41_11 <- table(global_data$Q41_11,global_data$hcat)
tableQ41_11 <- as.matrix(tableQ41_11)
row.names(tableQ41_11)<-c('Did Not Select','Selected')
colnames(tableQ41_11) <-c('Normal','Mild','Moderate','Severe')
#BarPlot 
barplot(tableQ41_11, main="Friends and Family",
        xlab="Categories of Hearing Loss", col=c("red","blue"),
        ylim=c(0,1000),
        ylab = "Number of Participants",
        legend = rownames(tableQ41_14), beside=TRUE)

global_data$Q41_12 <- global_data$q0041_0012
global_data$Q41_12[is.na(global_data$Q41_12)] <- 0
#Making Labels for Q41_12 table
tableQ41_12 <- table(global_data$Q41_12,global_data$hcat)
tableQ41_12 <- as.matrix(tableQ41_12)
row.names(tableQ41_12)<-c('Did Not Select','Selected')
colnames(tableQ41_12) <-c('Normal','Mild','Moderate','Severe')
#BarPlot 
barplot(tableQ41_12, main="Hearing Loss Associations/Charities",
        xlab="Categories of Hearing Loss", col=c("red","blue"),
        ylim=c(0,1000),
        ylab = "Number of Participants",
        legend = rownames(tableQ41_14), beside=TRUE)

global_data$Q41_13 <- global_data$q0041_0013
global_data$Q41_13[is.na(global_data$Q41_13)] <- 0
#Making Labels for Q41_13 table
tableQ41_13 <- table(global_data$Q41_13,global_data$hcat)
tableQ41_13 <- as.matrix(tableQ41_13)
row.names(tableQ41_13)<-c('Did Not Select','Selected')
colnames(tableQ41_13) <-c('Normal','Mild','Moderate','Severe')
#BarPlot 
barplot(tableQ41_13, main="Advertisements in Public Places",
        xlab="Categories of Hearing Loss", col=c("red","blue"),
        ylim=c(0,1000),
        ylab = "Number of Participants",
        legend = rownames(tableQ41_14), beside=TRUE)

global_data$Q41_14 <- global_data$q0041_0014
global_data$Q41_14[is.na(global_data$Q41_14)] <- 0
#Making Labels for Q41_14 table
tableQ41_14 <- table(global_data$Q41_14,global_data$hcat)
tableQ41_14 <- as.matrix(tableQ41_14)
row.names(tableQ41_14)<-c('Did Not Select','Selected')
colnames(tableQ41_14) <-c('Normal','Mild','Moderate','Severe')
#BarPlot 
barplot(tableQ41_14, main="Public Libraries",
        xlab="Categories of Hearing Loss", col=c("red","blue"),
        ylim=c(0,1000),
        ylab = "Number of Participants",
        legend = rownames(tableQ41_14), beside=TRUE)



#makes a grid table
grid.table(table(US_data$provider,US_data$hcat))
(table(global_data$HAold,global_data$hcat))

# Stacked Bar Plot with Colors and Legend (Copy and paste into console to have appear)
barplot(tableQ41_14, main="Public Libraries",
        xlab="Categories of Hearing Loss", col=c("red","blue"),
        ylim=c(0,1000),
        ylab = "Number of Participants",
        legend = rownames(tableQ41_14))

# Grouped Bar Plot(Copy and paste into the Console to have appear)
barplot(tableQ41_14, main="Public Libraries",
        xlab="Categories of Hearing Loss", col=c("red","blue"),
        ylim=c(0,1000),
        ylab = "Number of Participants",
        legend = rownames(tableQ41_14), beside=TRUE)

# Percentage BarPlot Example


#Reference Example For Barplot
global_data$Q41_14 <- global_data$q0041_0014
global_data$Q41_14[is.na(global_data$Q41_14)] <- 0
#Making Labels for Q41_14 table
tableQ41_14 <- table(global_data$Q41_14,global_data$hcat)
tableQ41_14 <- as.matrix(tableQ41_14)
row.names(tableQ41_14)<-c('Did Not Select','Selected')
colnames(tableQ41_14) <-c('Normal','Mild','Moderate','Severe')
#BarPlot 
barplot(tableQ41_14, main="Public Libraries",
        xlab="Categories of Hearing Loss", col=c("red","blue"),
        ylim=c(0,1000),
        ylab = "Number of Participants",
        legend = rownames(tableQ41_14), beside=TRUE)

#OBJ rename table columns and row.
#1. Assign table a name ("test)
test<-table(global_data$dpressure,global_data$hcat)
#2. Save "test" as a matrix (as.matrix(test))
test <- as.matrix(test)
#3. Select and assign row names
row.names(test)<-c('no','yes')
#4. Select and assign column names
colnames(test) <-c('normal','mild','mod','severe')
#5 Type "test" or grid.table(test) and it should all come together
grid.table(test)

test<-table(global_data$dpressure,global_data$hcat)
test <- as.matrix(test)
row.names(test)<-c('Did Not Select','Selected')
colnames(test) <-c('Normal','Mild','Moderate','Severe')

# Sorting Q41
Q41 <- as.data.frame(table(global_data$Q41_1)[2])
Q41[2] <- table(global_data$Q41_2)[2]
Q41[3] <- table(global_data$Q41_3)[2]
Q41[4] <- table(global_data$Q41_4)[2]
Q41[5] <- table(global_data$Q41_5)[2]
Q41[6] <- table(global_data$Q41_6)[2]
Q41[7] <- table(global_data$Q41_7)[2]
Q41[8] <- table(global_data$Q41_8)[2]
Q41[9] <- table(global_data$Q41_9)[2]
Q41[10] <- table(global_data$Q41_10)[2]
Q41[11] <- table(global_data$Q41_11)[2]
Q41[12] <- table(global_data$Q41_12)[2]
Q41[13] <- table(global_data$Q41_13)[2]
Q41[14] <- table(global_data$Q41_14)[2]

Q41<-t(Q41)
Q41 <- as.data.frame(Q41)
Q41[,2] <- Q41
Q41[,1] <- c('Doctor','Pamphlets in HCP Office', 'Radio Ads', 'Newspaper', 'Mailed Pamphlets','TV Ads', 'University Websites', 'Internet Ads', 'Government Websites', 'Medical Websites', 'Friends and Family', 'Hearing Loss Associations_Charities', 'Ads in Public Places', 'Public Libraries')
names(Q41) <- c('Question','Responses')
Q41<-(Q41[order(-Q41$Responses),])

#Organizing Q41 Global In-Person Data into horizontal graph
ggplot(data=Q41, aes(x=Question,y=Responses)) +   
  geom_bar(,stat="identity") +    
  coord_flip()+ 
  ggtitle("Q41 Global In-Person Data") +
  scale_x_discrete(   limits=rev(c(t(Q41[1]))))

#Country Data Stratification. 
data_stratify <- function(data){  
  data$Q41_1 <- data$q0041_0001  
  data$Q41_1[is.na(data$Q41_1)] <- 0 
  data$Q41_2 <- data$q0041_0002  
  data$Q41_2[is.na(data$Q41_2)] <- 0 
  data$Q41_3 <- data$q0041_0003  
  data$Q41_3[is.na(data$Q41_3)] <- 0 
  data$Q41_4 <- data$q0041_0004  
  data$Q41_4[is.na(data$Q41_4)] <- 0 
  data$Q41_5 <- data$q0041_0005  
  data$Q41_5[is.na(data$Q41_5)] <- 0 
  data$Q41_6 <- data$q0041_0006  
  data$Q41_6[is.na(data$Q41_6)] <- 0
  data$Q41_7 <- data$q0041_0007  
  data$Q41_7[is.na(data$Q41_7)] <- 0 
  data$Q41_8 <- data$q0041_0008  
  data$Q41_8[is.na(data$Q41_8)] <- 0 
  data$Q41_9 <- data$q0041_0009  
  data$Q41_9[is.na(data$Q41_9)] <- 0 
  data$Q41_10 <- data$q0041_0010  
  data$Q41_10[is.na(data$Q41_10)] <- 0 
  data$Q41_11 <- data$q0041_0011  
  data$Q41_11[is.na(data$Q41_11)] <- 0 
  data$Q41_12 <- data$q0041_0012 
  data$Q41_12[is.na(data$Q41_12)] <- 0 
  data$Q41_13 <- data$q0041_0013  
  data$Q41_13[is.na(data$Q41_13)] <- 0 
  data$Q41_14 <- data$q0041_0014  
  data$Q41_14[is.na(data$Q41_14)] <- 0 
  return(data) }  
US_data <- data_stratify(US_data)
UK_data <- data_stratify(UK_data)
CAN_data <- data_stratify(CAN_data)
AUS_data <- data_stratify(AUS_data)





#Graphing Q41 Function
Graph_Q41 <- function(data) {
  Q41 <- as.data.frame(table(data$Q41_1)[2]/nrow(data)*100)
  Q41[2] <- table(data$Q41_2)[2]/nrow(data)*100
  Q41[3] <- table(data$Q41_3)[2]/nrow(data)*100
  Q41[4] <- table(data$Q41_4)[2]/nrow(data)*100
  Q41[5] <- table(data$Q41_5)[2]/nrow(data)*100
  Q41[6] <- table(data$Q41_6)[2]/nrow(data)*100
  Q41[7] <- table(data$Q41_7)[2]/nrow(data)*100
  Q41[8] <- table(data$Q41_8)[2]/nrow(data)*100
  Q41[9] <- table(data$Q41_9)[2]/nrow(data)*100
  Q41[10] <- table(data$Q41_10)[2]/nrow(data)*100
  Q41[11] <- table(data$Q41_11)[2]/nrow(data)*100
  Q41[12] <- table(data$Q41_12)[2]/nrow(data)*100
  Q41[13] <- table(data$Q41_13)[2]/nrow(data)*100
  Q41[14] <- table(data$Q41_14)[2]/nrow(data)*100
  
  Q41<-t(Q41)
  Q41 <- as.data.frame(Q41)
  Q41[,2] <- Q41
  Q41[,1] <- c('Doctor','Pamphlets in HCP Office', 'Radio Ads', 'Newspaper', 'Mailed Pamphlets','TV Ads', 'University Websites', 'Internet Ads', 'Government Websites', 'Medical Websites', 'Friends and Family', 'Hearing Loss Associations/Charities', 'Ads in Public Places', 'Public Libraries')
  names(Q41) <- c('Question','Responses')
  Q41<-(Q41[order(-Q41$Responses),])
  
  #Organizing Q41  In-Person Data into horizontal graph
  Q41plot <- ggplot(data=Q41, aes(x=Question,y=Responses)) +   
    geom_bar(,stat="identity") +    
    coord_flip()+ 
    scale_x_discrete(   limits=rev(c(t(Q41[1]))))+ 
    scale_y_continuous (limits=c(0,100) )
return(Q41plot)
  
} 

#Q41 Plotting individual countries
USQ41plot<-Graph_Q41(US_data)
USQ41plot + ggtitle("Q41 US In-Person Data") 

UKQ41plot<-Graph_Q41(UK_data)
UKQ41plot + ggtitle("Q41 UK In-Person Data")

CANQ41plot<-Graph_Q41(CAN_data)
CANQ41plot + ggtitle("Q41 Canada In-Person Data")

AUSQ41plot<-Graph_Q41(AUS_data)
AUSQ41plot + ggtitle("Q41 Australia In-Person Data")

#stratifying hcat 
global_1 <- global_data%>%filter(hcat==1)
global_2 <- global_data%>%filter(hcat==2)
global_3 <- global_data%>%filter(hcat==3)
global_4 <- global_data%>%filter(hcat==4)

hcat1Q41plot<-Graph_Q41(global_1)
hcat1Q41plot + ggtitle("Q41 Normal Hearing Data")

hcat2Q41plot<-Graph_Q41(global_2)
hcat2Q41plot + ggtitle("Q41 Mild Hearing Loss Data") 

hcat3Q41plot<-Graph_Q41(global_3)
hcat3Q41plot + ggtitle("Q41 Moderate Hearing Loss Data") 

hcat4Q41plot<-Graph_Q41(global_4)
hcat4Q41plot + ggtitle("Q41 Severe Hearing Loss Data") 


#Sum of Responses (or responses per person)
global_1$q0041_0001
sum(global_1$q0041_0001)

sum(global_1[88:101],na.rm=T)/nrow(global_1)
sum(global_2[88:101],na.rm=T)/nrow(global_2)
sum(global_3[88:101],na.rm=T)/nrow(global_3)
sum(global_4[88:101],na.rm=T)/nrow(global_4)

sum(US_data[88:101],na.rm=T)/nrow(US_data)
sum(AUS_data[88:101],na.rm=T)/nrow(AUS_data)
sum(UK_data[92:105],na.rm=T)/nrow(UK_data)
sum(CAN_data[92:105],na.rm=T)/nrow(CAN_data)







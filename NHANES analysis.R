library(Hmisc)
library(SASxport)
library(gridExtra)
library(dplyr)
library(ggplot2)

#Import SAS datasets
setwd('C:/Users/cwbishop/Documents/Github/eHearing/NHANES datasets')

################### LOAD DATA ###########################

# Import and merge all "DEMO" datasets
demo_current <- NULL
filelist <- Sys.glob("DEMO*")
for (i in 1:length(filelist)){
    print(filelist[i])
    demo_add <- read.xport(filelist[i])
    demo_current <- merge(demo_add,demo_current,all=TRUE)
}

# Import and merge all "HSQ" datasets
HSQ_current <- NULL
filelist <- Sys.glob("HSQ*")
for (i in 1:length(filelist)){
    print(filelist[i])
    HSQ_add <- read.xport(filelist[i])
    HSQ_current <- merge(HSQ_add,HSQ_current,all=TRUE)
}

# Import and merge all "AUX" datasets
aux_current <- NULL
filelist <- Sys.glob("AUX*")
for (i in 1:length(filelist)){
    print(filelist[i])
    aux_add <- read.xport(filelist[i])
    aux_current <- merge(aux_add,aux_current,all=TRUE)
}

# Import and merge all "AUQ" datasets
auq_current <- NULL
filelist <- Sys.glob("AUQ*")
for (i in 1:length(filelist)){
    print(filelist[i])
    auq_add <- read.xport(filelist[i])
    auq_current <- merge(auq_add,auq_current,all=TRUE)
}

#Merge datasets
all_data_temp <- merge(demo_current,HSQ_current,by='SEQN',all=TRUE)
all_data_temp <- merge(all_data_temp,auq_current,by='SEQN',all=TRUE)
all_data <- merge(all_data_temp,aux_current,by='SEQN',all=TRUE)

################### LOAD DATA ###########################

## Relevant measures

# AUXU1K1R - Right threshold @ 1000Hz (db)
# AUXU500R - Right threshold @ 500Hz (db)
# AUXU1K2R - Right Threshold @ 1000Hz-2nd Read (db)
# AUXU2KR - Right threshold @ 2000Hz (db)
# AUXU3KR - Right threshold @ 3000Hz (db)
# AUXU4KR - Right threshold @ 4000Hz (db)
# AUXU6KR - Right threshold @ 6000Hz (db)
# AUXU8KR - Right threshold @ 8000Hz (db)
# AUXU1K1L - Left threshold @ 1000Hz (db)
# AUXU500L - Left threshold @ 500Hz (db)
# AUXU1K2L - Left Threshold @ 1000Hz-2nd Read (db)
# AUXU2KL - Left threshold @ 2000Hz (db)
# AUXU3KL - Left threshold @ 3000Hz (db)
# AUXU4KL - Left threshold @ 4000Hz (db)
# AUXU6KL - Left threshold @ 6000Hz (db)
# AUXU8KL - Left threshold @ 8000Hz (db)
# AUQ054 - General condition of hearing
# AUQ060 - Hear a whisper from across a quiet room?
# AUQ070 - Hear normal voice across a quiet room?
# AUQ080 - Hear a shout from across a quiet room?
# AUQ090 - Hear if spoken loudly to in better ear?
# AUQ100 - Difficult follow conversation if noise
# AUQ110 - Hearing cause frustration when talking?
# AUQ146 - Ever worn hearing aid/cochlear implant?


# PTA calculation: average of dbhl thresholds at 500, 1000,2000,4000 hz for both ears. Better ear PTA (BEPTA) is the lowest average of 2 ears

all_data$RPTA <- rowMeans(all_data[,c('AUXU500R','AUXU1K1R','AUXU2KR','AUXU4KR')])
all_data$LPTA <- rowMeans(all_data[,c('AUXU500L','AUXU1K1L','AUXU2KL','AUXU4KL')])
all_data$BEPTA <- apply(all_data[,c('RPTA','LPTA')],1,min)
all_data$BEPTA[all_data$BEPTA>100] <- NA

# Examine relationship between BEPTA and response to General condition of hearing
cor.test(all_data$BEPTA,all_data$AUQ054)
plot(all_data$BEPTA,all_data$AUQ054,ylim=c(0,8))

ggplot(all_data)
#Examine age distribution

hist(all_data$RIDAGEYR,breaks=80)
table(all_data$RIAGENDR,all_data$RIDAGEYR)

# Examine age by gender
age_gender_table <- as.data.frame(table(all_data$RIAGENDR,all_data$RIDAGEYR))
age_gender_table_65 <- age_gender_table[133:dim(age_gender_table)[1],]

age_gender_table$Var1 <- as.numeric(age_gender_table$Var1)
age_gender_table$Var2 <- as.numeric(age_gender_table$Var2)

age_gender_table_11<- as.data.frame(table(dataset_11$RIAGENDR,dataset_11$RIDAGEYR))

#age_gender_table_09_10 <- as.data.frame(table(data_09_10$RIAGENDR,data_09_10$RIDAGEYR))
#age_gender_table_09_10_65 <- age_gender_table_09_10[133:dim(age_gender_table_09_10)[1],]

age_gender_table_11<- as.data.frame(table(dataset_11$RIAGENDR,dataset_11$RIDAGEYR))

# Create table by gender and over 65
##Convert Var1 and Var2 to numerical
age_gender_table_11$Var1 <- as.numeric(age_gender_table_11$Var1)
age_gender_table_11$Var2 <- as.numeric(age_gender_table_11$Var2)


#sum frequecies by gender and age cutoff of 65

table_dataset <- matrix(c(sum(age_gender_table$Freq[age_gender_table$Var2>=65&age_gender_table$Var1==2]),sum(age_gender_table$Freq[age_gender_table$Var2>=65&age_gender_table$Var1==1]),sum(age_gender_table$Freq[age_gender_table$Var2<65&age_gender_table$Var1==2]),sum(age_gender_table$Freq[age_gender_table$Var2<65&age_gender_table$Var1==1])),ncol=2,byrow=TRUE)

## Examine audiometric data in individuals over 65

table(all_data$BEPTA[all_data$RIDAGEYR>64])
plot(all_data$BEPTA[all_data$RIDAGEYR>64])
plot(all_data$BEPTA,xlim=c(0,62000))
addmargins(table(all_data$BEPTA[all_data$RIDAGEYR>64]))
addmargins(table(all_data$BEPTA))






# For 2011 dataset with audio
table_dataset_11 <- matrix(c(sum(age_gender_table_11$Freq[age_gender_table_11$Var2>=65&age_gender_table_11$Var1==2]),sum(age_gender_table_11$Freq[age_gender_table_11$Var2>=65&age_gender_table_11$Var1==1]),sum(age_gender_table_11$Freq[age_gender_table_11$Var2<65&age_gender_table_11$Var1==2]),sum(age_gender_table_11$Freq[age_gender_table_11$Var2<65&age_gender_table_11$Var1==1])),ncol=2,byrow=TRUE)

# Add new column 1 if <65 and 2 if >= 65 years of age

all_data$age[all_data$RIDAGEYR<65] <- 1
all_data$age[all_data$RIDAGEYR>=65] <- 2
all_data_count <- all_data %>% count(AUQ054,RIAGENDR,age)

grid.table(all_data_count)

colnames(table_dataset) <- c('Male','Female')
rownames(table_dataset) <- c('65+','<65')
grid.table(addmargins(table_dataset))



dataset_11$RIDAGEYR<65
dataset_11$age[dataset_11$RIDAGEYR<65]=1
dataset_11$age[dataset_11$RIDAGEYR>=65]=2

dataset_11_count <- dataset_11 %>% count(AUQ054,RIAGENDR,age)

grid.table(dataset_11_count)

colnames(table_dataset_11) <- c('Male','Female')
rownames(table_dataset_11) <- c('65+','<65')
grid.table(addmargins(table_dataset_11))












# For 2011 dataset
table_age_gender_11_12 <- matrix(c(sum(age_gender_table_11_12$Freq[age_gender_table_11_12$Var2>=65&age_gender_table_11_12$Var1==2]),sum(age_gender_table_11_12$Freq[age_gender_table_11_12$Var2>=65&age_gender_table_11_12$Var1==1]),sum(age_gender_table_11_12$Freq[age_gender_table_11_12$Var2<65&age_gender_table_11_12$Var1==2]),sum(age_gender_table_11_12$Freq[age_gender_table_11_12$Var2<65&age_gender_table_11_12$Var1==1])),ncol=2,byrow=TRUE)
colnames(table_age_gender_11_12) <- c('Male','Female')
rownames(table_age_gender_11_12) <- c('65+','<65')
grid.table(addmargins(table_age_gender_11_12))









#OLD
age_table_11_12 <- as.data.frame(table(data_11_12$RIDAGEYR))
age_table_09_10 <- as.data.frame(table(data_09_10$RIDAGEYR))

age_table_11_12_65 <- age_table_11_12[66:dim(age_table_11_12)[1],]
age_table_09_10_65 <- age_table_09_10[66:dim(age_table_09_10)[1],]
sum(age_table_09_10$Freq)
sum(age_table_11_12$Freq)

sum(age_table_11_12_65$Freq)
sum(age_table_09_10_65$Freq)


lookup.xport("DEMO.XPT")
data_b <- read.xport("DEMO_B.XPT")
data_11_12 <- read.xport("DEMO_G.XPT")
aux_data_11 <- read.xport('AUX_G.XPT')
auq_data_11 <- read.xport('AUQ_G.XPT')

# add 2009 dataset later
data_09_10 <- read.xport("DEMO_F.XPT")

# Subset relevant variables of AUQ: Respondent number, General hearing condition
auq_11 <- select(auq_data_11,SEQN:AUQ054)

# From demo dataset, subset respondent number, age, gender
demo_11 <- select(data_11_12,SEQN:RIDAGEYR)

#Merge demo and auq datasets. This will result in fewer subject numbers due to missing AUQ data
dataset_11 <- merge(auq_11,demo_11, by='SEQN')

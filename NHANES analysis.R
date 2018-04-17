library(Hmisc)
library(SASxport)
library(gridExtra)
library(dplyr)

#Import SAS datasets
setwd('C:/Users/cwbishop/Documents/Github/eHearing')
lookup.xport("DEMO_G.XPT")
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



#Examine age distribution

hist(dataset_11$RIDAGEYR,breaks=80)


# Examine age by gender
age_gender_table_09_10 <- as.data.frame(table(data_09_10$RIAGENDR,data_09_10$RIDAGEYR))
age_gender_table_09_10_65 <- age_gender_table_09_10[133:dim(age_gender_table_09_10)[1],]

age_gender_table_11<- as.data.frame(table(dataset_11$RIAGENDR,dataset_11$RIDAGEYR))

# Create table by gender and over 65
##Convert Var1 and Var2 to numerical
age_gender_table_11$Var1 <- as.numeric(age_gender_table_11$Var1)
age_gender_table_11$Var2 <- as.numeric(age_gender_table_11$Var2)


#sum frequecies by gender and age cutoff of 65

# For 2011 dataset with audio
table_dataset_11 <- matrix(c(sum(age_gender_table_11$Freq[age_gender_table_11$Var2>=65&age_gender_table_11$Var1==2]),sum(age_gender_table_11$Freq[age_gender_table_11$Var2>=65&age_gender_table_11$Var1==1]),sum(age_gender_table_11$Freq[age_gender_table_11$Var2<65&age_gender_table_11$Var1==2]),sum(age_gender_table_11$Freq[age_gender_table_11$Var2<65&age_gender_table_11$Var1==1])),ncol=2,byrow=TRUE)

# Add new column 1 if <65 and 2 if >= 65 years of age

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

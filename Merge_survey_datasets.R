library(foreign)
library(plyr)
library(dplyr)
library(Hmisc)


df_combined <- NULL
df_current_temp <- NULL
setwd('E:/Lab/Project AF/2018 SPSS Data Sets')
filename <- file.choose()
df_current <- read.spss(filename, to.data.frame=TRUE)

basename(filename)

df_current$country_id <- 'AUS'
df_current$state_id <- 'NA'
df_current$city_id <- 'NA'
df_current$testing <- 'IP'


## Some datasets have different colnames. This code tries to align them

dataset.labels <- attr(df_current,"variable.labels")

df_current_temp$gender <- as.data.frame(df_current[which(dataset.labels=="What is your gender?")])
df_current_temp$race <- df_current[which(dataset.labels=="Would you describe yourself as?")]
df_current_temp$ethnic <- df_current[which(dataset.labels=="Would you describe yourself as?")+1]
df_current_temp <- as.data.frame(df_current_temp)
colnames(df_current_temp) <- c('gender','race','ethnic')
df_current <- df_current[1:100]
df_current <- as.data.frame(c(df_current,df_current_temp))
## Test column names
#unique(df_current['q0013'])
#unique(df_combined_ip['q0013'])
#unique(df_current['q0014'])
#unique(df_combined_ip['q0014'])
#
#unique(df_current['q0015'])
#unique(df_combined_ip['q0015'])
#
##Shift column names
#names(df_current)[names(df_current)=='q0014'] <- 'q0015'
#names(df_current)[names(df_current)=='q0013'] <- 'q0014'



# Merge df's   
df_combined <- rbind.fill(df_combined,df_current)

# Merge ip
df_combined_ip <-df_combined 

# Merge RN
df_combined_rn <- df_combined


df_comb <- rbind.fill(df_combined_ip,df_combined_rn)

write.csv(df_comb, file = paste('merged_survey_data',Sys.Date(),'.csv',sep=""))



df_comb_in_person <- df_combined




)



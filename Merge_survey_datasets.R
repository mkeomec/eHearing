library(foreign)
library(plyr)
library(dplyr)
library(Hmisc)


df_combined <- NULL
setwd('E:/Lab/Project AF/2018 SPSS Data Sets')


df_current_temp <- NULL

filename <- file.choose()
df_current <- read.spss(filename, to.data.frame=TRUE)

basename(filename)

## Some datasets have different colnames. This code tries to align them

dataset.labels <- attr(df_current,"variable.labels")

df_current_temp$gender <- as.data.frame(df_current[which(dataset.labels=="What is your gender?")])
df_current_temp$race <- df_current[which(dataset.labels=="Would you describe yourself as?")]
df_current_temp$ethnic <- df_current[which(dataset.labels=="Would you describe yourself as?")+1]

df_current_temp <- as.data.frame(df_current_temp)
df_current_temp$country_id <- 'CAN'
df_current_temp$state_id <- 'NA'
df_current_temp$city_id <- 'NA'
df_current_temp$testing <- 'RN'


colnames(df_current_temp) <- c('gender','race','ethnic','country_id','state_id','city_id','testing')
#df_current <- df_current[1:100]
#df_current <- as.data.frame(c(df_current,df_current_temp))


# Merge df's   
df_combined <- rbind.fill(df_combined,df_current_temp)




# Reassign to ip dataset
df_combined_ip <-df_combined 

# Reassign to RN dataset
df_combined_rn <- df_combined


df_comb <- rbind.fill(df_combined_ip,df_combined_rn)

write.csv(df_comb, file = paste('merged_survey_data',Sys.Date(),'.csv',sep=""))












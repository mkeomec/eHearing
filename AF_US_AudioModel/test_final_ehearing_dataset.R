#Sample Test code for all global eHearing dataset 

library(foreign)
library('plyr')
library("dplyr")
library("tidyr")
library("ggplot2")
library('caTools')
library("Hmisc")
library("boot")
library("gridExtra")
library("MASS")
library("MuMIn")
library("MatchIt")
library('fastDummies')
library("RItools")

#Import data
data<- read.csv("C:/Users/cwbishop/Documents/dropbox/Dropbox/Work/UW BandBlab/Studies/eHearing/2018 SPSS datasets/ehearing_all_4-12-2019v1.csv", header=T, na.strings=c("#NULL!",'90NR'))


#1) take self report hearing loss and divide into 'no problem' vs ' some problem reported' and then tabulate 'reported hearing status' vs age group (10 year bands), possession of aid, seen a primary care doctor about hearing, seen audiologist or otologist about hearing

data$q0042
# Age 2= 60-64, 3=65-69, 4=70-74,5=75-79, 6=80-84,7=85+
table(data$q0003)

table(data$q0046)
table(data$q0003,data$q0046)
age_hearing_table <- table(data$q0003,data$q0046)
dimnames(age_hearing_table) <- list(c("60-64", "65-69","70-74",'75-79','80-84','80+'),c('I dont know','No problems with hearing','No problems with hearing, but other people think so','Hearing problem, have not sought help','Hearing problem use hearing aids'))
age_hearing_table                           

#data$q0053 "What is the title of the professional who tested your hearing?" 
#response 2 is Physician (e.g., primary care provider, family physician, general practitioner, ear/nose/throat doctor)
#response 3 is Audiologist


table(data$q0003,data$q0053)
age_who_test_hearing_table <- table(data$q0003,data$q0053)
dimnames(age_who_test_hearing_table) <- list(c("60-64", "65-69","70-74",'75-79','80-84','80+'),c('1','2','3','4','5','6','7','8','9','11'))
age_who_test_hearing_table

#2) take 'reported hearing status' and tabulate for those with HHT against the severity of better ear hearing impairment using GBD classification, (which i presume you have) and WHO or ASHA classification which I think you already used.

table(data$q0046,data$hcat)

#3) tabulate 'reported hearing status' vs tinnitus presence and age groups AND tinnitus within age groups
#No
#Yes, some of the time
#Yes, most or all of the time

table(data$q0046,data$q0034)

#4) tabulate 'reported hearing status' by sex / gender and age groups AND sex within age groups
table(data$q0046,data$q0002)
table(data$q0046,data$q0003)
table(data$q0046,data$q0002,data$q0003)

#For people with different demographic characteristics and hearing status (w/wo HL; w/wo HA),

#How does awareness of comorbidities and general health (e.g., Q 18-25, 28-30, 78, 82, 83, 121, 132, 135, 136, 185-187) affect help-seeking and interest in regard to different rehab options?

#How do interpersonal relationships (e.g., Q 5, 6, 7, 31, 33, 63-65, 115, 117-119, 145, 169-173, 177) affect help-seeking and interest in regard to different rehab options?

#How do views about stigma (e.g., Q 59-61, 64, 68, 91,111, 114, 115, 117, 118, 120-122, 133, 134, 144, 145, 167, 168, 171, 172, 174, 175, 184-197) affect help-seeking and interest in regard to different rehab options?
#How does experience of hearing testing and hearing conservation in the workplace (e.g., Q 11, 36-38, 51-53, 93, 99-102, 106, 115, 146, 152-155, 159, 165, 166) affect help-seeking and interest in regard to different rehab options?

#ONLY FOR THOSE WHO DID AUDIOGRAMS: In addition to audiometry, what other factors contribute to self-perceptions of hearing loss?

library(gridExtra)
library(foreign)
library(plyr)
setwd('C:/Users/cwbishop/Documents/dropbox/Dropbox/Work/UW BandBlab/Studies/eHearing')



print('load USA data')
data_USA <- read.csv(file.choose(), header=TRUE)
data_count <- data.frame(table(data_USA['q0046']))
names(data_count) <- c('USA Pathway Response','# Responses')
grid.table(data_count)
colSums(data_count[2])
#collapse age
data_USA$age <- ''
data_USA$age[which(data_USA$q0003 %in% c(2,3))] <-1 
data_USA$age[which(data_USA$q0003 %in% c(4,5))] <-2
data_USA$age[which(data_USA$q0003 %in% c(6,7))] <-3
table(data_USA$age)
#collapse pathway
data_USA$pathway <- ''
data_USA$pathway[which(data_USA$q0046 %in% c(1,2,3))] <-1 
data_USA$pathway[which(data_USA$q0046 %in% c(4))] <-2
data_USA$pathway[which(data_USA$q0046 %in% c(5))] <-3
table(data_USA$pathway)


pathway_age <- (with(data_USA,table(age=age,pathway=pathway,gender=q0002)))
pathway_age <- pathway_age[-1,]
pathway_age_female <- pathway_age[,,gender=2]
pathway_age_male <- pathway_age[,,gender=3]
pathway_age_male <- pathway_age_male[-1,]
pathway_age_female <- pathway_age_female[-1,]
rownames(pathway_age_male) <- c('60-69','70-79','80+')
rownames(pathway_age_female) <- c('60-69','70-79','80+')

grid.table(pathway_age_male)
grid.table(pathway_age_female)




print('load UK data')
data_UK <- read.csv(file.choose(), header=TRUE)
data_count <- data.frame(table(data_UK['q0046']))
names(data_count) <- c('UK Pathway Response','# Responses')
grid.table(data_count)
colSums(data_count[2])
#collapse age
data_UK$age <- ''
data_UK$age[which(data_UK$q0003 %in% c(2,3))] <-1 
data_UK$age[which(data_UK$q0003 %in% c(4,5))] <-2
data_UK$age[which(data_UK$q0003 %in% c(6,7))] <-3
table(data_UK$age)
#collapse pathway
data_UK$pathway <- ''
data_UK$pathway[which(data_UK$q0046 %in% c(1,2,3))] <-1 
data_UK$pathway[which(data_UK$q0046 %in% c(4))] <-2
data_UK$pathway[which(data_UK$q0046 %in% c(5))] <-3
table(data_UK$pathway)

pathway_age <- (with(data_UK,table(age=age,pathway=pathway,gender=q0002)))
pathway_age_female <- pathway_age[,,gender=1]
pathway_age_male <- pathway_age[,,gender=2]
pathway_age_male <- pathway_age_male[-1,]
pathway_age_male <- pathway_age_male[,-1]
pathway_age_female <- pathway_age_female[-1,]
pathway_age_female <- pathway_age_female[,-1]
rownames(pathway_age_male) <- c('60-69','70-79','80+')
rownames(pathway_age_female) <- c('60-69','70-79','80+')

grid.table(pathway_age_male)
grid.table(pathway_age_female)



print('load CAN data')
data_CAN <- read.csv(file.choose(), header=TRUE)
data_count <- data.frame(table(data_CAN['q0046']))
names(data_count) <- c('CAN Pathway Response','# Responses')
grid.table(data_count)
colSums(data_count[2])

#collapse age
data_CAN$age <- ''
data_CAN$age[which(data_CAN$q0003 %in% c(2,3))] <-1 
data_CAN$age[which(data_CAN$q0003 %in% c(4,5))] <-2
data_CAN$age[which(data_CAN$q0003 %in% c(6,7))] <-3
table(data_CAN$age)
#collapse pathway
data_CAN$pathway <- ''
data_CAN$pathway[which(data_CAN$q0046 %in% c(1,2,3))] <-1 
data_CAN$pathway[which(data_CAN$q0046 %in% c(4))] <-2
data_CAN$pathway[which(data_CAN$q0046 %in% c(5))] <-3
table(data_CAN$pathway)
pathway_age <- (with(data_CAN,table(age=age,pathway=pathway)))
pathway_age <- pathway_age[,-1]
rownames(pathway_age) <- c('60-69','70-79','80+')
grid.table(pathway_age)

pathway_age <- (with(data_CAN,table(age=age,pathway=pathway,gender=q0002)))
pathway_age_female <- pathway_age[,,gender=1]
pathway_age_male <- pathway_age[,,gender=2]
pathway_age_male <- pathway_age_male[,-1]

pathway_age_female <- pathway_age_female[,-1]

rownames(pathway_age_male) <- c('60-69','70-79','80+')
rownames(pathway_age_female) <- c('60-69','70-79','80+')

grid.table(pathway_age_male)
grid.table(pathway_age_female)




print('load Australia data')
#dataset = data.frame(read.spss(file.choose()))
data_AUS = data.frame(read.csv(file.choose()))

data_count <- data.frame(table(dataset['q0046']))
names(data_count) <- c('AUS Pathway Response','# Responses')
grid.table(data_count)
#collapse age
data_AUS$age <- ''
data_AUS$age[which(data_AUS$q0003 %in% c(2,3))] <-1 
data_AUS$age[which(data_AUS$q0003 %in% c(4,5))] <-2
data_AUS$age[which(data_AUS$q0003 %in% c(6,7))] <-3
table(data_AUS$age)
#collapse pathway
data_AUS$pathway <- ''
data_AUS$pathway[which(data_AUS$q0046 %in% c(1,2,3))] <-1 
data_AUS$pathway[which(data_AUS$q0046 %in% c(4))] <-2
data_AUS$pathway[which(data_AUS$q0046 %in% c(5))] <-3
table(data_AUS$pathway)
pathway_age <- (with(data_AUS,table(age=age,pathway=pathway)))
pathway_age <- pathway_age[,-1]
pathway_age <- pathway_age[-1,]
rownames(pathway_age) <- c('60-69','70-79','80+')
grid.table(pathway_age)

pathway_age <- (with(data_AUS,table(age=age,pathway=pathway,gender=q0002)))
pathway_age_female <- pathway_age[,,gender=1]
pathway_age_male <- pathway_age[,,gender=2]
pathway_age_male <- pathway_age_male[,-1]
pathway_age_male <- pathway_age_male[-1,]
pathway_age_female <- pathway_age_female[,-1]
pathway_age_female <- pathway_age_female[-1,]
rownames(pathway_age_male) <- c('60-69','70-79','80+')
rownames(pathway_age_female) <- c('60-69','70-79','80+')

grid.table(pathway_age_male)
grid.table(pathway_age_female)


#Research Now country online survey

print('load USA data')
data_USA = data.frame(read.spss(file.choose()))

#collapse age
data_USA$age <- ''
data_USA$age[grep('60 ',data_USA$q0002)] <- 2
data_USA$age[grep('65 ',data_USA$q0002)] <- 3
data_USA$age[grep('70 ',data_USA$q0002)] <- 4
data_USA$age[grep('75 ',data_USA$q0002)] <- 5
data_USA$age[grep('80 ',data_USA$q0002)] <- 6
data_USA$age[grep('85 ',data_USA$q0002)] <- 7
data_USA$age[which(data_USA$age %in% c(2,3))] <-1 
data_USA$age[which(data_USA$age %in% c(4,5))] <-2
data_USA$age[which(data_USA$age %in% c(6,7))] <-3
table(data_USA$age)

#collapse pathway
data_USA$pathway <- ''
data_USA$pathway[grep('I do not know if I have a hearing problem',data_USA$q0045)] <- 1
data_USA$pathway[grep('I do not have any problems with my hearing and nobody has complained about my hearing',data_USA$q0045)] <- 1
data_USA$pathway[grep('I do not have any problems with my hearing but sometimes other people tell me I have a hearing problem',data_USA$q0045)] <- 1
data_USA$pathway[grep('I do have a hearing problem but I',data_USA$q0045)] <- 2
data_USA$pathway[grep('I do have a hearing problem and I wear hearing aids or use other technology to help me hear',data_USA$q0045)] <- 3

table(data_USA$pathway)
pathway_age <- (with(data_USA,table(age=age,pathway=pathway, gender=q0001)))
pathway_age_male <- pathway_age[,,gender='Male']
pathway_age_female <- pathway_age[,,gender='Female']
pathway_age_male <- pathway_age_male[,-1]
pathway_age_male <- pathway_age_male[-1,]
pathway_age_female <- pathway_age_female[,-1]
pathway_age_female <- pathway_age_female[-1,]
rownames(pathway_age_male) <- c('60-69','70-79','80+')
rownames(pathway_age_female) <- c('60-69','70-79','80+')
grid.table(pathway_age_male)
grid.table(pathway_age_female)


print('load UK data')
data_UK = data.frame(read.spss(file.choose()))

#collapse age
data_UK$age <- ''
data_UK$age[grep('60 ',data_UK$q0003)] <- 2
data_UK$age[grep('65 ',data_UK$q0003)] <- 3
data_UK$age[grep('70 ',data_UK$q0003)] <- 4
data_UK$age[grep('75 ',data_UK$q0003)] <- 5
data_UK$age[grep('80 ',data_UK$q0003)] <- 6
data_UK$age[grep('85 ',data_UK$q0003)] <- 7
data_UK$age[which(data_UK$age %in% c(2,3))] <-1 
data_UK$age[which(data_UK$age %in% c(4,5))] <-2
data_UK$age[which(data_UK$age %in% c(6,7))] <-3
table(data_UK$age)

#collapse pathway
data_UK$pathway <- ''
data_UK$pathway[grep('I do not know if I have a hearing problem',data_UK$q0046)] <- 1
data_UK$pathway[grep('I do not have any problems with my hearing and nobody has complained about my hearing',data_UK$q0046)] <- 1
data_UK$pathway[grep('I do not have any problems with my hearing but sometimes other people tell me I have a hearing problem',data_UK$q0046)] <- 1
data_UK$pathway[grep('I do have a hearing problem but I',data_UK$q0046)] <- 2
data_UK$pathway[grep('I do have a hearing problem and I wear hearing aids or use other technology to help me hear',data_UK$q0046)] <- 3

table(data_UK$pathway)
pathway_age <- (with(data_UK,table(age=age,pathway=pathway, gender=q0002)))
pathway_age_male <- pathway_age[,,gender='Male']
pathway_age_female <- pathway_age[,,gender='Female']
pathway_age_male <- pathway_age_male[,-1]
pathway_age_male <- pathway_age_male[-1,]
pathway_age_female <- pathway_age_female[,-1]
pathway_age_female <- pathway_age_female[-1,]
rownames(pathway_age_male) <- c('60-69','70-79','80+')
rownames(pathway_age_female) <- c('60-69','70-79','80+')
grid.table(pathway_age_male)
grid.table(pathway_age_female)

print('load CAN Research Now data')
data_CAN <- data.frame(read.spss(file.choose()))
data_count <- data.frame(table(data_CAN['q0045']))
names(data_count) <- c('CAN Pathway Response','# Responses')
grid.table(data_count)
colSums(data_count[2])

#collapse age
data_CAN$age <- ''
data_CAN$age[grep('60 ',data_CAN$q0002)] <- 2
data_CAN$age[grep('65 ',data_CAN$q0002)] <- 3
data_CAN$age[grep('70 ',data_CAN$q0002)] <- 4
data_CAN$age[grep('75 ',data_CAN$q0002)] <- 5
data_CAN$age[grep('80 ',data_CAN$q0002)] <- 6
data_CAN$age[grep('85 ',data_CAN$q0002)] <- 7
data_CAN$age[which(data_CAN$age %in% c(2,3))] <-1 
data_CAN$age[which(data_CAN$age %in% c(4,5))] <-2
data_CAN$age[which(data_CAN$age %in% c(6,7))] <-3
table(data_CAN$age)
#collapse pathway
data_CAN$pathway <- ''
data_CAN$pathway[grep('I do not know if I have a hearing problem',data_CAN$q0045)] <- 1
data_CAN$pathway[grep('I do not have any problems with my hearing and nobody has complained about my hearing',data_CAN$q0045)] <- 1
data_CAN$pathway[grep('I do not have any problems with my hearing but sometimes other people tell me I have a hearing problem',data_CAN$q0045)] <- 1
data_CAN$pathway[grep('I do have a hearing problem but I',data_CAN$q0045)] <- 2
data_CAN$pathway[grep('I do have a hearing problem and I wear hearing aids or use other technology to help me hear',data_CAN$q0045)] <- 3
table(data_CAN$pathway)
pathway_age <- (with(data_CAN,table(age=age,pathway=pathway)))
pathway_age <- pathway_age[,-1]
rownames(pathway_age) <- c('60-69','70-79','80+')
grid.table(pathway_age)

pathway_age <- (with(data_CAN,table(age=age,pathway=pathway,gender=q0001)))
pathway_age_female <- pathway_age[,,gender=1]
pathway_age_male <- pathway_age[,,gender=2]
pathway_age_male <- pathway_age_male[,-1]
pathway_age_male <- pathway_age_male[-1,]
pathway_age_female <- pathway_age_female[,-1]
pathway_age_female <- pathway_age_female[-1,]

rownames(pathway_age_male) <- c('60-69','70-79','80+')
rownames(pathway_age_female) <- c('60-69','70-79','80+')

grid.table(pathway_age_male)
grid.table(pathway_age_female)




print('load Australia Country Research Now data')
data_AUS = data.frame(read.spss(file.choose()))


data_count <- data.frame(table(dataset['q0046']))
names(data_count) <- c('AUS Pathway Response','# Responses')
grid.table(data_count)
#collapse age
data_AUS$age <- ''
data_AUS$age[grep('60 ',data_AUS$q0003)] <- 2
data_AUS$age[grep('65 ',data_AUS$q0003)] <- 3
data_AUS$age[grep('70 ',data_AUS$q0003)] <- 4
data_AUS$age[grep('75 ',data_AUS$q0003)] <- 5
data_AUS$age[grep('80 ',data_AUS$q0003)] <- 6
data_AUS$age[grep('85 ',data_AUS$q0003)] <- 7
data_AUS$age[which(data_AUS$age %in% c(2,3))] <-1 
data_AUS$age[which(data_AUS$age %in% c(4,5))] <-2
data_AUS$age[which(data_AUS$age %in% c(6,7))] <-3
table(data_AUS$age)
#collapse pathway
data_AUS$pathway <- ''
data_AUS$pathway[grep('I do not know if I have a hearing problem',data_AUS$q0046)] <- 1
data_AUS$pathway[grep('I do not have any problems with my hearing and nobody has complained about my hearing',data_AUS$q0046)] <- 1
data_AUS$pathway[grep('I do not have any problems with my hearing but sometimes other people tell me I have a hearing problem',data_AUS$q0046)] <- 1
data_AUS$pathway[grep('I do have a hearing problem but I',data_AUS$q0046)] <- 2
data_AUS$pathway[grep('I do have a hearing problem and I wear hearing aids or use other technology to help me hear',data_AUS$q0046)] <- 3
table(data_AUS$pathway)

pathway_age <- (with(data_AUS,table(age=age,pathway=pathway,gender=q0002)))
pathway_age_female <- pathway_age[,,gender=1]
pathway_age_male <- pathway_age[,,gender=2]
pathway_age_male <- pathway_age_male[,-1]
pathway_age_male <- pathway_age_male[-1,]
pathway_age_female <- pathway_age_female[,-1]
pathway_age_female <- pathway_age_female[-1,]
rownames(pathway_age_male) <- c('60-69','70-79','80+')
rownames(pathway_age_female) <- c('60-69','70-79','80+')

grid.table(pathway_age_male)
grid.table(pathway_age_female)

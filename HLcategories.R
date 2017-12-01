raw.data<-read.csv("~/Desktop/US0002-US0484_Survey+HTdata_11.16.17.csv", header=TRUE)
head(data)
View(data)
plot(RPTA~LPTA, data=data)
View(mtcars)
plot(cyl~mpg, data=mtcars)
plot(data$LPTA,data$RPTA)
ncol(raw.data)

aud.data<- raw.data[, 1000:1016]


#rtpta <-- data$RPTA #right pure tone average of all ppl
#ltpta <-- data$LPTA #left pure tone average of all ppl
#rm(rtpta=NA)
#rm(rtpta = apropos("NA"))
#crpta <-- na.omit(rtpta)
#clpta <-- na.omit(ltpta)
#rnormal <-- crpta[crpta > -11 & crpta < 16]
#rslight <-- crpta[crpta > 15 & crpta < 26]
#rmild <--- crpta[crpta > 25 & crpta < 41]
#rmoderate <--- crpta[crpta > 40 & crpta < 56]
#lnormal <-- clpta[clpta > -11 & clpta < 16]
#lslight <-- clpta[clpta > 15 & clpta < 26]
#lmild <-- clpta[clpta > 25 & clpta < 41]
#lmoderate <-- clpta[clpta > 40 & clpta < 56]

colnames(aud.data)
rowMeans(aud.data, na.rm = TRUE)

sapply(aud.data, class)
sapply(aud.data, is.factor)
levels(aud.data$R4000)[levels(aud.data$R4000)=="90NR"] <- 9999 #change 90NR to 9999
levels(aud.data$R8000)[levels(aud.data$R8000)=="90NR"] <- 9999
levels(aud.data$L8000)[levels(aud.data$L8000)=="90NR"] <- 9999
sapply(aud.data, class)
aud.data$R4000 <- as.numeric(as.character(aud.data$R4000)) 
aud.data$R8000 <- as.numeric(as.character(aud.data$R8000))
aud.data$L8000 <- as.numeric(as.character(aud.data$L8000)) #rid of factor 
sapply(aud.data, class)

aud.data$FRPTA <- rowMeans(aud.data[c('R500', 'R1000', 'R2000', 'R4000')], na.rm=TRUE)
aud.data$FLPTA <- rowMeans(aud.data[c('L500', 'L1000', 'L2000', 'L4000')], na.rm=TRUE)



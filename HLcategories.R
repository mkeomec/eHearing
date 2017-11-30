data<-read.csv("~/Desktop/US0002-US0484_Survey+HTdata_11.16.17.csv", header=TRUE)
head(data)
View(data)
plot(RPTA~LPTA, data=data)
View(mtcars)
plot(cyl~mpg, data=mtcars)
plot(data$LPTA,data$RPTA)
rtpta <-- data$RPTA #right pure tone average of all ppl
ltpta <-- data$LPTA #left pure tone average of all ppl
rm(rtpta=NA)
rm(rtpta = apropos("NA"))
crpta <-- na.omit(rtpta)
clpta <-- na.omit(ltpta)
rnormal <-- crpta[crpta > -11 & crpta < 16]
rslight <-- crpta[crpta > 15 & crpta < 26]
rmild <--- crpta[crpta > 25 & crpta < 41]
rmoderate <--- crpta[crpta > 40 & crpta < 56]
lnormal <-- clpta[clpta > -11 & clpta < 16]
lslight <-- clpta[clpta > 15 & clpta < 26]
lmild <-- clpta[clpta > 25 & clpta < 41]
lmoderate <-- clpta[clpta > 40 & clpta < 56]

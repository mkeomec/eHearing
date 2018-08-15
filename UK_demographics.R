#UKdemographics
library(gridExtra)


region<-c('East','EastMidlands','London','NorthEast','NorthWest','SouthWest','SouthEast','Wales','WestMidlands','YorkshireandTheHumber')

male_ENG_age50_64<-c(523273,410351,474543,248110,634155,499414,766327,287890,475946,468451)
male_ENG_age65_74<-c(252562,196562,190042,113282,295693,255467,358474,144545,236278,217013)
male_ENG_age75_84<-c(152240,110032,116326,65890,165665,152461,215833,81273,133117,123836)
male_ENG_age85<-c(46934,32780,36688,17568,46130,50036,69735,23618,37956,35910)

female_ENG_age50_64<-c(536520,412236,482775,256112,638950,519497,779554,297480,476329,471993)
female_ENG_age65_74<-c(268370,206197,208091,124085,319454,272770,388024,153655,250039,236690)
female_ENG_age75_84<-c(191009,139296,151073,87797,221346,193477,275984,104692,171212,165560)
female_ENG_age85<-c(93815,66582,76078,37537,100558,102589,144701,50407,81990,76485)

male_ASIAN_age50_64<-c(5194,11023,42025,1033,10298,1448,8799,912,16565,8922)
male_ASIAN_age65_74<-c(1926,3763,14894,338,3399,428,3285,342,6409,3084)
male_ASIAN_age75_84<-c(1094,1981,8769,253,1814,214,1578,137,4254,2063)
male_ASIAN_age85<-c(187,462,1511,30,283,37,274,18,909,332)

female_ASIAN_age50_64<-c(6393,12716,47958,1350,12347,1367,10163,1024,22543,11673)
female_ASIAN_age65_74<-c(2331,5012,19644,457,4028,507,3778,369,8925,3848)
female_ASIAN_age75_84<-c(1318,2822,10665,295,2123,247,2006,146,5362,2424)
female_ASIAN_age85<-c(253,735,2508,38,377,51,416,33,1261,456)

#Createdataframesseperatedbyage
male_ENG<-data.frame(region,male_ENG_age50_64,male_ENG_age65_74,male_ENG_age75_84,male_ENG_age85)
female_ENG<-data.frame(region,female_ENG_age50_64,female_ENG_age65_74,female_ENG_age75_84,female_ENG_age85)
male_ASIAN<-data.frame(region,male_ASIAN_age50_64,male_ASIAN_age65_74,male_ASIAN_age75_84,male_ASIAN_age85)
female_ASIAN<-data.frame(region,female_ASIAN_age50_64,female_ASIAN_age65_74,female_ASIAN_age75_84,female_ASIAN_age85)

#totalnumberofpeopleineachdataframe
male_ENG_total<-colSums(data.frame(colSums(male_ENG[-1])))
female_ENG_total<-colSums(data.frame(colSums(female_ENG[-1])))
male_ASIAN_total<-colSums(data.frame(colSums(male_ASIAN[-1])))
female_ASIAN_total<-colSums(data.frame(colSums(female_ASIAN[-1])))

total<-male_ENG_total+female_ENG_total+male_ASIAN_total+female_ASIAN_total

#Calculateasamplefor1000people
male_ENG_percent<-data.frame(cbind(region,male_ENG[-1]/total*1000))
female_ENG_percent<-data.frame(cbind(region,female_ENG[-1]/total*1000))
male_ASIAN_percent<-data.frame(cbind(region,male_ASIAN[-1]/total*1000))
female_ASIAN_percent<-data.frame(cbind(region,female_ASIAN[-1]/total*1000))

grid.table(male_ENG_percent)
grid.table(female_ENG_percent)
grid.table(male_ASIAN_percent)
grid.table(female_ASIAN_percent)


### UK dataset

UK <- c(3808000,3017500,2462800,2006000,2890900)
England <- c(3172277,2508154,2044129,1669345,2438901)
Wales <- c(204885,166007,134543,108202,153792)
Scotland <- c(336500,261200,220600,178100,230400)
NIreland <- c(94290,82121,63479,50358,67762)

#Collapse age categories

UK_age_collapse <- c(UK[1]+UK[2],UK[3]+UK[4],UK[5])
England_age_collapse <- data.frame(t(c(England[1]+England[2],England[3]+England[4],England[5])))
Wales_age_collapse <- data.frame(t(c(Wales[1]+Wales[2],Wales[3]+Wales[4],Wales[5])))
Scotland_age_collapse <- data.frame(t(c(Scotland[1]+Scotland[2],Scotland[3]+Scotland[4],Scotland[5])))
NIreland_age_collapse <- data.frame(t(c(NIreland[1]+NIreland[2],NIreland[3]+NIreland[4],NIreland[5])))



UK_age_collapse_1000 <- data.frame(t(UK_age_collapse/sum(UK_age_collapse)*1000))


age_cat <- c('60-69','70-79','80+')
names(UK_age_collapse_1000) <- age_cat
rownames(UK_age_collapse_1000) <- 'UK'
names(England_age_collapse) <- age_cat
rownames(England_age_collapse) <- 'England'
names(Wales_age_collapse) <- age_cat
rownames(Wales_age_collapse) <- 'Wales'
names(Scotland_age_collapse) <- age_cat
rownames(Scotland_age_collapse) <- 'Scotland'
names(NIreland_age_collapse) <- age_cat
rownames(NIreland_age_collapse) <- 'NIreland'
Country <- rbind(England_age_collapse,Wales_age_collapse,Scotland_age_collapse,NIreland_age_collapse)
Country_1000 <- Country/sum(Country)*1000

grid.table(UK_age_collapse_1000)
grid.table(Country_1000)

### England

Northeast <- c(168576,128511,110731,89861,120382)
Northwest <- c(439644,342240,285502,227350,316063)
Yorkshire <- c(325329,252608,211241,170823,239899)
EastMidlands <- c(291401,232160,182553,146526,211709)
WestMidlands <- c(335265,279804,225885,181119,259167)
East <- c(372566,294329,237064,198778,294011)
London <- c(342590,256772,216286,176831,254860)
Southeast <- c(535399,424290,339405,282391,435934)
Southwest <- c(361507,297440,235462,195666,306876)

Northeast_age_collapse <- data.frame(t(c(Northeast[1]+Northeast[2],Northeast[3]+Northeast[4],Northeast[5])))
Northwest_age_collapse <- data.frame(t(c(Northwest[1]+Northwest[2],Northwest[3]+Northwest[4],Northwest[5])))
Yorkshire_age_collapse <- data.frame(t(c(Yorkshire[1]+Yorkshire[2],Yorkshire[3]+Yorkshire[4],Yorkshire[5])))
EastMidlands_age_collapse <- data.frame(t(c(EastMidlands[1]+EastMidlands[2],EastMidlands[3]+EastMidlands[4],EastMidlands[5])))
WestMidlands_age_collapse <- data.frame(t(c(WestMidlands[1]+WestMidlands[2],WestMidlands[3]+WestMidlands[4],WestMidlands[5])))
East_age_collapse <- data.frame(t(c(East[1]+East[2],East[3]+East[4],East[5])))
London_age_collapse <- data.frame(t(c(London[1]+London[2],London[3]+London[4],London[5])))
Southeast_age_collapse <- data.frame(t(c(Southeast[1]+Southeast[2],Southeast[3]+Southeast[4],Southeast[5])))
Southwest_age_collapse <- data.frame(t(c(Southwest[1]+Southwest[2],Southwest[3]+Southwest[4],Southwest[5])))

England_region <- rbind(Northeast_age_collapse,Northwest_age_collapse,Yorkshire_age_collapse,EastMidlands_age_collapse,WestMidlands_age_collapse,East_age_collapse,Southeast_age_collapse,Southwest_age_collapse,London_age_collapse)
names(England_region) <- age_cat
rownames(England_region) <- c('Northeast','Northwest','Yorkshire','EastMidlands','WestMidlands','East','Southeast','Southwest','London')
England_region_1000 <- England_region/sum(England_region)*1000
grid.table(England_region_1000)


### London
  
Westminster <- c(9205,7190,6017,4786,6521)
Inner_London <- c(110445,81781,69435,53331,73032)-Westminster  
Other <- c(232145,174991,146851,123500,181828)
Westminster_age_collapse <- data.frame(t(c(Westminster[1]+Westminster[2],Westminster[3]+Westminster[4],Westminster[5])))
Inner_London_age_collapse <- data.frame(t(c(Inner_London[1]+Inner_London[2],Inner_London[3]+Inner_London[4],Inner_London[5])))
Other_age_collapse <- data.frame(t(c(Other[1]+Other[2],Other[3]+Other[4],Other[5])))
London_region <- rbind(Westminster_age_collapse,Inner_London_age_collapse,Other_age_collapse)
names(London_region) <- age_cat
rownames(London_region) <- c('Westminster','London','Other')
London_region_1000 <- London_region/sum(London_region)*1000
grid.table(London_region_1000)



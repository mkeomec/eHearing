library(foreign)

AUS_data<- read.spss("C:/Users/cwbishop/Documents/dropbox/Dropbox/Work/UW BandBlab/Studies/eHearing/Australia eHearing Study - In Person Survey AUS 510 031518.sav", to.data.frame=TRUE)

unique(AUS_data$StartDate)

AUS_data<- read.spss("C:/Users/cwbishop/Documents/dropbox/Dropbox/Work/UW BandBlab/Studies/eHearing/Australia eHearing Study - In Person Survey AUS 510 031518.sav", to.data.frame=TRUE)

UK_RN_data<- read.spss("C:/Users/cwbishop/Documents/dropbox/Dropbox/Work/UW BandBlab/Studies/eHearing/U.K. eHearing Study - Online Panel Survey.sav", to.data.frame=TRUE)

UK_RN_data<- read.spss("C:/Users/cwbishop/Documents/dropbox/Dropbox/Work/UW BandBlab/Studies/eHearing/U.K. eHearing Study - Online Panel Survey.sav", to.data.frame=TRUE)

AUS_RN_data <- read.spss("C:/Users/cwbishop/Documents/dropbox/Dropbox/Work/UW BandBlab/Studies/eHearing/2018 SPSS datasets/eHearing Study-Australia-Panel.sav", to.data.frame=TRUE)

NSW_RN_data <- read.spss("C:/Users/cwbishop/Documents/dropbox/Dropbox/Work/UW BandBlab/Studies/eHearing/2018 SPSS datasets/eHearing Study - NSW - Panel.sav", to.data.frame=TRUE)

CAN_RN_data<- read.spss("C:/Users/cwbishop/Documents/dropbox/Dropbox/Work/UW BandBlab/Studies/eHearing/2018 SPSS datasets/eHearing Study - CanadaPanel.sav", to.data.frame=TRUE)

CAN_state_RN_data<- read.spss("C:/Users/cwbishop/Documents/dropbox/Dropbox/Work/UW BandBlab/Studies/eHearing/2018 SPSS datasets/eHearing Study of Alberta_BC_Quebec_Other_Canada_Panel - 2.sav", to.data.frame=TRUE)

US_WA_data <- read.spss("C:/Users/cwbishop/Documents/dropbox/Dropbox/Work/UW BandBlab/Studies/eHearing/2018 SPSS datasets/eHearing Study - USA WA - Panel.sav", to.data.frame=TRUE)

England_data <- read.spss("C:/Users/cwbishop/Documents/dropbox/Dropbox/Kelly and Michael/England eHearing Study - Online Panel Survey.sav", to.data.frame=TRUE)

London_data <- read.spss("C:/Users/cwbishop/Documents/dropbox/Dropbox/Kelly and Michael/Greater London eHearing Study - Online Panel Survey.sav", to.data.frame=TRUE)

UK_data <- read.spss("C:/Users/cwbishop/Documents/dropbox/Dropbox/Kelly and Michael/U.K. eHearing Study - Online Panel Survey.sav", to.data.frame=TRUE)

seattle_data <- read.spss("C:/Users/cwbishop/Documents/dropbox/Dropbox/Work/UW BandBlab/Studies/eHearing/2018 SPSS datasets/eHearing Study - Seattle DMA - Panel.sav", to.data.frame=TRUE)

AUS_demo_data <- read.spss("C:/Users/cwbishop/Documents/dropbox/Dropbox/Work/UW BandBlab/Studies/eHearing/2018 SPSS datasets/Australia Panel Demos plus Q41 and Q45.spv", to.data.frame=TRUE)

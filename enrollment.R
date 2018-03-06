library(plyr)
library(gridExtra)
library(grid)
library(Hmisc)

## READ IN MERGED DATA
setwd('E:/Lab/Project AF/2018 SPSS Data Sets')
filename <- file.choose()
df_comb <- read.csv(filename,  header=TRUE)
basename(filename)
         
total.table <- table(df_comb$country_id)
testing.table <- with(df_comb,table(testing,country_id))

race.table<- with(df_comb,table(q0014,country_id))
race.table<- with(df_comb,table(q0014,country_id,testing))
country.table <- with(df_comb,table(q0015,country_id))

breakdown.table <- with(df_comb,table(q0002,q0015,country_id))

breakdown.table

grid.table(total.table)
grid.table(testing.table)
grid.table(race.table)
grid.table(country.table)

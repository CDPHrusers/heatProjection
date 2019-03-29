rm(list = ls())
setwd('R:/data/')
getwd()
library(data.table)
library(tidyverse)
library(parallel)
bobbCodes <- c(55L, 157L, 159L, 244L, 108L, 2L)

file <- "/processed/ED_6CCScodes.csv"

ED<-fread(paste(file))
head(ED)
sort(table(ED$ccs_dx_prin))
sort(table(ED$ccs_odx1))
sort(table(ED$ccs_odx2))

class(ED)
class(ED$ccs_dx_prin)
ED$ccs_dx_prin<-as.numeric(ED$ccs_dx_prin)
setkey(ED, ccs_dx_prin)
sub<-ED[.(bobbCodes)]

sort(table(sub$ccs_dx_prin))
# cols<-c(1,7,8,11,19)
# sub[cols]<-lapply(sub[cols],factor)

##turn character vars into factors
sub$dx_prin<-as.factor(sub$dx_prin)
sub$pat_type<-as.factor(sub$pat_type)
sub$lic_type<-as.factor(sub$lic_type)
sub$sex<-as.factor(sub$sex)
sub$payer<-as.factor(sub$payer)

##It doesn't work to save rds-> need special permission to add to folder??
saveRDS(sub, "sub.ED.factors.rds")

##summary stats
##55=Fluid and electrolyte disorders 
##157=Acute and unspecified renal failure 
##159=Urinary tract infections 
##244=Other injuries and conditions due to external causes
##108=Congestive heart failure; nonhypertensive
##2=Septicemia (except in labor)
summary(sub)
table(sub$ccs_dx_prin)
table(sub$serv_y)
table(sub$serv_y,sub$ccs_dx_prin)
##race groups
##0=White
##1=Black
##2=Hispanic
##3=Asian/Pacific islander
##4=American Indian/Alaska Native
##5=Other
##6=Unknown
table(sub$race_grp)
table(sub$serv_y,sub$race_grp)
table(sub$serv_y,sub$race_grp,sub$ccs_dx_prin)

race.ccs<-sub[, .N, by=.(ccs_dx_prin, serv_y, race_grp)]
##collapse sub data table into sum of ccs counts by year, day, zipcode
rm(list = ls())
##setwd('/mnt/projects/ohe/data/')
setwd('R:/heatProjections')
getwd()
library(data.table)
library(tidyverse)
library(parallel)

##Jason's functions

##define Bobb CCS codes of interest which will be filtered by later on
##55=Fluid and electrolyte disorders 
##157=Acute and unspecified renal failure 
##159=Urinary tract infections 
##244=Other injuries and conditions due to external causes
##108=Congestive heart failure; nonhypertensive
##2=Septicemia (except in labor)
bobbCodes <- c(55L, 157L, 159L, 244L, 108L, 2L)

##create function to read in and filter the hospital data based on Bobb codes of interest
filterED <- function(year) {
  file<- paste0("ED/cdph_ed_rln",year,".csv")
  foo <- fread(paste(file))
  
 ##set key to primary, secondary, and tertiary ccs diagnosis columns
  setkey(foo, ccs_dx_prin, ccs_odx1, ccs_odx2)
 ##pull out all cases that have the designated Bobb codes in either the primary, secondary or tertiary diagnosis columns
 ##bring in patient's zipcode, county code, and date of service
  out <-
    foo [ccs_dx_prin %in% bobbCodes |
           ccs_odx1 %in% bobbCodes |
           ccs_odx2 %in% bobbCodes, c(
             "dx_prin",
             "ec_prin",
             "ccs_dx_prin",
             "ccs_odx1",
             "ccs_odx2" ,
             "fac_id",
             "pat_type",
             "lic_type",
             "agdyserv",
             "agyrserv",
             "sex",
             "patzip",
             "patco",
           # "serv_q",
           # "serv_d",
           # "serv_m",
           # "serv_y",
             "dispn",
             "payer",
             "pr_prin",
             "opr1",
             "serv_dt",
             "brthdate",
           # "dob_raw",
             "faczip",
             "fac_co",
             "rln",
             "race_grp"
           )] %>%
  ##count the number of cases by patient zip code and by date
  .[,n :=.n, by=.(patzip, serv_dt)]%>%
  .[, Date:= as.Date(serv_dt, format = "%m/%d/%Y")]
  
  return(out)
  
}

##create function to read in the PRISM temp data for each year of interest
getPRISM <- function(year){
  #year<-2008
  file<- paste0("PRISM/prism_",year,".csv")
  foo <- fread(paste(file))
 #colnames(foo)[1]<-"index"
 
  foo$Date <-as.Date(substr(foo$`system:index`, start = 1,stop = 8), format = "%Y%m%d")
  
  return(foo)
  
}

##create function that combines the hospital data and the temperature data into one datatable
EDtempcombine <- function(year) {
   #ED <- filterED(2008) 
   #temp <- getPRISM(2008)

   ED <- filterED(year) 
   temp <- getPRISM(year)


##create aggregated data table for zip and day combos-using facility zip due to missingness in patient zip
#ED.agg<- ED %>% 
 # group_by(faczip, serv_dt) %>%
  # summarize(n=n()) 
# ED.agg$Date<-as.Date(ED.agg$serv_dt, format = "%m/%d/%Y")

#merge the ED data with the temp data by patient zipcode and date
join<-merge(temp, ED, by.x = c("Date", "ZCTA"), by.y = c("Date", "patzip"), all.x = T) %>% replace_na(list(n = 0))

fwrite(join, paste0("processed/tempAndED/tempAndED_",year,".csv"))

# return(join)
}

# I coulnd't get this to work
# years <- list(2005:2017)
# lapply(years, EDtempcombine)


#######################
# reading in years one by one- they are saved on the server
########################
EDtempcombine(2005)
EDtempcombine(2006)
EDtempcombine(2007)
EDtempcombine(2008)
EDtempcombine(2009)
EDtempcombine(2010)
EDtempcombine(2011)
EDtempcombine(2012)
EDtempcombine(2013)
EDtempcombine(2014)
EDtempcombine(2015)
EDtempcombine(2016)
EDtempcombine(2017)


#####################
# Then combine these years
#####################
all <- rbindlist(lapply(list.files(path = "processed/tempAndED/", full.names = T), fread), fill = T)


# and save
fwrite(all, "processed/tempAndED_allYears.csv")


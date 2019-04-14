rm(list = ls())
##setwd('/mnt/projects/ohe/data/')
setwd('R:/heatProjections')
getwd()
library(data.table)
library(tidyverse)
library(parallel)

file <- "data/processed/ED_6CCScodes.csv"

ED<-fread(paste(file))
head(ED)
##data table to collapse by zip+date combo (sum number of obs in each unique zip+date combo)
ED.sub<-ED[,c('ccs_dx_prin','ccs_odx1','ccs_odx2','patzip','faczip','serv_dt','patco')]
table(ED.sub$patzip)
summary(ED.sub$patzip)
summary(ED.sub$faczip)

##create aggregated data table for zip and day combos-using facility zip due to missingness in patient zip
ED.agg<- ED.sub %>% 
  group_by(faczip, serv_dt) %>%
  summarize(n=n())

summary(ED.agg$n)
table(ED.agg$n)


##Jason's functions
bobbCodes <- c(55L, 157L, 159L, 244L, 108L, 2L)

filterED <- function(year) {
  file<- paste0("ED/cdph_ed_rln",year,".csv")
  foo <- fread(paste(file))
  
  setkey(foo, ccs_dx_prin, ccs_odx1, ccs_odx2)
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
             "serv_q",
             "serv_d",
             "serv_m",
             "serv_y",
             "dispn",
             "payer",
             "pr_prin",
             "opr1",
             "serv_dt",
             "brthdate",
             "dob_raw",
             "faczip",
             "fac_co",
             "rln",
             "race_grp"
           )]
  
  return(out)
  
}

getPRISM <- function(year){
  #year<-2008
  file<- paste0("PRISM/prism_",year,".csv")
  foo <- fread(paste(file))
 #colnames(foo)[1]<-"index"
 
  foo$Date <-as.Date(substr(foo$`system:index`, start = 1,stop = 8), format = "%Y%m%d")
  
  return(foo)
  
}

EDtempcombine <- function(year) {
#ED <- filterED(2008) 
#temp <- getPRISM(2008)

  ED <- filterED(year) 
temp <- getPRISM(year)
ED.sub<-ED[,c('ccs_dx_prin','ccs_odx1','ccs_odx2','patzip','faczip','serv_dt')]
summary(ED.sub$patzip)
summary(ED.sub$faczip)

##create aggregated data table for zip and day combos-using facility zip due to missingness in patient zip
ED.agg<- ED.sub %>% 
  group_by(faczip, serv_dt) %>%
  summarize(n=n()) 
ED.agg$Date<-as.Date(ED.agg$serv_dt, format = "%m/%d/%Y")

#merge the ED data with the temp data by zipcode and date
join<-merge(temp, ED.agg, by.x = c("Date", "ZCTA"), by.y = c("Date", "faczip"), all.x = T) %>% replace_na(list(n = 0))

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

test %>% ggplot() + geom_point(aes(x = tmean_mean, y = n, color = Date, alpha = 0.2)) 
test %>% ggplot() + geom_boxplot(aes(x = Date, y = tmean_mean, group = Date)) + geom_bar(aes(x= Date, y= n/100, fill = "red", alpha = 0.3), stat="identity")


##bring in all file
file <- "data/processed/tempAndED_allYears.csv"
file<-"processed/tempAndED/tempAndED_2015.csv"

all<-fread(paste(file))
y2015<-fread(paste(file))

summary(all)
##33,145 missing NA is a commomn occurrence, 6 million NA's for Day-> will figure out why...

summary(y2015)

##find the zipcodes that are missing temp data: 
y2015 %>% filter(is.na(tmax_max)) %>% count(ZCTA)
##90079: Downtown LA, 90747:Carson/Long Beach , 91046:Glendale (Burbank) ,92132:Downtown San Diego , 93606:Biola , 94575:Moraga , 95486:Villa Grande (Sonoma) 
364*7 ##2548
##find the zipcodes that are missing temp data: 
all %>% filter(is.na(tmax_max)) %>% count(ZCTA)
4735*7 ##33145

##The same zipcodes from 1 year and from the all dataframe are missing temp data. 
missing.zips<-c(90079,90747,91046,92132,93606,94575,95486)
missing<-all %>% filter(ZCTA %in% missing.zips)
missing %>% count(n)
missing %>% summary()
##No bobbcode cases for zips with missing temp data -> might be justification for leaving out these zips?

##subset to three test counties: Bakersfield, Lone Pine and Tahoe: 93301, 93545, 96150
sub<-c("93301", "93545", "96150")
sub.df<-all[ZCTA %in% sub,]
unique(sub.df$ZCTA)
saveRDS(sub.df, "data/dlnm_test_zips.rds")

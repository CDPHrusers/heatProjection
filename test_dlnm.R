rm(list = ls())
setwd('M:/')
getwd()
#install.packages("tidyverse")
install.packages("dplyr")
library(dplyr)
library(data.table)
#library(tidyverse)
library(parallel)


# LOAD THE PACKAGES
install.packages(c("dlnm", "mvmeta","splines","tsModel"))
library(dlnm) ; library(mvmeta) ; library(splines) ; library(tsModel)

# CHECK VERSION OF THE PACKAGE
if(packageVersion("dlnm")<"2.2.0")
  stop("update dlnm package to version >= 2.2.0")

# LOAD THE DATASET (INCLUDING THE 10 UK REGIONS ONLY)
##bring in all file
file <-"data/dlnm_test_zips.rds"
zipCA <-readRDS(file)
zipCA$Date <- as.Date(zipCA$Date)
zipCA$zipnames <- ifelse (zipCA$ZCTA == 93301, "Bakersfield",
                         ifelse( zipCA$ZCTA == 93545, "LonePine", "Tahoe"))
  

#regEngWales <- read.csv("regEngWales.csv",row.names=1)
#regEngWales$date <- as.Date(regEngWales$date)


# ARRANGE THE DATA AS A LIST OF DATA SETS
regions <- as.character(unique(zipCA$zipname))
dlist <- lapply(regions,function(x) zipCA[zipCA$zipnames==x,])
names(dlist) <- regions

# METADATA FOR LOCATIONS
cities <- data.frame(
  city = regions,
  cityname = c("Bakersfield", "LonePine", "Tahoe")
)

# ORDER
ord <- order(cities$cityname)
dlist <- dlist[ord]
cities <- cities[ord,]

# REMOVE ORIGINALS
rm(zipCA,regions,ord)

################################################################################

# SPECIFICATION OF THE EXPOSURE FUNCTION
varfun = "bs"
vardegree = 2
varper <- c(10,75,90)

# SPECIFICATION OF THE LAG FUNCTION
lag <- 21
lagnk <- 3

# DEGREE OF FREEDOM FOR SEASONALITY-> knots per year
dfseas <- 8
year<- 13
# COMPUTE PERCENTILES
per <- t(sapply(dlist,function(x) 
  quantile(x$tmean_mean,c(2.5,10,25,50,75,90,97.5)/100,na.rm=T)))

# MODEL FORMULA dow=day of week, cb= crossbasis ns=function for natural spline?
formula <- n~cb+ns(Date,df=dfseas*length(unique(year)))



# COEFFICIENTS AND VCOV FOR OVERALL CUMULATIVE SUMMARY
coef <- matrix(NA,nrow(cities),length(varper)+vardegree,
               dimnames=list(cities$city))
vcov <- vector("list",nrow(cities))
names(vcov) <- cities$city

################################################################################
# RUN THE LOOP

# LOOP
time <- proc.time()[3]
for(i in seq(length(dlist))) {
  # PRINT
  cat(i,"")
  
  # EXTRACT THE DATA
  data <- dlist[[i]]
  
  # DEFINE THE CROSSBASIS
  argvar <- list(fun=varfun,knots=quantile(data$tmean_mean,varper/100,na.rm=T),
                 degree=vardegree)
  cb <- dlnm::crossbasis(data$tmean_mean,lag=lag,argvar=argvar,
                   arglag=list(knots=logknots(lag,lagnk)))
  #summary(cb)
  
  # RUN THE MODEL AND OBTAIN PREDICTIONS
  # NB: NO CENTERING NEEDED HERE, AS THIS DOES NOT AFFECT COEF-VCOV
  model <- glm(formula,data,family=quasipoisson,na.action="na.exclude")
  cen <- mean(data$tmean_mean,na.rm=T)
  pred <- crosspred(cb,model,cen=cen)
  
  # REDUCTION TO OVERALL CUMULATIVE
  red <- crossreduce(cb,model,cen=cen)
 
  coef[i,] <- coef(red)
  vcov[[i]] <- vcov(red)
  
}
proc.time()[3]-time

#
coef[1]
vcov[1]

data.frame(temp = as.numeric(names(red$RRfit)), RR = red$RRfit, LL = red$RRlow, UL = red$RRhigh)%>%
ggplot() + geom_line(aes(x = temp, y=RR)) + geom_line(aes(x = temp, y=LL), color= "red")+ geom_line(aes(x = temp, y=UL), color="blue")



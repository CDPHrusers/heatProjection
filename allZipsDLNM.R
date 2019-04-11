rm(list = ls())
# setwd('/mnt/projects/ohe/heatProjections/')
# setwd('/heatProjections/')
setwd('M:/')
getwd()
library(data.table)
library(tidyverse)
library(parallel)
# LOAD THE PACKAGES
#install.packages(c("dlnm", "mvmeta","splines","tsModel"))
library(dlnm) ; library(mvmeta) ; library(splines) ; library(tsModel)

# CHECK VERSION OF THE PACKAGE
if(packageVersion("dlnm")<"2.2.0")
  stop("update dlnm package to version >= 2.2.0")


# LOAD THE DATASET (CA zip temps and ED counts by day)
allZips <- fread("data/processed/tempAndED_allYears.csv")
setkey(allZips, ZCTA)

# restrict data to only zipcodes with cases -> I am second guessing this
zipsWithCases <- allZips[, .(total = sum(n, na.rm=T)), by= "ZCTA"] %>% .[total>0, "ZCTA"]
##Only 320 out of ~2000 zip codes in CA have cases?

##define data for model to run on
zipCA <- allZips[.(zipsWithCases)] 

zipCA$Date <- as.Date(zipCA$Date)


# ARRANGE THE DATA AS A LIST OF DATA SETS
regions <- as.character(unique(zipCA$ZCTA))
dlist <- lapply(regions,function(x) zipCA[zipCA$ZCTA==x,])
names(dlist) <- regions

# METADATA FOR LOCATIONS
cities <- data.frame(
  city = regions,
  cityname = regions
)

# ORDER
ord <- order(cities$cityname)
dlist <- dlist[ord]
cities <- cities[ord,]
# these are indeces for the zipcodes that do not converge, as determined by running the model previously
non.converge<-c(10, 13, 22, 40, 89, 111, 121, 123, 129, 139, 145, 180, 181, 183, 189, 207, 219, 228, 243, 265, 310)
dlist.non<-dlist[non.converge]
dlist.non.names<-names(dlist.non)

# count the number of cases in each zipcode and create new datatable
sum.n<-zipCA %>% group_by(ZCTA) %>% summarise(sum_n= sum(n))
# create column that defines if the zipcode converged or not
sum.n$converge<- ifelse(sum.n$ZCTA %in% dlist.non.names, "n","y")
                
# remove the zipcodes that did not converge
dlist<-dlist[-c(10, 13, 22, 40, 89, 111, 121, 123, 129, 139, 145, 180, 181, 183, 189, 207, 219, 228, 243, 265, 310)]
                
# remove the zipcodes that did not converge
cities<-cities[-c(10, 13, 22, 40, 89, 111, 121, 123, 129, 139, 145, 180, 181, 183, 189, 207, 219, 228, 243, 265, 310),]
                
# REMOVE ORIGINALS
rm(zipCA,regions,ord)

################################################################################
# The following specifications are those defined by Antonnio Gasparrini in his 2015 Lancet paper
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
    i <- 278
  # PRINT
  cat(i,"")
 
  # EXTRACT THE DATA
  data <- dlist[[i]]
 
  # DEFINE THE CROSSBASIS
  argvar <- list(fun=varfun,knots=quantile(data$tmean_mean,varper/100,na.rm=T),
                 degree=vardegree)
  cb <- dlnm::crossbasis(data$tmean_mean,lag=lag,argvar=argvar,
                   arglag=list(knots=logknots(lag,lagnk)))
  
  # RUN THE MODEL AND OBTAIN PREDICTIONS
  # NB: NO CENTERING NEEDED HERE, AS THIS DOES NOT AFFECT COEF-VCOV
  model <- glm(formula,data,family=quasipoisson,na.action="na.exclude")
  options(warn=1)
  
  #centered on average temperature
  cen <- mean(data$tmean_mean,na.rm=T)
  pred <- crosspred(cb,model,cen=cen, by=1)

  # REDUCTION TO OVERALL CUMULATIVE (default)
  red <- crossreduce(cb,model,cen=cen)
 
  coef[i,] <- coef(red)
  vcov[[i]] <- vcov(red)
  
}
proc.time()[3]-time
# I ran the model originally and these were the zipcodes that did not converge -> these are the ones taken out at the beginning of script
# in dlist: 10, 13, 22, 40, 89, 111, 121, 123, 129, 139, 145, 180, 181, 183, 189, 207, 219, 228, 243, 265, 310 did not converge
coef[1]
vcov[1]



##So this is a plot looking at the fit for just one zipcode correct? the last zipcode -> a zip in LA. dlist[278] is Chico and looks more like
## Antonio's plots.
data.frame(temp = as.numeric(names(red$RRfit)), RR = red$RRfit, LL = red$RRlow, UL = red$RRhigh)%>%
ggplot() + geom_line(aes(x = temp, y=RR)) + geom_line(aes(x = temp, y=LL), color= "red")+ geom_line(aes(x = temp, y=UL), color="blue")

plot(pred, "slices", var=35, col=3, ylab="RR", ci.arg=list(density=21,lwd=2),
     main="Association with a 1-unit increase in temp")
plot(pred, xlab="Temperature", zlab="RR", theta=200, phi=40, lphi=30,
     main="3D graph of temperature effect")
plot(pred, "contour", xlab="Temperature", key.title=title("RR"),
     plot.title=title("Contour plot",xlab="Temperature",ylab="Lag"))

####
##SECOND STAGE##
avgtmean <- sapply(dlist,function(x) mean(x$tmean_mean,na.rm=T))
rangetmean <- sapply(dlist,function(x) diff(range(x$tmean_mean,na.rm=T)))
################################################################################
# META-ANALYSIS
# NB: COUNTRY EFFECT IS NOT INCLUDED IN THIS EXAMPLE
## location-specific average temp and range included as potential effect modifiers
mv <- mvmeta(coef~avgtmean+rangetmean,vcov,data=cities,control=list(showiter=T))
summary(mv)
# NB: IN THIS EXAMPLE THE MV-META MODEL IS CLEARLY OVERPARAMETERIZED. what are the 5 y's?

################################################################################

# FUNCTION FOR COMPUTING THE P-VALUE OF A WALD TEST
fwald <- function(model,var) {
  ind <- grep(var,names(coef(model)))
  coef <- coef(model)[ind]
  vcov <- vcov(model)[ind,ind]
  waldstat <- coef%*%solve(vcov)%*%coef
  df <- length(coef)
  return(1-pchisq(waldstat,df))
}

# TEST THE EFFECTS
fwald(mv,"avgtmean")
fwald(mv,"rangetmean")

################################################################################
# OBTAIN BLUPS BLUP= best linear unbiased prediction-> city specific covariates from meta model (weighted model)

blup <- blup(mv,vcov=T)

################################################################################
# RE-CENTERING

# GENERATE THE MATRIX FOR STORING THE RESULTS
minperccity <- mintempcity <- rep(NA,length(dlist))
names(mintempcity) <- names(minperccity) <- cities$city

# DEFINE MINIMUM MORTALITY VALUES: EXCLUDE LOW AND VERY HOT TEMPERATURE
for(i in seq(length(dlist))) {
  data <- dlist[[i]]
  ##generate percentiles for average temperatures
  predvar <- quantile(data$tmean_mean,1:99/100,na.rm=T)
  # REDEFINE THE FUNCTION USING ALL THE ARGUMENTS (BOUNDARY KNOTS INCLUDED), define the knots and dfs again (same as before)
  argvar <- list(x=predvar,fun=varfun,
                 knots=quantile(data$tmean_mean,varper/100,na.rm=T),degree=vardegree,
                 Bound=range(data$tmean_mean,na.rm=T))
  ##generate the basis matrix for a predictor vector, with parameters defined in argvar (bs, knots, df)
  bvar <- do.call(onebasis,argvar) ##what are the values in this cross bassis?
  
  ##find the optimum temperature for each location based on blups and average temperatures
  minperccity[i] <- (1:99)[which.min((bvar%*%blup[[i]]$blup))] ##crossbassis times coefficients
  ##find the percentile of temps that optimum temperature corresponds to
  mintempcity[i] <- quantile(data$tmean_mean,minperccity[i]/100,na.rm=T)
}

# COUNTRY-SPECIFIC POINTS OF MINIMUM MORTALITY-> the median optimal percentile
(minperccountry <- median(minperccity))




##ATTRIBUTABLE RISK
# LOAD THE FUNCTION FOR COMPUTING THE ATTRIBUTABLE RISK MEASURES
source("code/attrdl.R")

# CREATE THE VECTORS TO STORE THE TOTAL MORTALITY (ACCOUNTING FOR MISSING)
totdeath <- rep(NA,nrow(cities))
names(totdeath) <- cities$city

# CREATE THE MATRIX TO STORE THE ATTRIBUTABLE DEATHS
matsim <- matrix(NA,nrow(cities),3,dimnames=list(cities$city,
                                                 c("glob","cold","heat")))

# NUMBER OF SIMULATION RUNS FOR COMPUTING EMPIRICAL CI
nsim <- 1000

# CREATE THE ARRAY TO STORE THE CI OF ATTRIBUTABLE DEATHS
arraysim <- array(NA,dim=c(nrow(cities),3,nsim),dimnames=list(cities$city,
                                                              c("glob","cold","heat")))

################################################################################

# RUN THE LOOP
for(i in seq(dlist)){
  
  # PRINT
  cat(i,"")
  
  # EXTRACT THE DATA
  data <- dlist[[i]]
  
  # DERIVE THE CROSS-BASIS
  # NB: CENTERING POINT DIFFERENT THAN ORIGINAL CHOICE OF 75TH
  argvar <- list(x=data$tmean_mean,fun=varfun,knots=quantile(data$tmean_mean,
                                                        varper/100,na.rm=T),degree=vardegree)
  cb <- crossbasis(data$tmean_mean,lag=lag,argvar=argvar,
                   arglag=list(knots=logknots(lag,lagnk)))
  
  # COMPUTE THE ATTRIBUTABLE DEATHS
  # NB: THE REDUCED COEFFICIENTS ARE USED HERE
  matsim[i,"glob"] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                             vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i])
  matsim[i,"cold"] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                             vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                             range=c(-100,mintempcity[i]))
  matsim[i,"heat"] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                             vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                             range=c(mintempcity[i],100))
  
  # COMPUTE EMPIRICAL OCCURRENCES OF THE ATTRIBUTABLE DEATHS
  # USED TO DERIVE CONFIDENCE INTERVALS
  arraysim[i,"glob",] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                                vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],sim=T,nsim=nsim)
  arraysim[i,"cold",] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                                vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                                range=c(-100,mintempcity[i]),sim=T,nsim=nsim)
  arraysim[i,"heat",] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                                vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                                range=c(mintempcity[i],100),sim=T,nsim=nsim)
  
  # STORE THE DENOMINATOR OF ATTRIBUTABLE DEATHS, I.E. TOTAL OBSERVED MORTALITY
  # CORRECT DENOMINATOR TO COMPUTE THE ATTRIBUTABLE FRACTION LATER, AS IN attrdl
  totdeath[i] <- sum(data$n,na.rm=T)
}

################################################################################
# ATTRIBUTABLE NUMBERS

# CITY-SPECIFIC
ancity <- matsim
ancitylow <- apply(arraysim,c(1,2),quantile,0.025)
ancityhigh <- apply(arraysim,c(1,2),quantile,0.975)
rownames(ancity) <- rownames(ancitylow) <- rownames(ancityhigh) <- cities$cityname
write.csv(ancity, "attributable_number_zips.csv")

# TOTAL
# NB: FIRST SUM THROUGH CITIES
antot <- colSums(matsim) ##839842.21 ED visits attributed to heat and cold
antotlow <- apply(apply(arraysim,c(2,3),sum),1,quantile,0.025)
antothigh <- apply(apply(arraysim,c(2,3),sum),1,quantile,0.975)
################################################################################
# TOTAL MORTALITY

# BY COUNTRY, the total number of ED visits (empirical)
totdeathtot <- sum(totdeath)
################################################################################
# ATTRIBUTABLE FRACTIONS

# CITY-SPECIFIC
afcity <- ancity/totdeath*100
afcitylow <- ancitylow/totdeath*100
afcityhigh <- ancityhigh/totdeath*100
write.csv(afcity,"attributable_frac_zips.csv")

# TOTAL: 6% from the zips that converged
aftot <- antot/totdeathtot*100
aftotlow <- antotlow/totdeathtot*100
aftothigh <- antothigh/totdeathtot*100


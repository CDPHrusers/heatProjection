
#' first_stage_DLNM
#'
#' This function conducts the first stage of the DLNM analysis described in DOI:https://doi.org/10.1016/S0140-6736(14)62114-0
#' @param file_path The file path to the ED data you wish to process. Defaults to test data on the R server at CDPH
#' @param varfun  Defines the type of spline used in the exposure function. Defaults to a B-spline.
#' @param vardegree  Define the number of degrees of freedom in the type of spline defined by varfun.
#' @param varper Defines that percentile at which to put the knots in the exposure function.
#' @param lag Defines the lag in days.
#' @param lagnk Defines the number of knots in the lag.
#' @param dfseas Defines the degrees of freedom for seasons i.e. number of knots per year. 
#' @param year Defines the number of years you are modeling.

library(data.table)
library(tidyverse)
library(parallel)
library(dlnm) 
library(mvmeta)
library(splines) 
library(tsModel)


#first_stage_DLNM <- function (file_path="./data/processed/combined_test_data.csv", varfun = "bs",
#                              vardegree = 2, varper = c(10,75,90), lag = 3, lagnk = 3, 
#                             dfseas = 8, year = 13){
  
  file_path="./data/processed/combined_test_data.csv"
  varfun = "bs"
  vardegree = 2
  varper = c(10,75,90)
  lag = 4 ##when I change the lag from 3 to 4 days, I no longer get NAs in my model coefficients
  lagnk = 3 
  dfseas = 8
  year = 13

  # LOAD THE DATASET (CA zip temps and ED counts by day)
  zipCA <- fread(paste(file_path))
  setkey(zipCA, ZCTA)
  zipCA[,Date := as.Date(Date)]
  
  # ARRANGE THE DATA AS A LIST OF DATA SETS
  zips <- as.character(unique(zipCA$ZCTA)) #used to be regions
  dlist <- lapply(zips,function(x) zipCA[zipCA$ZCTA==x,])
  names(dlist) <- zips
  
  # METADATA FOR LOCATIONS used to be "cities"
  zips_meta <- data.frame(
    zip = zips,
    zipname = zips
  )
  
  ord <- order(zips_meta$zipname)
  dlist <- dlist[ord]
  zips_meta <- zips_meta[ord,]
  zipCA$year<-substring(zipCA$Date,1,4)
 
  # REMOVE ORIGINALS
  rm(zipCA,zips,ord) 
  
  
  # MODEL FORMULA dow=day of week, cb= crossbasis ns=function for natural spline?
  formula <- n~cb+ns(Date,df=dfseas*length(unique(year)))
  
  
  # COEFFICIENTS AND VCOV FOR OVERALL CUMULATIVE SUMMARY
  coef <- matrix(NA,nrow(zips_meta),length(varper)+vardegree,
                 dimnames=list(zips_meta$zip))
  vcov <- vector("list",nrow(zips_meta))
  names(vcov) <- zips_meta$zip
  
  mod.list <- list()
 
   df<-dlist[[1]]
  argvar <- list(fun=varfun,knots=quantile(df$tmean_mean,varper/100,na.rm=T),
                 degree=vardegree)
  cb <- dlnm::crossbasis(df$tmean_mean,lag=lag,argvar=argvar,
                         arglag=list(knots=logknots(lag,lagnk)))
  
  model <- glm(formula,data=df,family=quasipoisson,na.action="na.exclude")
  
  #time <- proc.time()[3]
  for(i in seq_along(dlist)) {
    # PRINT
    cat(i,"")
   #i<-3
    # EXTRACT THE DATA
    data2 <- dlist[[i]]

    # DEFINE THE CROSSBASIS
    argvar <- list(fun=varfun,knots=quantile(data2$tmean_mean,varper/100,na.rm=T),
                   degree=vardegree)
    cb <- dlnm::crossbasis(data2$tmean_mean,lag=lag,argvar=argvar,
                           arglag=list(knots=logknots(lag,lagnk)))
    
    # RUN THE MODEL AND OBTAIN PREDICTIONS
    # NB: NO CENTERING NEEDED HERE, AS THIS DOES NOT AFFECT COEF-VCOV
    
    
    mod2=try(update(model,data=dlist[[i]]),TRUE)
    if(isTRUE(class(mod2)=="try-error")) { next } else  {mod.list[[i]]=mod2 } 
 
  # model <- glm(formula,data2,family=quasipoisson,na.action="na.exclude")
  options(warn=1)
  
  #centered on average temperature
  cen <- mean(data2$tmean_mean,na.rm=T)
  pred <- crosspred(cb,mod2,cen=cen, by=1)


# REDUCTION TO OVERALL CUMULATIVE (default)
red <- crossreduce(cb,mod2,cen=cen)

coef[i,] <- coef(red)
vcov[[i]] <- vcov(red)

  }
  
proc.time()[3]-time
return(list("coef" = coef, "vcov" = vcov, "zips_meta" = zips_meta, "dlist" = dlist))

#}



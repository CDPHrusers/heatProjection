
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


first_stage_DLNM <- function (file_path="./data/processed/combined_test_data.csv", varfun = "bs",
                              vardegree = 2, varper = c(10,75,90), lag = 3, lagnk = 2, 
                              dfseas = 8, year = 13){
  
  # CHECK VERSION OF THE PACKAGE
  if(packageVersion("dlnm")<"2.2.0")
    stop("update dlnm package to version >= 2.2.0")
  
  # LOAD THE DATASET (CA zip temps and ED counts by day)
  zipCA <- fread(paste(file_path))
  setkey(zipCA, ZCTA)
  # restrict data to only zipcodes with cases -> I am second guessing this
  #zipsWithCases <-  zipCA[, .(total = sum(n, na.rm=T)), by= "ZCTA"] %>% .[total>0, "ZCTA"]
  
  ##define data for model to run on
  #zipCA <- allZips[.(zipsWithCases)] 
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
  # these are indeces for the zipcodes that do not converge, as determined by running the model previously
  #non.converge<-c(10, 13, 22, 40, 89, 111, 121, 123, 129, 139, 145, 180, 181, 183, 189, 207, 219, 228, 243, 265, 310)
  #dlist.non<-dlist[non.converge]
  #dlist.non.names<-names(dlist.non)
 
  # count the number of cases in each zipcode and create new datatable
  #sum.n<-zipCA %>% group_by(ZCTA) %>% summarise(sum_n= sum(n))
  # create column that defines if the zipcode converged or not
  #sum.n$converge<- ifelse(sum.n$ZCTA %in% dlist.non.names, "n","y")
  
  # remove the zipcodes that did not converge
  #dlist<-dlist[-c(10, 13, 22, 40, 89, 111, 121, 123, 129, 139, 145, 180, 181, 183, 189, 207, 219, 228, 243, 265, 310)]
  
  # remove the zipcodes that did not converge
  #zip_met<-zip_met[-c(10, 13, 22, 40, 89, 111, 121, 123, 129, 139, 145, 180, 181, 183, 189, 207, 219, 228, 243, 265, 310),]
  
  # REMOVE ORIGINALS
  rm(zipCA,zips,ord) 
  
  # COMPUTE PERCENTILES this is needed for the plot making later on
   # per <- t(sapply(dlist,function(x) 
   # quantile(x$tmean_mean,c(2.5,10,25,50,75,90,97.5)/100,na.rm=T)))
  
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
  
  ################################################################################
  # RUN THE LOOP
  
  # LOOP
  time <- proc.time()[3]
  for(i in seq(length(dlist))) {
    #i<-2
      # PRINT
    cat(i,"")
    #year<-1
    # EXTRACT THE DATA
    data2 <- dlist[[i]]
    
    # DEFINE THE CROSSBASIS
    argvar <- list(fun=varfun,knots=quantile(data2$tmean_mean,varper/100,na.rm=T),
                   degree=vardegree)
    cb <- dlnm::crossbasis(data2$tmean_mean,lag=lag,argvar=argvar,
                           arglag=list(knots=logknots(lag,lagnk)))
    
    
    mod2=try(update(model,data=dlist[[i]]),TRUE)
    if(is(mod2,"warning")) print(data2$ZCTA[1]) #This doesn't actually do anything - figure out how to print zip when warning occurs
    
    if(isTRUE(class(mod2)=="try-error")) {next} else  {mod.list[[i]]=mod2} 
    
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

}



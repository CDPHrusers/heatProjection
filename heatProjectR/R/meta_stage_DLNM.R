
#' meta_stage_DLNM
#'
#' This function conducts the meta analysis (second stage) of the DLNM analysis described in DOI:https://doi.org/10.1016/S0140-6736(14)62114-0
#' @param first_stage_list
#' @param output_path_num
#' @param output_path_frac

library(data.table)
library(tidyverse)
library(parallel)
library(dlnm) 
library(mvmeta)
library(splines) 
library(tsModel)

meta_stage_DLNM<-function(first_stage_list = first_stage,  output_path_num = "/data/processed/attributable_number_zips.csv",output_path_frac = "/data/processed/attributable_frac_zips.csv", varfun = "bs", vardegree = 2, varper = c(10,75,90), lag = 3, lagnk = 2){
  
  
  dlist <- first_stage_list$dlist
  coef <- first_stage_list$coef
  vcov <- first_stage_list$vcov
  zips_meta <-first_stage_list$zips_meta
  varlist<- as.data.frame(first_stage_list$variance)
 colnames(varlist)<- c("n","temp","zip")
  
  zips_to_remove <- varlist$zip[which(varlist$n < 0.02)]
  
  dlist<-dlist[!names(dlist) %in% zips_to_remove]
  coef<-coef[!rownames(coef) %in% zips_to_remove,]
  vcov<-vcov[!names(vcov) %in% zips_to_remove]
  zips_meta<-filter(zips_meta, !zip %in% zips_to_remove)
  
  
  # CHECK VERSION OF THE PACKAGE
  if(packageVersion("dlnm")<"2.2.0")
    stop("update dlnm package to version >= 2.2.0")
  
##SECOND STAGE##
avgtmean <- sapply(dlist,function(x) mean(x$tmean_mean,na.rm=T))
rangetmean <- sapply(dlist,function(x) diff(range(x$tmean_mean,na.rm=T)))
################################################################################
# META-ANALYSIS
## location-specific average temp and range included as potential effect modifiers
print(length(avgtmean))
print(length(rangetmean))
print(length(vcov))
print("1")

mv <- mvmeta(coef~avgtmean+rangetmean,vcov,data=zips_meta,control=list(showiter=T))
print("2") 

summary(mv)
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
minperczip <- mintempzip <- rep(NA,length(dlist))
names(mintempzip) <- names(minperczip) <- zips_meta$city

# DEFINE MINIMUM ED VISIT VALUES: EXCLUDE LOW AND VERY HOT TEMPERATURE
for(i in seq(length(dlist))) {
  tryCatch({
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
  minperczip[i] <- (1:99)[which.min((bvar%*%blup[[i]]$blup))] ##crossbassis times coefficients
  ##find the percentile of temps that optimum temperature corresponds to
  mintempzip[i] <- quantile(data$tmean_mean,minperczip[i]/100,na.rm=T)
  print("3")
  }, error=function(e){cat("ERROR: ",conditionMessage(e), "\n")})
}

# COUNTRY-SPECIFIC POINTS OF MINIMUM ED VISIT-> the median optimal percentile
minperccal <- median(minperczip)

print("4")
##ATTRIBUTABLE RISK
# LOAD THE FUNCTION FOR COMPUTING THE ATTRIBUTABLE RISK MEASURES
source("code/heatProjection/attrdl.R") #fix pathway

# CREATE THE VECTORS TO STORE THE TOTAL ED VISITS (ACCOUNTING FOR MISSING)
totdeath <- rep(NA,nrow(zips_meta))
names(totdeath) <- zips_meta$zip

# CREATE THE MATRIX TO STORE THE ATTRIBUTABLE ED VISITS
matsim <- matrix(NA,nrow(zips_meta),3,dimnames=list(zips_meta$zip,
                                                 c("glob","cold","heat")))
print("5")
# NUMBER OF SIMULATION RUNS FOR COMPUTING EMPIRICAL CI
nsim <- 1000

# CREATE THE ARRAY TO STORE THE CI OF ATTRIBUTABLE ED VISITS
arraysim <- array(NA,dim=c(nrow(zips_meta),3,nsim),dimnames=list(zips_meta$zip,
                                                              c("glob","cold","heat")))
print("6")
################################################################################

# RUN THE LOOP
for(i in seq(dlist)){
  tryCatch({
  # PRINT
  cat(i,"")
  print("7")
  # EXTRACT THE DATA
  data <- dlist[[i]]
  
  # DERIVE THE CROSS-BASIS
  # NB: CENTERING POINT DIFFERENT THAN ORIGINAL CHOICE OF 75TH
    argvar <- list(x=data$tmean_mean,fun=varfun,knots=quantile(data$tmean_mean,
                                                             varper/100,na.rm=T),degree=vardegree)
  cb <- crossbasis(data$tmean_mean,lag=lag,argvar=argvar,
                   arglag=list(knots=logknots(lag,lagnk)))
  
  # COMPUTE THE ATTRIBUTABLE ED VISITS
  # NB: THE REDUCED COEFFICIENTS ARE USED HERE
  matsim[i,"glob"] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                             vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i])
  matsim[i,"cold"] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                             vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                             range=c(-100,mintempzip[i]))
  matsim[i,"heat"] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                             vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                             range=c(mintempzip[i],100))
  
  # COMPUTE EMPIRICAL OCCURRENCES OF THE ATTRIBUTABLE ED VISITS
  # USED TO DERIVE CONFIDENCE INTERVALS
  arraysim[i,"glob",] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                                vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],sim=T,nsim=nsim)
  arraysim[i,"cold",] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                                vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                                range=c(-100,mintempzip[i]),sim=T,nsim=nsim)
  arraysim[i,"heat",] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                                vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                                range=c(mintempzip[i],100),sim=T,nsim=nsim)
  
  # STORE THE DENOMINATOR OF ATTRIBUTABLE ED VISITS, I.E. TOTAL OBSERVED ED VISITS
  # CORRECT DENOMINATOR TO COMPUTE THE ATTRIBUTABLE FRACTION LATER, AS IN attrdl
  totdeath[i] <- sum(data$n,na.rm=T)
  print("8")
  }, error=function(e){cat("ERROR: ",conditionMessage(e), "\n")})
  
}
print("9")
################################################################################
# ATTRIBUTABLE NUMBERS

# CITY-SPECIFIC
anzip <- matsim
anziplow <- apply(arraysim,c(1,2),quantile,0.025, na.rm=TRUE)
anziphigh <- apply(arraysim,c(1,2),quantile,0.975, na.rm=TRUE)
rownames(anzip) <- rownames(anziplow) <- rownames(anziphigh) <- zips_meta$zipname
write.csv(anzip, output_path_num)

# TOTAL
# NB: FIRST SUM THROUGH CITIES
antot <- colSums(matsim) ##839842.21 ED visits attributed to heat and cold
antotlow <- apply(apply(arraysim,c(2,3),sum),1,quantile,0.025, na.rm=TRUE)
antothigh <- apply(apply(arraysim,c(2,3),sum),1,quantile,0.975, na.rm=TRUE)

################################################################################
# TOTAL ED VISITS

# BY COUNTRY, the total number of ED visits (empirical)
totdeathtot <- sum(totdeath)
print("10")
################################################################################
# ATTRIBUTABLE FRACTIONS

# CITY-SPECIFIC
afzip <- anzip/totdeath*100
afziplow <- anziplow/totdeath*100
afziphigh <- anziphigh/totdeath*100
write.csv(afzip, output_path_frac)

# TOTAL: 6% from the zips that converged
aftot <- antot/totdeathtot*100
aftotlow <- antotlow/totdeathtot*100
aftothigh <- antothigh/totdeathtot*100

print("11")
return(list("min_temp_zip" = mintempzip,"min_percent_zip" = minperczip,"min_percent_california" = minperccal,"attributable_number_total"= antot, "attributable_number_low"= antotlow, 
            "attributable_number_high" = antothigh, "total_death_total"= totdeathtot,"attributable_fraction_total"= aftot,
            "attributable_fraction_low" = aftotlow,"attributable_fraction_high"= aftothigh))

}

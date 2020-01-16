
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

meta_stage_DLNM<-function(first_stage_list = first_stage,  output_path_mv_model = "data/meta_model_climate_16_11_7.rds", output_path_num = "data/processed/attributable_number_zips.csv",output_path_frac = "data/processed/attributable_frac_zips.csv", output_path_mintemp = "data/processed/mintemp_zips.csv", varfun = "bs", vardegree = 2, varper = c(10,75,90), lag = 3, lagnk = 2){
  
  
first_stage_list = full.list
output_path_mv_model = "//mnt/projects/ohe/heatProjections/data/meta_model_climate_UTI.rds"
output_path_num = "//mnt/projects/ohe/heatProjections/data/processed/attributable_number_climate_UTI.csv"
output_path_frac = "//mnt/projects/ohe/heatProjections/data/processed/attributable_frac_climate_UTI.csv" 
output_path_mintemp = "//mnt/projects/ohe/heatProjections/data/processed/mintemp_zips_climate_UTI.csv"
varfun = "bs"
vardegree = 2 
varper = c(10,75,90)
lag = 3
lagnk = 2
  
  

  dlist <- first_stage_list$dlist
  coef <- first_stage_list$coef
  vcov <- first_stage_list$vcov
  vcov<-vcov[names(vcov) %in% names(dlist)]
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

 #climate_zone_df<-read.csv("data/zip_climate_region_xwalk.csv")
 #climate_zone_df<-climate_zone_df[climate_zone_df$ZCTA5CE10 %in% zips_meta$zip,]
 #climate_zone_df<-climate_zone_df[match(zips_meta$zip, climate_zone_df$ZCTA5CE10),]
 #climate_zone_11<-climate_zone_df$climate_region11
 
 climate_zone_df_16<-read.csv("data/BuildingClimateZonesByZIPCode.csv")
 climate_zone_df_16<-climate_zone_df_16[climate_zone_df_16$zip %in% zips_meta$zip,]
 climate_zone_df_16<-climate_zone_df_16[match(zips_meta$zip, climate_zone_df_16$zip),]
 climate_zone_16<-climate_zone_df_16$building_CZ
################################################################################
# META-ANALYSIS
## location-specific average temp and range included as potential effect modifiers
print(length(avgtmean))
print(length(rangetmean))
print(length(vcov))
# print(length(climate_zone_10))
# print(length(climate_zone_11))
print("1")
#zips_meta<-droplevels(zips_meta)
#mv <- mvmeta(coef~avgtmean+rangetmean+climate_zone_11,vcov,data=zips_meta,control=list(showiter=T))
mv <- mvmeta(coef~avgtmean+rangetmean+climate_zone_16,vcov,data=zips_meta,control=list(showiter=T))

#mv <- mvmeta(coef~avgtmean+rangetmean,vcov,data=zips_meta,control=list(showiter=T))

print("2") 
#saveRDS(mv, "data/meta_model_just_zips_10_27.rds")
saveRDS(mv, output_path_mv_model)

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
#fwald(mv,"climate_zone_11")
print("2a")
################################################################################
# OBTAIN BLUPS BLUP= best linear unbiased prediction-> city specific covariates from meta model (weighted model)

blup <- blup(mv,vcov=T)
################################################################################
# RE-CENTERING
print("2b")
# GENERATE THE MATRIX FOR STORING THE RESULTS
minperczip <- mintempzip <- rep(NA,length(dlist))
names(mintempzip) <- names(minperczip) <- zips_meta$zip ###check this

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
totED <- rep(NA,nrow(zips_meta))
names(totED) <- zips_meta$zip

# CREATE THE MATRIX TO STORE THE ATTRIBUTABLE ED VISITS
matsim <- matrix(NA,nrow(zips_meta),15,dimnames=list(zips_meta$zipname,
                                                       c("glob","cold","heat","p2_5","p10", "p20","p30","p40","p50","p60","p70","p80","p90","p97_5", "p100")))
# NUMBER OF SIMULATION RUNS FOR COMPUTING EMPIRICAL CI
nsim <- 1000

# CREATE THE ARRAY TO STORE THE CI OF ATTRIBUTABLE ED VISITS
arraysim <- array(NA,dim=c(nrow(zips_meta),15,nsim),dimnames=list(zips_meta$zipname,
                                                                    c("glob","cold","heat","p2_5","p10", "p20","p30","p40","p50","p60","p70","p80","p90","p97_5", "p100")))
################################################################################
per <- t(sapply(dlist,function(x) 
  quantile(x$tmean,c(2.5, 10, 20, 30, 40, 50,60, 70, 80, 90, 97.5)/100,na.rm=T)))



print("6")
################################################################################

i <-3

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
  matsim[i,"glob"] <- attrdl(
    x = data$tmean_mean,
    basis = cb,
    cases = data$n,
    coef = blup[[i]]$blup,
    vcov = blup[[i]]$vcov,
    type = "an",
    dir = "forw",
    cen = mintempzip[i]
  )
  
  
  matsim[i,"cold"] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                             vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                             range=c(-100,mintempzip[i]))
  
  ##get rid of negative ED visits in cold
  matsim[i,"cold"]<- ifelse(matsim[i,"cold"] < 0, 0, matsim[i,"cold"])
  
  matsim[i,"heat"] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                             vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                             range=c(mintempzip[i],100))
  
  
  matsim[i,"p2_5"] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                             vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                             range=c(-100,per[i,1])) ## bottom 2.5%ile or temperatures
  matsim[i,"p10"] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                            vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                            range=c(per[i,1], per[i,2])) ## 2.5 - 10%ile or temperatures
  matsim[i,"p20"] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                            vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                            range=c(per[i,2], per[i,3])) ## 10 - 20%ile  or temperatures
  matsim[i,"p30"] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                            vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                            range=c(per[i,3], per[i,4])) ## 20 - 30%ile  or temperatures
  matsim[i,"p40"] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                            vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                            range=c(per[i,4], per[i,5])) ## 30 - 40%ile  or temperatures
  matsim[i,"p50"] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                            vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                            range=c(per[i,5], per[i,6])) ## 40 - 50%ile  or temperatures
  matsim[i,"p60"] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                            vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                            range=c(per[i,6], per[i,7])) ## 50 - 60%ile  or temperatures
  matsim[i,"p70"] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                            vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                            range=c(per[i,7], per[i,8])) ## 60 - 70%ile or temperatures
  matsim[i,"p80"] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                            vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                            range=c(per[i,8], per[i,9])) ## 70 - 80%ile or temperatures
  matsim[i,"p90"] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                            vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                            range=c(per[i,9], per[i,10])) ## 80 - 90%ile or temperatures
  matsim[i,"p97_5"] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                              vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                              range=c(per[i,10], per[i,11])) ## 90 - 97.5%ile or temperatures
  matsim[i,"p100"] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                             vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                             range=c(per[i,11],100)) ## top 2.5%ile ot temperatures
  
  # COMPUTE EMPIRICAL OCCURRENCES OF THE ATTRIBUTABLE ED VISITS
  # USED TO DERIVE CONFIDENCE INTERVALS
  arraysim[i,"glob",] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                                vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],sim=T,nsim=nsim)
  
  arraysim[i,"cold",] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                                vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                                range=c(-100,mintempzip[i]),sim=T,nsim=nsim)
  ##get rid of negative ED visits in cold
  arraysim[i,"cold",]<- ifelse(arraysim[i,"cold",] < 0, 0, arraysim[i,"cold",])
  
  arraysim[i,"heat",] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                                vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                                range=c(mintempzip[i],100),sim=T,nsim=nsim)
  
  arraysim[i,"p2_5",] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                                vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                                range=c(-100,per[i,1]),sim=T,nsim=nsim) ## bottom 2.5%ile or temperatures
  arraysim[i,"p10",] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                               vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                               range=c(per[i,1], per[i,2]),sim=T,nsim=nsim) ## 2.5 - 10%ile or temperatures
  arraysim[i,"p20",] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                               vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                               range=c(per[i,2], per[i,3]),sim=T,nsim=nsim) ## 10 - 20%ile  or temperatures
  arraysim[i,"p30",] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                               vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                               range=c(per[i,3], per[i,4]),sim=T,nsim=nsim) ## 20 - 30%ile  or temperatures
  arraysim[i,"p40",] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                               vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                               range=c(per[i,4], per[i,5]),sim=T,nsim=nsim) ## 30 - 40%ile  or temperatures
  arraysim[i,"p50",] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                               vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                               range=c(per[i,5], per[i,6]),sim=T,nsim=nsim) ## 40 - 50%ile  or temperatures
  arraysim[i,"p60",] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                               vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                               range=c(per[i,6], per[i,7]),sim=T,nsim=nsim) ## 50 - 60%ile  or temperatures
  arraysim[i,"p70",] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                               vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                               range=c(per[i,7], per[i,8]),sim=T,nsim=nsim) ## 60 - 70%ile or temperatures
  arraysim[i,"p80",] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                               vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                               range=c(per[i,8], per[i,9]),sim=T,nsim=nsim) ## 70 - 80%ile or temperatures
  arraysim[i,"p90",] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                               vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                               range=c(per[i,9], per[i,10]),sim=T,nsim=nsim) ## 80 - 90%ile or temperatures
  arraysim[i,"p97_5",] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                                 vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                                 range=c(per[i,10], per[i,11]),sim=T,nsim=nsim) ## 90 - 97.5%ile or temperatures
  arraysim[i,"p100",] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                                vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                                range=c(per[i,11],100),sim=T,nsim=nsim) ## top 2.5%ile ot temperatures
  
  
  # STORE THE DENOMINATOR OF ATTRIBUTABLE ED VISITS, I.E. TOTAL OBSERVED ED VISITS
  # CORRECT DENOMINATOR TO COMPUTE THE ATTRIBUTABLE FRACTION LATER, AS IN attrdl
  totED[i] <- sum(data$n,na.rm=T)
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
totEDtot <- sum(totED)
print("10")
################################################################################
# ATTRIBUTABLE FRACTIONS

# CITY-SPECIFIC
afzip <- anzip/totED*100
afziplow <- anziplow/totED*100
afziphigh <- anziphigh/totED*100
write.csv(afzip, output_path_frac)

# TOTAL: 6% from the zips that converged
aftot <- antot/totEDtot*100
aftotlow <- antotlow/totEDtot*100
aftothigh <- antothigh/totEDtot*100

write.csv(mintempzip, output_path_mintemp)

print("11")
return(list("min_temp_zip" = mintempzip,"min_percent_zip" = minperczip,
            "min_percent_california" = minperccal,"attributable_number_total"= antot,
            "attributable_number_low"= antotlow, "attributable_number_high" = antothigh,
            "total_ED_zip" = totED,"total_ED_total"= totEDtot, 
            "attributable_number_zip" = anzip, 
            "attributable_fraction_zip" = afzip, "attributable_fraction_total"= aftot,
            "attributable_fraction_low" = aftotlow,"attributable_fraction_high"= aftothigh, 
            "dlist_meta" = dlist, "blup" = blup))

}

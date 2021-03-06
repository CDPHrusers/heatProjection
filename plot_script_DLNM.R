
library(data.table)
library(tidyr)

# setwd("R:/heatProjections/")
#setwd("//mnt/projects/ohe/heatProjections/")

# or through Rstudio in your browser 
 setwd("//mnt/projects/ohe/heatProjections/")
source("code/heatProjection/heatProjectR/R/filter_ed.R")
source("code/heatProjection/heatProjectR/R/get_prism.R")
source("code/heatProjection/heatProjectR/R/combine_ed_temp.R")
source("code/heatProjection/heatProjectR/R/first_stage_DLNM.R")
source("code/heatProjection/heatProjectR/R/meta_stage_DLNM.R")
source("code/heatProjection/attrdl.R") #fix pathway
 
###Figures
##########
################################################################################
# TABLES
################################################################################
 meta<-readRDS("data/processed/meta_stage_DLNM_climate_bobb_12_7_19.rds")
 #meta<-readRDS("data/processed/meta_stage_DLNM_climate_bobb_12_3_19.rds")
 #meta<-readRDS("/data/processed/meta_stage_DLNM_climate_16_11_7.rds")
# meta <- readRDS("./data/processed/meta_stage_DLNM_climate_allED_MD.rds")
# meta<-readRDS("./data/processed/meta_stage_DLNM_climate_mental.rds")

dlist<-meta$dlist
totEDtot<-meta$total_ED_total
minperccal<-meta$min_percent_california
aftot<-meta$attributable_fraction_total
aftotlow<-meta$attributable_fraction_low
aftothigh<-meta$attributable_fraction_high
totED<-meta$total_ED_zip 
minperczip<-meta$min_percent_zip
mintempzip<-meta$min_temp_zip
afzip<-meta$attributable_fraction_zip
blup<-meta$blup
anzip<-meta$attributable_number_zip
zips_meta<-meta$dlist_meta
antot<-meta$attributable_number_total
antotlow<-meta$attributable_number_low
antothigh<-meta$attributable_number_high

anzip[,"cold"]<-ifelse(anzip[,"cold"]<0, 0, anzip[,"cold"])

###ATTRIBUTABLE NUMBERS FROM EXTREME TEMPS (2.5, 97.5 PERCENTILES)

# CREATE THE VECTORS TO STORE THE TOTAL ED VISITS (ACCOUNTING FOR MISSING)
totED <- rep(NA,length(zips_meta))
names(totED) <- names(zips_meta)

# CREATE THE MATRIX TO STORE THE ATTRIBUTABLE ED VISITS
matsim <- matrix(NA,length(zips_meta),3,dimnames=list(names(zips_meta),
                                                      c("glob","cold","heat")))
# NUMBER OF SIMULATION RUNS FOR COMPUTING EMPIRICAL CI
nsim <- 1000

# CREATE THE ARRAY TO STORE THE CI OF ATTRIBUTABLE ED VISITS
arraysim <- array(NA,dim=c(length(zips_meta),3,nsim),dimnames=list(names(zips_meta),
                                                                   c("glob","cold","heat")))
################################################################################
per <- t(sapply(dlist,function(x) 
  quantile(x$tmean_mean,c(2.5,97.5)/100,na.rm=T)))

##parameters for plotting
varfun = "bs" 
vardegree = 2 
varper = c(10,75,90) 
lag = 3
lagnk = 2

# RUN THE LOOP
for(i in seq(dlist)){
  tryCatch({
    i<-1
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
                               range=c(-100,per[i,1])) ##have changed this for extreme cold
    ##get rid of negative ED visits in cold
    matsim[i,"cold"]<- ifelse(matsim[i,"cold"] < 0, 0, matsim[i,"cold"])
    
    matsim[i,"heat"] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                               vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                               range=c(per[i,2],100)) ##have changed this for extreme heat
    
    # COMPUTE EMPIRICAL OCCURRENCES OF THE ATTRIBUTABLE ED VISITS
    # USED TO DERIVE CONFIDENCE INTERVALS
    arraysim[i,"glob",] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                                  vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],sim=T,nsim=nsim)
    arraysim[i,"cold",] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                                  vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                                  range=c(-100,per[i,1]),sim=T,nsim=nsim)
   
    ##get rid of negative ED visits in cold
    arraysim[i,"cold",]<- ifelse(arraysim[i,"cold",] < 0, 0, arraysim[i,"cold",])
    
     arraysim[i,"heat",] <- attrdl(data$tmean_mean,cb,data$n,coef=blup[[i]]$blup,
                                  vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempzip[i],
                                  range=c(per[i,2],100),sim=T,nsim=nsim)
    
    # STORE THE DENOMINATOR OF ATTRIBUTABLE ED VISITS, I.E. TOTAL OBSERVED ED VISITS
    # CORRECT DENOMINATOR TO COMPUTE THE ATTRIBUTABLE FRACTION LATER, AS IN attrdl
    totED[i] <- sum(data$n,na.rm=T)
    print("8")
  }, error=function(e){cat("ERROR: ",conditionMessage(e), "\n")})
  
}
print("9")

# CITY-SPECIFIC
anzip.x <- matsim
anziplow.x <- apply(arraysim,c(1,2),quantile,0.025, na.rm=TRUE)
anziphigh.x <- apply(arraysim,c(1,2),quantile,0.975, na.rm=TRUE)
rownames(anzip.x) <- rownames(anziplow.x) <- rownames(anziphigh.x) <- names(zips_meta)
#write.csv(anzip.x, "/data/processed/attributable_number_climate_extremes_12_8_19.csv")


anzip.x<-read.csv("/data/processed/attributable_number_climate_extremes_12_8_19.csv")
totED<-meta$total_ED_zip



per <- t(sapply(dlist,function(x) 
  quantile(x$tmean,c(2.5,10,25,50,75,90,97.5)/100,na.rm=T)))


climate_zone_df_16<-read.csv("/data/BuildingClimateZonesByZIPCode.csv")
climate_zone_df_16<-climate_zone_df_16[climate_zone_df_16$zip %in% names(dlist),]
climate_zone_df_16<-climate_zone_df_16[match(names(dlist), climate_zone_df_16$zip),]
climate_zone_df_16$building_CZ<-as.factor(climate_zone_df_16$building_CZ)

totED<-as.data.frame(cbind(totED, climate_zone_df_16$building_CZ))
colnames(totED)[2]<-"climate_zone"
totED$climate_zone<-as.factor(totED$climate_zone)

minperczip<-as.data.frame(cbind(minperczip, climate_zone_df_16$building_CZ ))
colnames(minperczip)[2]<-"climate_zone"
minperczip$climate_zone<-as.factor(minperczip$climate_zone)

mintempzip<-as.data.frame(cbind(mintempzip, climate_zone_df_16$building_CZ))
colnames(mintempzip)[2]<-"climate_zone"
mintempzip$climate_zone<-as.factor(mintempzip$climate_zone)

afzip<-as.data.frame(cbind(afzip, climate_zone_df_16$building_CZ ))
colnames(afzip)[4]<-"climate_zone"
afzip$climate_zone<-as.factor(afzip$climate_zone)

anzip<-as.data.frame(cbind(anzip, climate_zone_df_16$building_CZ ))
colnames(anzip)[4]<-"climate_zone"
anzip$climate_zone<-as.factor(anzip$climate_zone)

anzip.x<-as.data.frame(cbind(anzip.x, climate_zone_df_16$building_CZ ))
colnames(anzip.x)[5]<-"climate_zone"
anzip.x$climate_zone<-as.factor(anzip.x$climate_zone)





climate.split<-split(climate_zone_df_16, climate_zone_df_16$building_CZ)

cz1<-climate.split[[1]] 
cz1<-as.character(as.numeric(cz1$zip))
cz2<-climate.split[[2]] 
cz2<-as.character(as.numeric(cz2$zip))
cz3<-climate.split[[3]] 
cz3<-as.character(as.numeric(cz3$zip))
cz4<-climate.split[[4]] 
cz4<-as.character(as.numeric(cz4$zip))
cz5<-climate.split[[5]] 
cz5<-as.character(as.numeric(cz5$zip))
cz6<-climate.split[[6]] 
cz6<-as.character(as.numeric(cz6$zip))
cz7<-climate.split[[7]] 
cz7<-as.character(as.numeric(cz7$zip))
cz8<-climate.split[[8]] 
cz8<-as.character(as.numeric(cz8$zip))
cz9<-climate.split[[9]] 
cz9<-as.character(as.numeric(cz9$zip))
cz10<-climate.split[[10]] 
cz10<-as.character(as.numeric(cz10$zip))
cz11<-climate.split[[11]] 
cz11<-as.character(as.numeric(cz11$zip))
cz12<-climate.split[[12]] 
cz12<-as.character(as.numeric(cz12$zip))
cz13<-climate.split[[13]] 
cz13<-as.character(as.numeric(cz13$zip))
cz14<-climate.split[[14]] 
cz14<-as.character(as.numeric(cz14$zip))
cz15<-climate.split[[15]] 
cz15<-as.character(as.numeric(cz15$zip))
cz16<-climate.split[[16]] 
cz16<-as.character(as.numeric(cz16$zip))

dlist.1<-dlist[cz1]
dlist.2<-dlist[cz2]
dlist.3<-dlist[cz3]
dlist.4<-dlist[cz4]
dlist.5<-dlist[cz5]
dlist.6<-dlist[cz6]
dlist.7<-dlist[cz7]
dlist.8<-dlist[cz8]
dlist.9<-dlist[cz9]
dlist.10<-dlist[cz10]
dlist.11<-dlist[cz11]
dlist.12<-dlist[cz12]
dlist.13<-dlist[cz13]
dlist.14<-dlist[cz14]
dlist.15<-dlist[cz15]
dlist.16<-dlist[cz16]

################################################################################
# RELATED PART OF TABLE 1

#mean temps for each zip in all of CALIFORNIA
tmeanca <- sapply(dlist,function(zip) mean(zip$tmean_mean,na.rm=T))

tmean1 <- sapply(dlist.1,function(zip) mean(zip$tmean_mean,na.rm=T))
tmean2 <- sapply(dlist.2,function(zip) mean(zip$tmean_mean,na.rm=T))
tmean3 <- sapply(dlist.3,function(zip) mean(zip$tmean_mean,na.rm=T))
tmean4 <- sapply(dlist.4,function(zip) mean(zip$tmean_mean,na.rm=T))
tmean5 <- sapply(dlist.5,function(zip) mean(zip$tmean_mean,na.rm=T))
tmean6 <- sapply(dlist.6,function(zip) mean(zip$tmean_mean,na.rm=T))
tmean7 <- sapply(dlist.7,function(zip) mean(zip$tmean_mean,na.rm=T))
tmean8 <- sapply(dlist.8,function(zip) mean(zip$tmean_mean,na.rm=T))
tmean9 <- sapply(dlist.9,function(zip) mean(zip$tmean_mean,na.rm=T))
tmean10 <- sapply(dlist.10,function(zip) mean(zip$tmean_mean,na.rm=T))
tmean11 <- sapply(dlist.11,function(zip) mean(zip$tmean_mean,na.rm=T))
tmean12 <- sapply(dlist.12,function(zip) mean(zip$tmean_mean,na.rm=T))
tmean13 <- sapply(dlist.13,function(zip) mean(zip$tmean_mean,na.rm=T))
tmean14 <- sapply(dlist.14,function(zip) mean(zip$tmean_mean,na.rm=T))
tmean15 <- sapply(dlist.15,function(zip) mean(zip$tmean_mean,na.rm=T))
tmean16 <- sapply(dlist.16,function(zip) mean(zip$tmean_mean,na.rm=T))

totED1<-sum(sapply(dlist.1, function(zip) sum(zip$n, na.rm = T)))
totED2<-sum(sapply(dlist.2, function(zip) sum(zip$n, na.rm = T)))
totED3<-sum(sapply(dlist.3, function(zip) sum(zip$n, na.rm = T)))
totED4<-sum(sapply(dlist.4, function(zip) sum(zip$n, na.rm = T)))
totED5<-sum(sapply(dlist.5, function(zip) sum(zip$n, na.rm = T)))
totED6<-sum(sapply(dlist.6, function(zip) sum(zip$n, na.rm = T)))
totED7<-sum(sapply(dlist.7, function(zip) sum(zip$n, na.rm = T)))
totED8<-sum(sapply(dlist.8, function(zip) sum(zip$n, na.rm = T)))
totED9<-sum(sapply(dlist.9, function(zip) sum(zip$n, na.rm = T)))
totED10<-sum(sapply(dlist.10, function(zip) sum(zip$n, na.rm = T)))
totED11<-sum(sapply(dlist.11, function(zip) sum(zip$n, na.rm = T)))
totED12<-sum(sapply(dlist.12, function(zip) sum(zip$n, na.rm = T)))
totED13<-sum(sapply(dlist.13, function(zip) sum(zip$n, na.rm = T)))
totED14<-sum(sapply(dlist.14, function(zip) sum(zip$n, na.rm = T)))
totED15<-sum(sapply(dlist.15, function(zip) sum(zip$n, na.rm = T)))
totED16<-sum(sapply(dlist.16, function(zip) sum(zip$n, na.rm = T)))


##mean mean temp and range for all of CALIFORNIA
c(ClimateZone="California",
  Period=paste(range(dlist[[1]]$Date),collapse="-"),EDvisits=totEDtot,
  Temperature=paste0(formatC(mean(tmeanca),dig=1,
                             format="f")," (",paste(formatC(range(tmeanca),dig=1,format="f"),
                                                    collapse="-"),")"))

c(ClimateZone="1",
  Period=paste(range(dlist.1[[1]]$Date),collapse="-"),EDvisits=totED1,
  Temperature=paste0(formatC(mean(tmean1),dig=1,
                             format="f")," (",paste(formatC(range(tmean1),dig=1,format="f"),
                                                    collapse="-"),")"))

c(ClimateZone="2",
  Period=paste(range(dlist.2[[1]]$Date),collapse="-"),EDvisits=totED2,
  Temperature=paste0(formatC(mean(tmean2),dig=1,
                             format="f")," (",paste(formatC(range(tmean2),dig=1,format="f"),
                                                    collapse="-"),")"))
c(ClimateZone="3",
  Period=paste(range(dlist.3[[1]]$Date),collapse="-"),EDvisits=totED3,
  Temperature=paste0(formatC(mean(tmean3),dig=1,
                             format="f")," (",paste(formatC(range(tmean3),dig=1,format="f"),
                                                    collapse="-"),")"))
c(ClimateZone="4",
  Period=paste(range(dlist.4[[1]]$Date),collapse="-"),EDvisits=totED4,
  Temperature=paste0(formatC(mean(tmean4),dig=1,
                             format="f")," (",paste(formatC(range(tmean4),dig=1,format="f"),
                                                    collapse="-"),")"))
c(ClimateZone="5",
  Period=paste(range(dlist.5[[1]]$Date),collapse="-"),EDvisits=totED5,
  Temperature=paste0(formatC(mean(tmean5),dig=1,
                             format="f")," (",paste(formatC(range(tmean5),dig=1,format="f"),
                                                    collapse="-"),")"))
c(ClimateZone="6",
  Period=paste(range(dlist.6[[1]]$Date),collapse="-"),EDvisits=totED6,
  Temperature=paste0(formatC(mean(tmean6),dig=1,
                             format="f")," (",paste(formatC(range(tmean6),dig=1,format="f"),
                                                    collapse="-"),")"))
c(ClimateZone="7",
  Period=paste(range(dlist.7[[1]]$Date),collapse="-"),EDvisits=totED7,
  Temperature=paste0(formatC(mean(tmean7),dig=1,
                             format="f")," (",paste(formatC(range(tmean7),dig=1,format="f"),
                                                    collapse="-"),")"))
c(ClimateZone="8",
  Period=paste(range(dlist.8[[1]]$Date),collapse="-"),EDvisits=totED8,
  Temperature=paste0(formatC(mean(tmean8),dig=1,
                             format="f")," (",paste(formatC(range(tmean8),dig=1,format="f"),
                                                    collapse="-"),")"))
c(ClimateZone="9",
  Period=paste(range(dlist.9[[1]]$Date),collapse="-"),EDvisits=totED9,
  Temperature=paste0(formatC(mean(tmean9),dig=1,
                             format="f")," (",paste(formatC(range(tmean9),dig=1,format="f"),
                                                    collapse="-"),")"))
c(ClimateZone="10",
  Period=paste(range(dlist.10[[1]]$Date),collapse="-"),EDvisits=totED10,
  Temperature=paste0(formatC(mean(tmean10),dig=1,
                             format="f")," (",paste(formatC(range(tmean10),dig=1,format="f"),
                                                    collapse="-"),")"))
c(ClimateZone="11",
  Period=paste(range(dlist.11[[1]]$Date),collapse="-"),EDvisits=totED11,
  Temperature=paste0(formatC(mean(tmean11),dig=1,
                             format="f")," (",paste(formatC(range(tmean11),dig=1,format="f"),
                                                    collapse="-"),")"))
c(ClimateZone="12",
  Period=paste(range(dlist.12[[1]]$Date),collapse="-"),EDvisits=totED12,
  Temperature=paste0(formatC(mean(tmean12),dig=1,
                             format="f")," (",paste(formatC(range(tmean12),dig=1,format="f"),
                                                    collapse="-"),")"))
c(ClimateZone="13",
  Period=paste(range(dlist.13[[1]]$Date),collapse="-"),EDvisits=totED13,
  Temperature=paste0(formatC(mean(tmean13),dig=1,
                             format="f")," (",paste(formatC(range(tmean13),dig=1,format="f"),
                                                    collapse="-"),")"))
c(ClimateZone="14",
  Period=paste(range(dlist.14[[1]]$Date),collapse="-"),EDvisits=totED14,
  Temperature=paste0(formatC(mean(tmean14),dig=1,
                             format="f")," (",paste(formatC(range(tmean14),dig=1,format="f"),
                                                    collapse="-"),")"))
c(ClimateZone="15",
  Period=paste(range(dlist.15[[1]]$Date),collapse="-"),EDvisits=totED15,
  Temperature=paste0(formatC(mean(tmean15),dig=1,
                             format="f")," (",paste(formatC(range(tmean15),dig=1,format="f"),
                                                    collapse="-"),")"))
c(ClimateZone="16",
  Period=paste(range(dlist.16[[1]]$Date),collapse="-"),EDvisits=totED16,
  Temperature=paste0(formatC(mean(tmean16),dig=1,
                             format="f")," (",paste(formatC(range(tmean1),dig=1,format="f"),
                                                    collapse="-"),")"))

################################################################################
# RELATED PART OF TABLE 2

# MMP
minperccal
median(mintempzip$mintempzip)

# ATTRIBUTABLE FRACTION
t(cbind(aftot,aftotlow,aftothigh))

##manually caluculate att

################################################################################
# RELATED PART OF TABLE S4

# DEATHS
totEDtot

# MINIMUM MORTALITY TEMPERATURE PERCENTILE AND ABSOLUTE TEMPERATURE
minperczip$minperczip<-round(minperczip$minperczip, 0)
minperczip %>% group_by(climate_zone) %>% summarise( Median = median(minperczip))

mintempzip$mintempzip<-round(mintempzip$mintempzip, 0)
mintempzip %>% group_by(climate_zone) %>% summarise( Median = median(mintempzip))


# ATTRIBUTABLE FRACTION MEDIANS
#afzip$cold<-round(afzip$cold, 2)
#afzip$heat<-round(afzip$heat, 2)
afzip.climate<-afzip %>% group_by(climate_zone) %>% summarise( Median.cold = median(cold), Median.hot = median(heat))

totED.climate<-totED %>% group_by(climate_zone) %>% summarise(totED = sum(totED))

# ATTRIBUTABLE NUMBER
#anzip$cold<-round(anzip$cold, 0)
#anzip$heat<-round(anzip$heat, 0)

anzip.climate<-anzip %>% 
  group_by(climate_zone) %>% 
  summarise( Median.cold = median(cold), 
             Median.heat = median(heat),
             an.sum.cold = sum(cold), 
             an.sum.heat=sum(heat), 
             sum.total=sum(glob))
anzip.climate<-merge(anzip.climate, totED.climate)
anzip.climate$af.cold.totED<-((anzip.climate$an.sum.cold/anzip.climate$totED))*100
anzip.climate$af.heat.totED<-((anzip.climate$an.sum.heat/anzip.climate$totED))*100
sum(anzip.climate$sum.total)
write.csv(anzip.climate, "data/processed/anzip_climatezone.csv")

# ATTRIBUTABLE NUMBER EXTREME
#anzip.x$cold<-round(anzip.x$cold, 0)
#anzip.x$heat<-round(anzip.x$heat, 0)

anzip.extreme<-anzip.x %>% 
  group_by(climate_zone) %>%
  summarise( Median.cold.x = median(cold), 
             Median.heat.x = median(heat),
             an.sum.cold.x = sum(cold), 
             an.sum.heat.x=sum(heat), 
             sum.total.x=sum(glob))

anzip.extreme.climate<-merge(anzip.extreme, totED.climate)

anzip.extreme.climate$af.cold.x.totED<-((anzip.extreme.climate$an.sum.cold.x/anzip.extreme.climate$totED))*100
anzip.extreme.climate$af.heat.x.totED<-((anzip.extreme.climate$an.sum.heat.x/anzip.extreme.climate$totED))*100

write.csv(anzip.extreme.climate, "/data/processed/anzip_extreme_median_climatezone_12_8_19.csv")


###SUBSET to selected zipcodes for AGU presentation

##zipcodes to keep: CZ1 Arcata: 95521, CZ2 Santa Rosa: 95403, 
## CZ3 Oakland: 94601, CZ4 San Jose-Reid 95123, CZ5 Santa Maria 93458, 
##CZ6 Torrance 90503, CZ7 San Diego- Lindberg 92126, CZ8 Fullerton 92833, 
##CZ10 Burbank-Glendale 91505, CZ10 Riverside 92503, CZ11 Red Bluff 96080, 
##CZ12 Sacramento 95823, CZ13 Fresno 93702, CZ14 Palmdale 93550
##CZ15 Palm Spring- Intl 92262, CZ16 Blue Canyon 96161,
mintempzip<-meta$min_temp_zip
minperczip<-meta$min_percent_zip
cz.names<-c("CZ1 :Arcata, 95521", "CZ2: Santa Rosa, 95403", "CZ3: Oakland,94601", "CZ4: San Jose, 95123",
            "CZ5: Santa Maria, 93458", "CZ6: Torrance, 90503", "CZ7: San Diego, 92126", 
            "CZ8: Fullerton, 92833", "CZ9: Burbank-Glendale, 91505", "CZ10: Riverside, 92503",
            "CZ11: Red Bluff, 96080", "CZ12: Sacramento, 95823", "CZ13: Fresno, 93702", 
            "CZ14: Palmdale, 93550", "CZ15: Palm Springs, 92262", "CZ16: Blue Canyon, 96161")

zips.keep<-c("95521", "95403","94601","95123", "93458", "90503", "92126",
             "92833", "91505", "92503", "96080", "95823", "93702", "93550",
             "92262", "96161")
match(zips.keep, names(dlist))

sub<-c(1322, 1261, 1026, 1138, 749, 114,
       385, 601, 240, 501, 1589,1464,840,
       780,424, 1630)

dlist.sub<-dlist[zips.keep]

blup.sub<-blup[sub]

mintempzip.sub<-mintempzip[sub]
minperczip.sub<-minperczip[sub]
mintempzip.df.sub<-cbind(names(dlist),cz.names, mintempzip, minperczip)

per.sub <- per[sub,]
##parameters for plotting
varfun = "bs" 
vardegree = 2 
varper = c(10,75,90) 


################################################################################
# PLOTS
################################################################################

################################################################################
# SIMILAR TO FIGURE 1

xlab <- expression(paste("Temperature (",degree,"C)"))

pdf("figure16_mental.pdf")
layout(matrix(c(0,1,1,2,2,0,rep(3:8,each=2),0,9,9,10,10,0),ncol=6,byrow=T))
par(mar=c(4,3.8,3,2.4),mgp=c(2.5,1,0),las=1)



for(i in seq(length(dlist.sub))) {
  data <- dlist.sub[[i]]
  # NB: CENTERING POINT DIFFERENT THAN ORIGINAL CHOICE OF 75TH
  argvar <- list(x=data$tmean_mean,fun=varfun,degree=vardegree,
                 knots=quantile(data$tmean_mean,varper/100,na.rm=T))
  bvar <- do.call(onebasis,argvar)
  pred <- crosspred(bvar,coef=blup.sub[[i]]$blup,vcov=blup.sub[[i]]$vcov,
                    model.link="log",by=0.1,cen=mintempzip.sub[i])
  x <- seq(0, 35, 5)

  plot(pred,type="n",ylim=c(0,1.5),xlim=c(0,35), yaxt="n",lab=c(6,5,7),xlab=xlab,ylab="RR",
       main=cz.names[i])

  ind1 <- pred$predvar<=mintempzip.sub[i]
  ind2 <- pred$predvar>=mintempzip.sub[i]
  lines(pred$predvar[ind1],pred$allRRfit[ind1],col=4,lwd=1.5)
  lines(pred$predvar[ind2],pred$allRRfit[ind2],col=2,lwd=1.5)
 
   #mtext(cities$countryname[i],cex=0.7,line=0)
  #axis(1,at=0:35*5)
  axis(2,at=1:5*0.5)
  breaks <- c(min(data$tmean,na.rm=T)-1,seq(pred$predvar[1],
                                            pred$predvar[length(pred$predvar)],length=30),max(data$tmean,na.rm=T)+1)
  hist <- hist(data$tmean_mean,breaks=breaks,plot=F)
  hist$density <- hist$density/max(hist$density)*0.7
  prop <- max(hist$density)/max(hist$counts)
  counts <- pretty(hist$count,3)
  plot(hist,ylim=c(0,max(hist$density)*3.5),axes=F,ann=F,col=grey(0.95),
       breaks=breaks,freq=F,add=T)
  axis(4,at=counts*prop,labels=counts,cex.axis=0.7)
  #mtext("N",4,line=-0.5,at=mean(counts*prop),cex=0.5)
  abline(v=mintempzip.sub[i],lty=3)
  abline(v=c(per.sub[i,c("2.5%","97.5%")]),lty=2)
}

dev.off()

#




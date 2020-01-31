library(data.table)
library(tidyr)

rm(list=ls())
gc()
# setwd("R:/heatProjections/")
setwd("//mnt/projects/ohe/heatProjections/")


# install.packages("dlnm")

# or through Rstudio in your browser 
# setwd("//mnt/projects/ohe/heatProjections/")
source("code/heatProjection/heatProjectR/R/filter_ed.R")
source("code/heatProjection/heatProjectR/R/get_prism.R")
source("code/heatProjection/heatProjectR/R/combine_ed_temp.R")
source("code/heatProjection/heatProjectR/R/first_stage_DLNM.R")
source("code/heatProjection/heatProjectR/R/meta_stage_DLNM.R")

# test case
combine_ed_temp(file_path_ed = "./data/processed/ed_test_data.csv", file_path_prism = "data/processed/prism_test_data.csv", output_path = "./data/processed/combined_test_data_small.csv")

# list of CSS codes here
# https://www.hcup-us.ahrq.gov/toolssoftware/ccs/CCSCategoryNames_FullLabels.pdf
# CCS codes 55  Fluid and electrolyte disorders
#           157 Renal failure (acute/unspecified)
#           159 Urinary tract infections
#           244 Heat stroke and other external causes
#           108 Congestive heart failure
#           2   Septicemia (except in labor)
# 

bobb_6CCSheat <- c(55L, 157L, 159L, 244L, 108L, 2L)
bobb_allED <- "all"
bobb_mental <- c(650, 651, 652, 653, 654, 655, 656, 657, 658, 659, 660, 661, 662, 663, 670)
bobb_UTI <- c(159L)
bobb_renal <- c(157L)


############################################################
############################################################
###########   from here forward                       ######
###########  replace the '_cause' portions of code    ######
###########  to run for another cause                 ######
############################################################
############################################################

condition <- "_mental"



#############combine temp and ed  #########################

# individual years
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2005.csv", file_path_prism = "data/PRISM/prism_2005.csv", output_path = paste0("./data/processed/tempAndED/tempAndED_2005",condition,".csv"), bobbCodes = bobb_mental)
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2006.csv", file_path_prism = "data/PRISM/prism_2006.csv", output_path = paste0("./data/processed/tempAndED/tempAndED_2006",condition,".csv"), bobbCodes = bobb_mental)
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2007.csv", file_path_prism = "data/PRISM/prism_2007.csv", output_path = paste0("./data/processed/tempAndED/tempAndED_2007",condition,".csv"), bobbCodes = bobb_mental)
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2008.csv", file_path_prism = "data/PRISM/prism_2008.csv", output_path = paste0("./data/processed/tempAndED/tempAndED_2008",condition,".csv"), bobbCodes = bobb_mental)
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2009.csv", file_path_prism = "data/PRISM/prism_2009.csv", output_path = paste0("./data/processed/tempAndED/tempAndED_2009",condition,".csv"), bobbCodes = bobb_mental)
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2010.csv", file_path_prism = "data/PRISM/prism_2010.csv", output_path = paste0("./data/processed/tempAndED/tempAndED_2010",condition,".csv"), bobbCodes = bobb_mental)
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2011.csv", file_path_prism = "data/PRISM/prism_2011.csv", output_path = paste0("./data/processed/tempAndED/tempAndED_2011",condition,".csv"), bobbCodes = bobb_mental)
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2012.csv", file_path_prism = "data/PRISM/prism_2012.csv", output_path = paste0("./data/processed/tempAndED/tempAndED_2012",condition,".csv"), bobbCodes = bobb_mental)
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2013.csv", file_path_prism = "data/PRISM/prism_2013.csv", output_path = paste0("./data/processed/tempAndED/tempAndED_2013",condition,".csv"), bobbCodes = bobb_mental)
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2014.csv", file_path_prism = "data/PRISM/prism_2014.csv", output_path = paste0("./data/processed/tempAndED/tempAndED_2014",condition,".csv"), bobbCodes = bobb_mental)
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2015.csv", file_path_prism = "data/PRISM/prism_2015.csv", output_path = paste0("./data/processed/tempAndED/tempAndED_2015",condition,".csv"), bobbCodes = bobb_mental)
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2016.csv", file_path_prism = "data/PRISM/prism_2016.csv", output_path = paste0("./data/processed/tempAndED/tempAndED_2016",condition,".csv"), bobbCodes = bobb_mental)
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2017.csv", file_path_prism = "data/PRISM/prism_2017.csv", output_path = paste0("./data/processed/tempAndED/tempAndED_2017",condition,".csv"), bobbCodes = bobb_mental)
gc()


# merge the years into a single table and save it as csv
rbindlist(lapply(list.files("./data/processed/tempAndED/", full.names = T, pattern = paste0(condition,".csv")), FUN = fread ), fill = TRUE) %>% 
  fwrite(paste0("./data/processed/temp_and_ed_05-17",condition,".csv"))

######## RUN DLNM ############################

full.list <- first_stage_DLNM(file_path= paste0("./data/processed/temp_and_ed_05-17",condition,".csv"))
saveRDS(full.list, paste0("./data/processed/first_stage_DLNM",condition,".rds")) # if running on rstudio server
# saveRDS(full.list, paste0("./data/processed/first_stage_DLNM",condition,"_version_B.rds")) # if running on your machine


####### run meta stage #############


full.list<-readRDS(paste0("data/processed/first_stage_DLNM",condition,".rds")) # change to match the path above in the 'saveRDS'
# full.list<-readRDS(paste0("data/processed/first_stage_DLNM",condition,"_version_B.rds")) # change to match the path above in the 'saveRDS'


meta.full <- meta_stage_DLNM(first_stage_list = full.list, 
                           output_path_mv_model = paste0("//mnt/projects/ohe/heatProjections/data/processed/results/meta_model",condition,".rds"),
                           output_path_num = paste0("//mnt/projects/ohe/heatProjections/data/processed/results/attributable_number",condition,".csv"),
                           output_path_mintemp = paste0("//mnt/projects/ohe/heatProjections/data/processed/results/mintemp_zips",condition,".csv"), 
                           output_path_frac = paste0("//mnt/projects/ohe/heatProjections/data/processed/results/attributable_frac",condition,".csv"), 
                           output_path_relrisk = paste0("//mnt/projects/ohe/heatProjections/data/processed/results/temp_RR_table",condition,".csv"), 
                           varfun = "bs", 
                           vardegree = 2, 
                           varper = c(10,75,90), 
                           lag = 3, 
                           lagnk = 2)

saveRDS(meta.full, paste0("./data/processed/results/meta_stage_DLNM",condition,".rds"))


############  Process results Grab RRs  ##############


meta.full <- readRDS(paste0("./data/processed/results/meta_stage_DLNM",condition,".rds"))

source("code/heatProjection/heatProjectR/R/get_rr.R")

rbindlist(lapply(names(meta.full$dlist_meta), get_rr)) %>% fwrite(paste0("./data/processed/results/temp_RR_table",condition,".csv"))






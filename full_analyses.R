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
bobb_all <- "all"
bobb_mental <- c(650, 651, 652, 653, 654, 655, 656, 657, 658, 659, 660, 661, 662, 663, 670)
bobb_UTI <- c(159L)
bobb_renal <- c(157L)


############################################################
############################################################
###########   from here forward                 ############
###########  replace the '_cause' portions of code    ######
###########  to run for another cause                 ######
############################################################
############################################################

# individual years
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2005.csv", file_path_prism = "data/PRISM/prism_2005.csv", output_path = "./data/processed/tempAndED/2019-11-25_tempAndED_2005_6CCSheat.csv", bobbCodes = bobb_6CCSheat)
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2006.csv", file_path_prism = "data/PRISM/prism_2006.csv", output_path = "./data/processed/tempAndED/2019-11-25_tempAndED_2006_6CCSheat.csv", bobbCodes = bobb_6CCSheat)
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2007.csv", file_path_prism = "data/PRISM/prism_2007.csv", output_path = "./data/processed/tempAndED/2019-11-25_tempAndED_2007_6CCSheat.csv", bobbCodes = bobb_6CCSheat)
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2008.csv", file_path_prism = "data/PRISM/prism_2008.csv", output_path = "./data/processed/tempAndED/2019-11-25_tempAndED_2008_6CCSheat.csv", bobbCodes = bobb_6CCSheat)
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2009.csv", file_path_prism = "data/PRISM/prism_2009.csv", output_path = "./data/processed/tempAndED/2019-11-25_tempAndED_2009_6CCSheat.csv", bobbCodes = bobb_6CCSheat)
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2010.csv", file_path_prism = "data/PRISM/prism_2010.csv", output_path = "./data/processed/tempAndED/2019-11-25_tempAndED_2010_6CCSheat.csv", bobbCodes = bobb_6CCSheat)
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2011.csv", file_path_prism = "data/PRISM/prism_2011.csv", output_path = "./data/processed/tempAndED/2019-11-25_tempAndED_2011_6CCSheat.csv", bobbCodes = bobb_6CCSheat)
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2012.csv", file_path_prism = "data/PRISM/prism_2012.csv", output_path = "./data/processed/tempAndED/2019-11-25_tempAndED_2012_6CCSheat.csv", bobbCodes = bobb_6CCSheat)
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2013.csv", file_path_prism = "data/PRISM/prism_2013.csv", output_path = "./data/processed/tempAndED/2019-11-25_tempAndED_2013_6CCSheat.csv", bobbCodes = bobb_6CCSheat)
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2014.csv", file_path_prism = "data/PRISM/prism_2014.csv", output_path = "./data/processed/tempAndED/2019-11-25_tempAndED_2014_6CCSheat.csv", bobbCodes = bobb_6CCSheat)
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2015.csv", file_path_prism = "data/PRISM/prism_2015.csv", output_path = "./data/processed/tempAndED/2019-11-25_tempAndED_2015_6CCSheat.csv", bobbCodes = bobb_6CCSheat)
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2016.csv", file_path_prism = "data/PRISM/prism_2016.csv", output_path = "./data/processed/tempAndED/2019-11-25_tempAndED_2016_6CCSheat.csv", bobbCodes = bobb_6CCSheat)
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2017.csv", file_path_prism = "data/PRISM/prism_2017.csv", output_path = "./data/processed/tempAndED/2019-11-25_tempAndED_2017_6CCSheat.csv", bobbCodes = bobb_6CCSheat)
gc()



# merge the years into a single table and save it as csv
rbindlist(lapply(list.files("./data/processed/tempAndED/", full.names = T, pattern = "_renal.csv"), FUN = fread ), fill = TRUE) %>% fwrite("./data/processed/temp_and_ed_05-17_6CCSheat.csv")



full.list <- first_stage_DLNM(file_path="./data/processed/temp_and_ed_05-17_6CCSheat.csv")
saveRDS(full.list, "./data/processed/first_stage_DLNM_6CCSheat.rds") # if running on rstudio server
# saveRDS(full.list, "./data/processed/first_stage_DLNM_6CCSheat_version_B.rds") # if running on your machine


full.list<-readRDS("data/processed/first_stage_DLNM_6CCSheat.rds") # change to match the path above in the 'saveRDS'
# full.list<-readRDS("data/processed/first_stage_DLNM_6CCSheat_version_B.rds") # change to match the path above in the 'saveRDS'


meta.full <- meta_stage_DLNM(first_stage_list = full.list, 
                           output_path_mv_model = "//mnt/projects/ohe/heatProjections/data/meta_model_6CCSheat.rds",
                           output_path_num = "//mnt/projects/ohe/heatProjections/data/processed/attributable_number_6CCSheat.csv",
                           output_path_frac = "//mnt/projects/ohe/heatProjections/data/processed/attributable_frac_6CCSheat.csv", 
                           output_path_mintemp = "//mnt/projects/ohe/heatProjections/data/processed/mintemp_zips_6CCSheat.csv", 
                           varfun = "bs", 
                           vardegree = 2, 
                           varper = c(10,75,90), 
                           lag = 3, 
                           lagnk = 2)

saveRDS(meta.full, "./data/processed/meta_stage_DLNM_6CCSheat.rds")
meta.full

#meta<-readRDS("./data/processed/meta_stage_DLNM_72419.rds")
#meta<-readRDS("./data/processed/meta_stage_DLNM_6CCSheat.rds")

#mv.model.zips<-readRDS("data/meta_model_just_zips_10_27.rds")
#mv.model.climate.16<-readRDS("data/meta_model_16_10_29.rds")




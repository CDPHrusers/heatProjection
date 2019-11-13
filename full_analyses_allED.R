library(data.table)
library(tidyr)
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

# individual years
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2005.csv", file_path_prism = "data/PRISM/prism_2005.csv", output_path = "./data/processed/tempAndED/2019-11-12_tempAndED_2005_allED.csv", bobbCodes = "all")
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2006.csv", file_path_prism = "data/PRISM/prism_2006.csv", output_path = "./data/processed/tempAndED/2019-11-12_tempAndED_2006_allED.csv", bobbCodes = "all")
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2007.csv", file_path_prism = "data/PRISM/prism_2007.csv", output_path = "./data/processed/tempAndED/2019-11-12_tempAndED_2007_allED.csv", bobbCodes = "all")
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2008.csv", file_path_prism = "data/PRISM/prism_2008.csv", output_path = "./data/processed/tempAndED/2019-11-12_tempAndED_2008_allED.csv", bobbCodes = "all")
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2009.csv", file_path_prism = "data/PRISM/prism_2009.csv", output_path = "./data/processed/tempAndED/2019-11-12_tempAndED_2009_allED.csv", bobbCodes = "all")
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2010.csv", file_path_prism = "data/PRISM/prism_2010.csv", output_path = "./data/processed/tempAndED/2019-11-12_tempAndED_2010_allED.csv", bobbCodes = "all")
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2011.csv", file_path_prism = "data/PRISM/prism_2011.csv", output_path = "./data/processed/tempAndED/2019-11-12_tempAndED_2011_allED.csv", bobbCodes = "all")
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2012.csv", file_path_prism = "data/PRISM/prism_2012.csv", output_path = "./data/processed/tempAndED/2019-11-12_tempAndED_2012_allED.csv", bobbCodes = "all")
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2013.csv", file_path_prism = "data/PRISM/prism_2013.csv", output_path = "./data/processed/tempAndED/2019-11-12_tempAndED_2013_allED.csv", bobbCodes = "all")
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2014.csv", file_path_prism = "data/PRISM/prism_2014.csv", output_path = "./data/processed/tempAndED/2019-11-12_tempAndED_2014_allED.csv", bobbCodes = "all")
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2015.csv", file_path_prism = "data/PRISM/prism_2015.csv", output_path = "./data/processed/tempAndED/2019-11-12_tempAndED_2015_allED.csv", bobbCodes = "all")
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2016.csv", file_path_prism = "data/PRISM/prism_2016.csv", output_path = "./data/processed/tempAndED/2019-11-12_tempAndED_2016_allED.csv", bobbCodes = "all")
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2017.csv", file_path_prism = "data/PRISM/prism_2017.csv", output_path = "./data/processed/tempAndED/2019-11-12_tempAndED_2017_allED.csv", bobbCodes = "all")
gc()



# merge the years into a single table and save it as csv
rbindlist(lapply(list.files("./data/processed/tempAndED/", full.names = T, pattern = "allED.csv"), FUN = fread ), fill = TRUE) %>% fwrite("./data/processed/temp_and_ed_05-17_allED.csv")



full.list <- first_stage_DLNM(file_path="./data/processed/temp_and_ed_05-17_allED.csv")
saveRDS(full.list, "./data/processed/first_stage_DLNM_allED.rds")

full.list<-readRDS("data/processed/first_stage_DLNM_71519.rds")


meta.full<-meta_stage_DLNM(first_stage_list =full.list, 
                           output_path_mv_model = "data/meta_model_climate_allED.rds",
                           output_path_num = "/data/processed/attributable_number_climate_allED.csv",
                           output_path_frac = "/data/processed/attributable_frac_climate_allED.csv", 
                           output_path_mintemp = "/data/processed/mintemp_zips_climate_allED.csv", 
                           varfun = "bs", 
                           vardegree = 2, 
                           varper = c(10,75,90), 
                           lag = 3, 
                           lagnk = 2)
saveRDS(meta.full, "./data/processed/meta_stage_DLNM_climate_allED.rds")
meta.full

#meta<-readRDS("./data/processed/meta_stage_DLNM_72419.rds")
#meta<-readRDS("./data/processed/meta_stage_DLNM_climate_allED.rds")

#mv.model.zips<-readRDS("data/meta_model_just_zips_10_27.rds")
#mv.model.climate.16<-readRDS("data/meta_model_climate_16_10_29.rds")




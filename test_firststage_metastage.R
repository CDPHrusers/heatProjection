library(data.table)
library(tidyr)
gc()
# setwd("R:/heatProjections/")
setwd("./")

# or through Rstudio in your browser 
# setwd("//mnt/projects/ohe/heatProjections/")
source("code/heatProjection/heatProjectR/R/filter_ed.R")
source("code/heatProjection/heatProjectR/R/get_prism.R")
source("code/heatProjection/heatProjectR/R/combine_ed_temp.R")
source("code/heatProjection/heatProjectR/R/first_stage_DLNM.R")
source("code/heatProjection/heatProjectR/R/meta_stage_DLNM.R")

test.list<-first_stage_DLNM(file_path="./data/processed/combined_test_data.csv")

full.list<-first_stage_DLNM(file_path="./data/processed/temp_and_ed_05-17.csv")
#saveRDS(full.list, "./data/processed/first_stage_DLNM_71519.rds")

full.list<-readRDS("data/processed/first_stage_DLNM_71519.rds")

#test.list.sub <-test.list[[1]][!is.na(test.list[[1]])]
                          
meta.test<-meta_stage_DLNM(first_stage_list =test.list, output_path_num = "/data/processed/attributable_number_zips.csv",output_path_frac = "/data/processed/attributable_frac_zips.csv",varfun = "bs", vardegree = 2, varper = c(10,75,90), lag = 3, lagnk = 2)

meta.full<-meta_stage_DLNM(first_stage_list =full.list, output_path_num = "/data/processed/attributable_number_zips.csv",output_path_frac = "/data/processed/attributable_frac_zips.csv",varfun = "bs", vardegree = 2, varper = c(10,75,90), lag = 3, lagnk = 2)

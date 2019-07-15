library(data.table)
library(tidyr)
gc()
# setwd("R:/heatProjections/")
setwd("//mnt/projects/ohe/heatProjections/")

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
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2005.csv", file_path_prism = "data/PRISM/prism_2005.csv", output_path = "./data/processed/tempAndED/2019-06-11_tempAndED_2005.csv")
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2006.csv", file_path_prism = "data/PRISM/prism_2006.csv", output_path = "./data/processed/tempAndED/2019-06-11_tempAndED_2006.csv")
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2007.csv", file_path_prism = "data/PRISM/prism_2007.csv", output_path = "./data/processed/tempAndED/2019-06-11_tempAndED_2007.csv")
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2008.csv", file_path_prism = "data/PRISM/prism_2008.csv", output_path = "./data/processed/tempAndED/2019-06-11_tempAndED_2008.csv")
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2009.csv", file_path_prism = "data/PRISM/prism_2009.csv", output_path = "./data/processed/tempAndED/2019-06-11_tempAndED_2009.csv")
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2010.csv", file_path_prism = "data/PRISM/prism_2010.csv", output_path = "./data/processed/tempAndED/2019-06-11_tempAndED_2010.csv")
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2011.csv", file_path_prism = "data/PRISM/prism_2011.csv", output_path = "./data/processed/tempAndED/2019-06-11_tempAndED_2011.csv")
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2012.csv", file_path_prism = "data/PRISM/prism_2012.csv", output_path = "./data/processed/tempAndED/2019-06-11_tempAndED_2012.csv")
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2013.csv", file_path_prism = "data/PRISM/prism_2013.csv", output_path = "./data/processed/tempAndED/2019-06-11_tempAndED_2013.csv")
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2014.csv", file_path_prism = "data/PRISM/prism_2014.csv", output_path = "./data/processed/tempAndED/2019-06-11_tempAndED_2014.csv")
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2015.csv", file_path_prism = "data/PRISM/prism_2015.csv", output_path = "./data/processed/tempAndED/2019-06-11_tempAndED_2015.csv")
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2016.csv", file_path_prism = "data/PRISM/prism_2016.csv", output_path = "./data/processed/tempAndED/2019-06-11_tempAndED_2016.csv")
gc()
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2017.csv", file_path_prism = "data/PRISM/prism_2017.csv", output_path = "./data/processed/tempAndED/2019-06-11_tempAndED_2017.csv")
gc()

# merge the years into a single table 
rbindlist(lapply(list.files("./data/processed/tempAndED/", full.names = T), FUN = fread ), fill = TRUE) %>% fwrite("./data/processed/temp_and_ed_05-17.csv")

# make test data to run DLNM and MVMETA
fread("./data/processed/temp_and_ed_05-17.csv")[ZCTA %in% c(90017,92102,92113,93301,91046,91106,91206,90716,90670,90704,90630,90039,90032,90021,90017,90012,90003, 90010,90073, 93625, 93653, 91945, 91948, 92083, 92550, 92253, 92322, 92389, 92401, 93546, 93551, 93921)] %>% fwrite("./data/processed/combined_test_data.csv")



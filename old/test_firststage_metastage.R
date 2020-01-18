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

# make test data to run DLNM and MVMETA
#fread("./data/processed/temp_and_ed_05-17.csv")[ZCTA %in% c(90017,92102,92113,93301,91046,91106,91206,90716,90670,90704,90630,90039,90032,90021,90017,90012,90003, 90010,90073, 93625, 93653, 91945, 91948, 92083, 92550, 92253, 92322, 92389, 92401, 93546, 93551, 93921,89010, 89019, 89060, 89061, 89439, 97635)] %>% fwrite("./data/processed/combined_test_data.csv")

test.data<-fread("./data/processed/combined_test_data.csv")
test.list<-first_stage_DLNM(file_path="./data/processed/combined_test_data.csv")
##90716 is derivs error (index 13) ##29 length

full.list<-first_stage_DLNM(file_path="./data/processed/temp_and_ed_05-17.csv")
#saveRDS(full.list, "./data/processed/first_stage_DLNM_71519.rds")

# dlist<-unlist(full.list$dlist)
# 
# for (i in 1:length(dlist2)){
#   dlist2[[i]]<-as.matrix(dlist2[[i]])
# }
# 
# dlist.mat<-lapply(dlist2, as.matrix)
# 
# dlist2<-full.list$dlist
# full.data<-do.call(rbind, dlist2)
#test.list.sub <-test.list[[1]][!is.na(test.list[[1]])]
                          
meta.test <-
  meta_stage_DLNM(
    first_stage_list = test.list,
    output_path_num = "/data/processed/test_attributable_number_zips.csv",
    output_path_frac = "/data/processed/test_attributable_frac_zips.csv",
    output_path_mintemp = "/data/processed/test_mintemp_zips.csv",
    varfun = "bs",
    vardegree = 2,
    varper = c(10, 75, 90),
    lag = 3,
    lagnk = 2
  )

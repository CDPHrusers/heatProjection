library(data.table)
library(tidyr)

setwd("R:/heatProjections/")

# or through Rstudio in your browser 
# setwd("//mnt/projects/ohe/heatProjections/")
source("code/heatProjection/heatProjectR/R/filter_ed.R")
source("code/heatProjection/heatProjectR/R/get_prism.R")
source("code/heatProjection/heatProjectR/R/combine_ed_temp.R")

combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2005.csv", file_path_prism = "data/PRISM/prism_2005.csv", output_path = "./data/processed/tempAndED/2019-04-26_tempAndED_2005.csv")
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2006.csv", file_path_prism = "data/PRISM/prism_2006.csv", output_path = "./data/processed/tempAndED/2019-04-26_tempAndED_2006.csv")
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2007.csv", file_path_prism = "data/PRISM/prism_2007.csv", output_path = "./data/processed/tempAndED/2019-04-26_tempAndED_2007.csv")
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2008.csv", file_path_prism = "data/PRISM/prism_2008.csv", output_path = "./data/processed/tempAndED/2019-04-26_tempAndED_2008.csv")
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2009.csv", file_path_prism = "data/PRISM/prism_2009.csv", output_path = "./data/processed/tempAndED/2019-04-26_tempAndED_2009.csv")
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2010.csv", file_path_prism = "data/PRISM/prism_2010.csv", output_path = "./data/processed/tempAndED/2019-04-26_tempAndED_2010.csv")
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2011.csv", file_path_prism = "data/PRISM/prism_2011.csv", output_path = "./data/processed/tempAndED/2019-04-26_tempAndED_2011.csv")
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2012.csv", file_path_prism = "data/PRISM/prism_2012.csv", output_path = "./data/processed/tempAndED/2019-04-26_tempAndED_2012.csv")
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2013.csv", file_path_prism = "data/PRISM/prism_2013.csv", output_path = "./data/processed/tempAndED/2019-04-26_tempAndED_2013.csv")
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2014.csv", file_path_prism = "data/PRISM/prism_2014.csv", output_path = "./data/processed/tempAndED/2019-04-26_tempAndED_2014.csv")
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2015.csv", file_path_prism = "data/PRISM/prism_2015.csv", output_path = "./data/processed/tempAndED/2019-04-26_tempAndED_2015.csv")
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2016.csv", file_path_prism = "data/PRISM/prism_2016.csv", output_path = "./data/processed/tempAndED/2019-04-26_tempAndED_2016.csv")
combine_ed_temp(file_path_ed = "./data/ED/cdph_ed_rln2017.csv", file_path_prism = "data/PRISM/prism_2017.csv", output_path = "./data/processed/tempAndED/2019-04-26_tempAndED_2017.csv")


# check it out the new and old 2005 files of combined temp and ed visits are identical
new <- fread("./data/processed/tempAndED/2019-04-26_tempAndED_2005.csv")
old <- fread("./data/processed/tempAndED/tempAndED_2005.csv")

new == old

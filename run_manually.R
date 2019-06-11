gc()

library(data.table)
library(tidyverse)
 
# setwd("R:/heatProjections/")
setwd("//mnt/projects/ohe/heatProjections/")

bobbCodes = c(55L, 157L, 159L, 244L, 108L, 2L)
file_path <- "./data/ED/cdph_ed_rln2015.csv"
columns_to_keep = c(
  # "dx_prin",
  # "ec_prin",
  "ccs_dx_prin", # CCS of principal diagnosis
  "ccs_odx1",    # CCS of first other diagnosis
  "ccs_odx2",    # CCS of second other diagnosis
  # "fac_id",
  # "pat_type",
  # "lic_type",
  "agdyserv",
  "agyrserv", # time of encounter
  "sex",
  "patzip",
  "patco",
  # "serv_q",
  # "serv_d",
  # "serv_m",
  # "serv_y",
  # "dispn",
  # "payer",
  "pr_prin",
  # "opr1",
  "serv_dt",
  "brthdate",
  # "dob_raw",
  "faczip",
  "fac_co",
  "rln",
  "race_grp"
)


foo <- fread(paste(file_path), select = columns_to_keep,  key = c("ccs_dx_prin", "ccs_odx1", "ccs_odx2"))[patco != 0]


# setkey(foo, ccs_dx_prin, ccs_odx1, ccs_odx2)
##pull out all cases that have the designated Bobb codes in either the primary, secondary or tertiary diagnosis columns
##bring in patient's zipcode, county code, and date of service
foo <-
  foo[ccs_dx_prin %in% bobbCodes |
        ccs_odx1 %in% bobbCodes |
        ccs_odx2 %in% bobbCodes, .(n =.N,
                                   Date = as.Date(serv_dt, format = "%m/%d/%Y")) , by=.(patzip, serv_dt)]

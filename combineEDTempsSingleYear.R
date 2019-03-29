rm(list = ls())
setwd('/mnt/projects/ohe/')
library(data.table)
library(tidyverse)
library(parallel)

bobbCodes <- c(55L, 157L, 159L, 244L, 108L, 2L)

files <- list.files("/mnt/projects/ohe/data/ED",full.names = T)

tDir <- tempdir()

filterED <- function(year) {
  file<- paste0("/mnt/projects/ohe/data/ED/cdph_ed_rln",year,".csv")
  foo <- fread(paste(file))
  
  setkey(foo, ccs_dx_prin, ccs_odx1, ccs_odx2)
  out <-
    foo [ccs_dx_prin %in% bobbCodes |
           ccs_odx1 %in% bobbCodes |
           ccs_odx2 %in% bobbCodes, c(
             "dx_prin",
             "ec_prin",
             "ccs_dx_prin",
             "ccs_odx1",
             "ccs_odx2" ,
             "fac_id",
             "pat_type",
             "lic_type",
             "agdyserv",
             "agyrserv",
             "sex",
             "patzip",
             "patco",
             "serv_q",
             "serv_d",
             "serv_m",
             "serv_y",
             "dispn",
             "payer",
             "pr_prin",
             "opr1",
             "serv_dt",
             "brthdate",
             "dob_raw",
             "faczip",
             "fac_co",
             "rln",
             "race_grp"
           )]
  
 return(out)
  
}


getPRISM <- function(year){
  
  file<- paste0("/mnt/projects/ohe/data/PRISM/prism_",year,".csv")
  foo <- fread(paste(file)) %>% .[, `:=` (DateTXT = as.character(substr(`system:index`, 1,8)),
                                          Date = as.Date(substr(`system:index`, 1,8), format = "%Y%m%d"))]
  
  
  return(foo)
  
}

tryED <- filterED(2008) 
try <- getPRISM(2017)



















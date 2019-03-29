rm(list = ls())
setwd('/mnt/projects/ohe/')
library(data.table)
library(tidyverse)
library(parallel)

bobbCodes <- c(55L, 157L, 159L, 244L, 108L, 2L)


files <- list.files("/mnt/projects/ohe/data/ED",full.names = T)

tDir <- tempdir()


grabBobbCodes <- function(file) {
  # file<-"/mnt/projects/ohe/data/ED/cdph_ed_rln2006.csv"
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
  
  # ccs<-out[,ccs_dx_prin]
  # table(ccs)
  fwrite(out, tempfile(
    pattern = "ED",
    tmpdir = tDir,
    fileext = ".csv"
  ))
}

# test function

# grabBobbCodes("/mnt/projects/ohe/data/ED/cdph_ed_rln2006.csv")
# grabBobbCodes(files[1])

lapply(files, grabBobbCodes)

EDvisits <-   rbindlist(lapply(list.files(tDir, full.names = T), fread),fill =F ) 

fwrite(EDvisits, "/mnt/projects/ohe/data/processed/ED_6CCScodes.csv")

# to see files and remove individually
list.files(tDir)
lapply(list.files(tDir, full.names = T), file.remove)


code.check<-EDvisits[dx_prin=="5990",.(dx_prin, ccs_dx_prin)]
summary(code.check)
table(code.check)




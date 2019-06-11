#' filter_ed
#'
#' This function allows you to filter a single year of OSHPD Emergency Department Visits for the Clinical Classification System Codes of interest.
#' @param BobbCodes A collection of text CCS codes to pull out.
#' @param file_path The file path to the ED data you wish to process.  Defaults to test data on the R server at CDPH
#' @param numDiagnosis If 1, only the primary diagnosis will be used. Values of 2 or 3 correspond to including secondary and tertiatry diagnoses. Defaults to 3.
#' @keywords EDvisits
#' @export
#' @examples
#' filter_ed()

filter_ed <-
  function(bobbCodes = c(55L, 157L, 159L, 244L, 108L, 2L),
           file_path = "//mnt/projects/ohe/heatProjections/data/processed/ed_test_data.csv" ,
           numDiagnosis = 3) {

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
    
    # read in Data table of ED for a single year
    foo <- fread(paste(file_path), select = columns_to_keep,  key = c("ccs_dx_prin", "ccs_odx1", "ccs_odx2"))[patco != 0]
    # setkey(foo, patzip, patco)
    
    # get rid of out of state and missing zip code records
    
    if (numDiagnosis == 1) {
      ##pull out all cases that have the designated Bobb codes in either the primary, secondary or tertiary diagnosis columns
      ##bring in patient's zipcode, county code, and date of service
      foo <- foo[ccs_dx_prin %in% bobbCodes,  .(n =.N,
                                         Date = as.Date(serv_dt, format = "%m/%d/%Y")) , by=.(patzip, serv_dt)]
    } else if (numDiagnosis == 2) {
      ##set key to primary, secondary, and tertiary ccs diagnosis columns
      ##pull out all cases that have the designated Bobb codes in either the primary, secondary or tertiary diagnosis columns
      ##bring in patient's zipcode, county code, and date of service
      foo <-
        foo[ccs_dx_prin %in% bobbCodes |
              ccs_odx1 %in% bobbCodes, .(n =.N,
                                         Date = as.Date(serv_dt, format = "%m/%d/%Y")) , by=.(patzip, serv_dt)]
      
    } else if (numDiagnosis == 3) {
      ##set key to primary, secondary, and tertiary ccs diagnosis column
      ##pull out all cases that have the designated Bobb codes in either the primary, secondary or tertiary diagnosis columns
      ##bring in patient's zipcode, county code, and date of service
      foo <-
        foo[ccs_dx_prin %in% bobbCodes |
              ccs_odx1 %in% bobbCodes |
              ccs_odx2 %in% bobbCodes, .(n =.N,
                                         Date = as.Date(serv_dt, format = "%m/%d/%Y")) , by=.(patzip, serv_dt)]
      
    }
    else
      echo("The number of diagnoses did not match the function criteria")
    return(foo)
  }

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
           file_path = "R:/heatProjections/data/processed/ed_test_data.csv",
           numDiagnosis = 3) {

    columns_to_keep = c(
      "dx_prin",
      "ec_prin",
      "ccs_dx_prin",
      "fac_id",
      "pat_type",
      "lic_type",
      "agdyserv",
      "agyrserv",
      "sex",
      "patzip",
      "patco",
      # "serv_q",
      # "serv_d",
      # "serv_m",
      # "serv_y",
      "dispn",
      "payer",
      "pr_prin",
      "opr1",
      "serv_dt",
      "brthdate",
      # "dob_raw",
      "faczip",
      "fac_co",
      "rln",
      "race_grp"
    )
    
    # read in Data table of ED for a single year
    foo <- fread(paste(file_path))
    setkey(foo, patzip, patco)
    
    # get rid of out of state and missing zip code records
    foo <- foo[patco != 0]

    
    if (numDiagnosis == 1) {
      setkey(foo, ccs_dx_prin)
      ##pull out all cases that have the designated Bobb codes in either the primary, secondary or tertiary diagnosis columns
      ##bring in patient's zipcode, county code, and date of service
      out <-
        foo[ccs_dx_prin %in% bobbCodes , columns_to_keep, with=FALSE]  %>%
        ##count the number of cases by patient zip code and by date
         .[,n :=.n, by=.(patzip, serv_dt)]%>%
        .[, Date := as.Date(serv_dt, format = "%m/%d/%Y")]
      
    } else if (numDiagnosis == 2) {
      ##set key to primary, secondary, and tertiary ccs diagnosis columns
      setkey(foo, ccs_dx_prin, ccs_odx1)
      ##pull out all cases that have the designated Bobb codes in either the primary, secondary or tertiary diagnosis columns
      ##bring in patient's zipcode, county code, and date of service
      out <-
        foo[ccs_dx_prin %in% bobbCodes |
              ccs_odx1 %in% bobbCodes, columns_to_keep, with=FALSE]  %>%
        ##count the number of cases by patient zip code and by date
         .[,n :=.n, by=.(patzip, serv_dt)]%>%
        .[, Date := as.Date(serv_dt, format = "%m/%d/%Y")]
      
    } else if (numDiagnosis == 3) {
      ##set key to primary, secondary, and tertiary ccs diagnosis columns
      setkey(foo, ccs_dx_prin, ccs_odx1, ccs_odx2)
      ##pull out all cases that have the designated Bobb codes in either the primary, secondary or tertiary diagnosis columns
      ##bring in patient's zipcode, county code, and date of service
      out <-
        foo[ccs_dx_prin %in% bobbCodes |
              ccs_odx1 %in% bobbCodes |
              ccs_odx2 %in% bobbCodes, columns_to_keep, with=FALSE]  %>%
        ##count the number of cases by patient zip code and by date
         .[,n :=.n, by=.(patzip, serv_dt)]%>%
        .[, Date := as.Date(serv_dt, format = "%m/%d/%Y")]
      
    }
    else
      echo("The number of diagnoses did not match the function criteria")
    return(out)
  }

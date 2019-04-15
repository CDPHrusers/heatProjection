#' Filter ED
#'
#' This function allows you to filter a single year of OSHPD Emergency Department Visits for the Clinical Classification System Codes of interest.
#' @param BobbCodes This is a collection of text CCS codes to pull out. 
#' @param year Four digit year as it appears in OSPHD filnes 'cdph_ed_rlnYYYY.csv'. Defauls to 2005
#' @param numDiagnosis If 1, only the primary diagnosis will be used. Values of 2 or 3 correspond to including secondary and tertiatry diagnoses. Defaults to 3.
#' @keywords EDvisits
#' @export
#' @examples
#' filterED()

filterED <- function(bobbCodes = c(55L, 157L, 159L, 244L, 108L, 2L), year = 2005, numDiagnosis = 3) {
  file<- paste0("ED/cdph_ed_rln",year,".csv")
  foo <- fread(paste(file))
  
  if (numDiagnosis==1){
    setkey(foo, ccs_dx_prin)
    ##pull out all cases that have the designated Bobb codes in either the primary, secondary or tertiary diagnosis columns
    ##bring in patient's zipcode, county code, and date of service
    out <-
      foo [ccs_dx_prin %in% bobbCodes , c(
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
      )]  %>%
    ##count the number of cases by patient zip code and by date
    # .[,n :=.n, by=.(patzip, serv_dt)]%>%
     .[, Date:= as.Date(serv_dt, format = "%m/%d/%Y")]
    
    return(out)
    
  } else if (numDiagnosis==2) {
    ##set key to primary, secondary, and tertiary ccs diagnosis columns
    setkey(foo, ccs_dx_prin, ccs_odx1)
    ##pull out all cases that have the designated Bobb codes in either the primary, secondary or tertiary diagnosis columns
    ##bring in patient's zipcode, county code, and date of service
    out <-
      foo [ccs_dx_prin %in% bobbCodes |
             ccs_odx1 %in% bobbCodes, c(
               "dx_prin",
               "ec_prin",
               "ccs_dx_prin",
               "ccs_odx1",
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
             )]  %>%
    ##count the number of cases by patient zip code and by date
    # .[,n :=.n, by=.(patzip, serv_dt)]%>%
     .[, Date:= as.Date(serv_dt, format = "%m/%d/%Y")]
    
    return(out)
    
  } else if (numDiagnosis==3) {
    ##set key to primary, secondary, and tertiary ccs diagnosis columns
    setkey(foo, ccs_dx_prin, ccs_odx1, ccs_odx2)
    ##pull out all cases that have the designated Bobb codes in either the primary, secondary or tertiary diagnosis columns
    ##bring in patient's zipcode, county code, and date of service
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
             )]  %>%
    ##count the number of cases by patient zip code and by date
    # .[,n :=.n, by=.(patzip, serv_dt)]%>%
     .[, Date:= as.Date(serv_dt, format = "%m/%d/%Y")]
    
    return(out)
    
  }
  else 
    print("The number of diagnoses did not match the function criteria")
  
}
